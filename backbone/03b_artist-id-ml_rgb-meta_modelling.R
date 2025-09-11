# Artist Identification: Model Fitting, Tuning, and Evaluation
# Using RGB & Metadata


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, tidymodels, future)

source(here("fns_objs", "00_fn-backbone.R"))

fp_train_feat <- here("data", paste0("03_train-feat_",
                                     Sys.Date(),
                                     ".rds"))

fp_app_feat <- here("data", paste0("03_app-feat_",
                                   Sys.Date(),
                                   ".rds"))

df_train_final <- readRDS(fp_train_feat)
df_app_final <- readRDS(fp_app_feat)



# Build, Fit, & Assess Models=======================================================================
## Create folds--------------------
#note: manually done because of small sample sizes for some artists
set.seed(1)
folds_cv <- vfold_cv(df_train_final, v = 3, strata = artist_clean)
#warning: unstratified resampling


check_n_fold <- function(cv_folds, type="analysis") {
  if(!type %in% c("analysis", "assess")) {
    stop("Error: Argument 'type' must be 'analysis' or 'assess'")
  }
  
  df <- cv_folds %>%
    {if(type=="analysis") mutate(., data=map(splits, analysis)) else .} %>%
    {if(type=="assess") mutate(., data=map(splits, assessment)) else .} %>%
    unnest(data) %>%
    select(!splits) %>% 
    group_by(artist_clean) %>% 
    summarize(n_folds=n_distinct(id)) %>%
    summarize(n_artists=n(),
              min_folds=min(n_folds)) %>%
    mutate(set=type, .before="n_artists")
  
  return(df)
}

check_analysis_assess <- function(df, n_folds) {
  folds_cv <- suppressWarnings(vfold_cv(df, v=n_folds, strata=artist_clean))
  
  df_analysis <- check_n_fold(folds_cv, type="analysis") %>%
    mutate(meet_thresh=min_folds==n_folds)
  
  df_assess <- check_n_fold(folds_cv, type="assess") %>%
    mutate(meet_thresh=min_folds==n_folds-1)
  
  df <- bind_rows(df_analysis, df_assess)
  
  hit_thresh <- sum(df$meet_thresh)==2
  
  return(list(hit_thresh=hit_thresh,
              summary=df))
}


iterate_fold_check <- function(df, n_folds, seed_min, seed_max) {
  for(i in seed_min: seed_max) {
    set.seed(i)
    list_result <- check_analysis_assess(df, n_folds)
    
    if(list_result$hit_thresh){
      return(append(list(seed=i), list_result))
    }
    print(i)
  }
  # return(list_result)
}

set.seed(1)
check_n_fold(folds_cv, "analysis")
check_n_fold(folds_cv, "assess")
check_analysis_assess(df=df_train_final, n_folds=3)
# iterate_fold_check(df_train_final, 3, 10000, 15000)




#check artists per fold
n_folds <- 3
folds_cv %>%
  mutate(data=map(splits, analysis)) %>%
  unnest(data) %>%
  select(!splits) %>% 
  group_by(artist_clean) %>% 
  summarize(n_folds=n_distinct(id)) %>%
  summarize(n_artists=n(),
            min_folds=min(n_folds)) %>%
  mutate(set="analysis", .before="n_artists") %>%
  mutate(meet_thresh=min_folds==n_folds)

folds_cv %>%
  mutate(data=map(splits, assessment)) %>%
  unnest(data) %>%
  select(!splits) %>% 
  group_by(artist_clean) %>% 
  summarize(n_folds=n_distinct(id)) %>%
  summarize(n_artists=n(),
            min_folds=min(n_folds))





## Create recipe--------------------
rec <- recipe(artist_clean ~ ., data=df_train_final) %>%
  update_role(object_id, new_role="id") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_mutate(across(where(is.logical), as.integer))



## Random forest--------------------
### Create model
model_rf <- rand_forest(
  mtry = tune(),      # number of predictors randomly sampled at each split
  trees = 500,        # increase trees for better stability
  min_n = tune()      # minimum node size
) %>%
  set_engine("ranger", verbose = FALSE) %>%
  set_mode("classification")


### Create workflow
workflow_rf <- workflow() %>%
  add_model(model_rf) %>%
  add_recipe(rec)


### Define parameter grid
params_rf <- parameters(
  mtry(range = c(1, 50)),  # allow mtry up to 50, safer than 250 to avoid heavy memory use
  min_n(range = c(2, 10))  # reasonable range for min_n
)

grid_rf <- grid_random(params_rf, size=15)


### Tune
tune_results_rf <- tune_grid(
  workflow_rf,
  resamples = folds_cv,
  grid = grid_rf,
  metrics = metric_set(accuracy, kap, roc_auc),
  control = control_grid(
    save_pred = TRUE,
    verbose = TRUE
  )
)



# ### Select the best parameters
show_best(tune_results_rf, metric="accuracy", n=5) #46.6 %
params_rf_best <- select_best(tune_results_rf, metric="accuracy")


### Assess results
collect_predictions(tune_results_rf) %>%
  conf_mat(truth = artist_clean, estimate = .pred_class) %>%
  autoplot(type = "heatmap")


### Finalize workflow
final_rf <- finalize_workflow(workflow_rf, params_rf_best)


### Fit final model (on all training data)
# final_fit_rf <- fit(final_rf, data=df_train_unprepped)


## XGBoost--------------------
### Create model
model_xgb <- boost_tree(
  trees = 500,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),    # gamma in xgboost
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")


### Create workflow
workflow_xgb <- workflow() %>%
  add_model(model_xgb) %>%
  add_recipe(rec)


### Define parameter grid
params_xgb <- parameters(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_prop(range = c(0.5, 1.0)),  # sample_size must be a proportion, e.g., between 0.5 and 1
  finalize(mtry(), df_train_final),
  learn_rate()
)

grid_xgb <- grid_random(params_xgb, size = 20)


### Tune
set.seed(202)
tune_results_xgb <- tune_grid(
  workflow_xgb,
  resamples=folds_cv,
  grid=grid_xgb,
  metrics=metric_set(accuracy, kap, roc_auc),
  control=control_grid(save_pred=TRUE)
)


### Select the best parameters
show_best(tune_results_xgb, metric="accuracy", n=5) 
params_xgb_best <- select_best(tune_results_xgb, metric="accuracy")


### Finalize workflow
final_xgb <- finalize_workflow(workflow_xgb, params_xgb_best)


### Fit final model (on all training data)
final_fit_xgb <- fit(final_xgb, data=df_train_unprepped) 









# Archive
make_stratified_cv <- function(data, label_col, v = 2, max_attempts = 1000, verbose = TRUE) {
  label_col <- rlang::ensym(label_col)
  label_name <- rlang::as_name(label_col)
  all_labels <- unique(data[[label_name]])
  
  for (seed in 1:max_attempts) {
    set.seed(seed)
    
    # Assign fold numbers per artist
    df_folds <- data %>%
      group_by(!!label_col) %>%
      mutate(fold = sample(rep(1:v, length.out = n()))) %>%
      ungroup()
    
    # Check: each fold's *assessment set* must include all artist labels
    fold_valid <- TRUE
    for (k in 1:v) {
      fold_artists <- df_folds %>%
        filter(fold == k) %>%
        pull(!!label_col) %>%
        unique()
      
      if (length(setdiff(all_labels, fold_artists)) > 0) {
        fold_valid <- FALSE
        break
      }
    }
    
    if (fold_valid) {
      if (verbose) message("Found seed: ", seed)
      
      # Build rsplit objects
      splits <- vector("list", length = v)
      for (k in 1:v) {
        idx_analysis <- which(df_folds$fold != k)
        idx_assess <- which(df_folds$fold == k)
        
        splits[[k]] <- rsample::make_splits(
          list(analysis = idx_analysis, assessment = idx_assess),
          data = df_folds
        )
      }
      
      # Wrap into vfold_cv-like object
      cv_obj <- tibble::tibble(
        splits = splits,
        id = paste0("Fold", seq_along(splits))
      )
      
      attr(cv_obj, "class") <- c("vfold_cv", "rset")
      attr(cv_obj, "v") <- v
      attr(cv_obj, "repeats") <- 1
      attr(cv_obj, "rset_info") <- tibble::tibble(
        v = v,
        repeats = 1,
        strata = label_name,
        id = NA_character_
      )
      
      # Return both folds and stratified data
      return(list(
        folds = cv_obj,
        data = df_folds %>% select(-fold)
      ))
    }
  }
  
  stop("No valid stratified seed found in ", max_attempts, " attempts.")
}



cv_out <- make_stratified_cv(df_train_final, label_col = artist_clean, v=4)

cv_folds <- cv_out$folds
df_stratified <- cv_out$data










