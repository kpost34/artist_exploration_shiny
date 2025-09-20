# Artist Identification: Model Fitting, Tuning, and Evaluation
# Using RGB stats and bins


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, tidymodels)

source(here("fns_objs", "00_fn-backbone.R"))

fp_train_feat <- grab_newest_fp(dir=here("data"),
                               patt="^01_train-feat_")

fp_test_feat <- grab_newest_fp(dir=here("data"),
                               patt="^01_test-feat")



df_train_unprepped <- readRDS(fp_train_feat) %>%
  select(object_id, artist_clean, starts_with(c("R_", "G_", "B_")))

df_test_unprepped <- readRDS(fp_test_feat) %>%
  select(object_id, artist_clean, starts_with(c("R_", "G_", "B_")))



# Build, Fit, & Assess Models======================================================================
## Create folds--------------------
set.seed(6) #splits across folds if prop=0.8 (10+ artworks)
folds_cv <- vfold_cv(df_train_unprepped, v = 3, strata = artist_clean)
#warning: unstratified resampling

#check artists per fold
folds_cv %>%
  mutate(data=map(splits, analysis)) %>%
  unnest(data) %>%
  select(!splits) %>% 
  group_by(artist_clean) %>% 
  summarize(n_folds=n_distinct(id)) %>%
  summarize(n_artists=n(),
            min_folds=min(n_folds))
#35 artists in 3 analysis folds

folds_cv %>%
  mutate(data=map(splits, assessment)) %>%
  unnest(data) %>%
  select(!splits) %>% 
  group_by(artist_clean) %>% 
  summarize(n_folds=n_distinct(id)) %>%
  summarize(n_artists=n(),
            min_folds=min(n_folds))
#35 artists in 3 assessment folds


## Create recipe--------------------
rec <- recipe(artist_clean ~ ., data=df_train_unprepped) %>%
  update_role(object_id, new_role="id") %>%
  step_rm(has_role("ID")) %>%
  step_normalize(all_predictors())


## Random forest--------------------
### Create model
model_rf <- rand_forest(
  mtry = tune(), 
  trees = 1000,
  min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("classification")


### Create workflow
workflow_rf <- workflow() %>%
  add_model(model_rf) %>%
  add_recipe(rec)


### Define parameter grid
params_rf <- parameters(
  mtry(range=c(5, 15)),
  min_n(range=c(2, 10))
)

grid_rf <- grid_random(params_rf, size=20)


### Tune
set.seed(202)
tune_results_rf <- tune_grid(
  workflow_rf,
  resamples=folds_cv,
  grid=grid_rf,
  metrics=metric_set(accuracy, kap),
  control=control_grid(
    save_pred=TRUE, 
    verbose=TRUE
  )
)


### Select the best parameters
show_best(tune_results_rf, metric="accuracy", n=5) #~24-26%
params_rf_best <- select_best(tune_results_rf, metric="accuracy")


## XGBoost--------------------
### Create model
model_xgb <- boost_tree(
  trees = 500, 
  tree_depth = tune(), 
  min_n = tune(),
  loss_reduction = tune(),
  sample_size=tune(),
  mtry = 1.0,
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
  tree_depth(range=c(4, 10)),
  min_n(range=c(2, 10)),
  loss_reduction(range=c(0, 1)),
  sample_prop(range=c(0.7, 0.9)),
  learn_rate(range=c(0.05, 0.5))
)

grid_xgb <- grid_random(params_xgb, size=20)


### Tune
set.seed(202)
tune_results_xgb <- tune_grid(
  workflow_xgb,
  resamples=folds_cv,
  grid=grid_xgb,
  metrics=metric_set(accuracy, kap),
  control=control_grid(
    save_pred=TRUE,
    verbose=TRUE
  )
)


### Select the best parameters
show_best(tune_results_xgb, metric="accuracy", n=5) #~17-18%
params_xgb_best <- select_best(tune_results_xgb, metric="accuracy")



## Multinomial Logistic Regression-------------------
### Create model
model_mlog <- multinom_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")


### Create workflow
workflow_mlog <- workflow() %>%
  add_model(model_mlog) %>%
  add_recipe(rec)


### Define parameter grid
params_mlog <- parameters(
  penalty(range=c(-4, 0), trans=log10_trans()),
  mixture()
)

grid_mlog <- grid_regular(params_mlog, levels=5)


### Tune
set.seed(202)
tune_results_mlog <- tune_grid(
  workflow_mlog,
  resamples=folds_cv,
  grid=grid_mlog,
  metrics=metric_set(accuracy, kap),
  control=control_grid(
    save_pred=TRUE,
    verbose=TRUE
  )
)


### Select the best parameters
show_best(tune_results_mlog, metric="accuracy", n=5) #~22 %
params_mlog_best <- select_best(tune_results_mlog, metric="accuracy")



## Evaluate on test set--------------------
### Finalize best worflow & model (RF)
#### Finalize workflow
final_rf <- finalize_workflow(workflow_rf, params_rf_best)


#### Fit final model (on all training data)
final_fit_rf <- fit(final_rf, data=df_train_unprepped) 


### Apply best model to test set
df_pred_rf <- predict(object=final_fit_rf,
                      new_data=df_test_unprepped,
                      type="class")


### Assess accuracy of predictions
df_test_pred <- df_test_unprepped %>%
  select(object_id, obs="artist_clean") %>%
  bind_cols(df_pred_rf %>% rename(pred=".pred_class")) %>% 
  # accuracy(obs, pred) %>%
  mutate(match=obs==pred)
#24.8%


### Get class probabilities
df_pred_probs_rf <- predict(final_fit_rf, 
                            new_data=df_test_unprepped,
                            type="prob")



# How to get top x preds for the whole test set
## Pull in object_ids & true label
df_pred_probs_obs <- df_test_unprepped %>%
  select(object_id, obs="artist_clean") %>%
  bind_cols(df_pred_probs_rf)

get_top_k_preds <- function(df, k = 3) {
  # Assume df columns: 1=object_id, 2=obs, 3+: predicted probabilities
  
  object_ids <- df[[1]]
  obs <- df[[2]]
  prob_df <- df[, -(1:2)]
  
  # Ensure probabilities are numeric
  prob_df <- as.data.frame(lapply(prob_df, as.numeric))
  
  # For each row, get top-k predictions in long format
  top_k_long <- purrr::map_dfr(seq_len(nrow(prob_df)), function(i) {
    prob_vector <- as.numeric(prob_df[i, ])
    names(prob_vector) <- colnames(prob_df)
    top_k <- sort(prob_vector, decreasing = TRUE)[1:k]
    
    tibble(
      object_id = object_ids[i],
      obs = obs[i],
      rank = seq_len(k),
      artist = names(top_k) %>% 
        stringr::str_remove("^\\.pred_") %>%
        stringr::str_replace_all("\\.", " "),
      prob = as.numeric(top_k)
    )
  })
  
  # Pivot to wide format
  top_k_wide <- tidyr::pivot_wider(
    top_k_long,
    id_cols = c(object_id, obs),
    names_from = rank,
    values_from = c(artist, prob),
    names_glue = "{.value}_top{rank}"
  )
  
  # Calculate match indicators
  top_k_wide <- top_k_wide %>%
    rowwise() %>%
    mutate(
      # Logical: does top1 artist match obs?
      top_match = (artist_top1 == obs),
      
      # Logical: does any artist in top-k match obs?
      any_match = any(c_across(starts_with("artist_top")) == obs),
      
      # Integer: which rank matches obs? NA if none
      rank_match = {
        matches <- which(c_across(starts_with("artist_top")) == obs)
        if (length(matches) == 0) NA_integer_ else matches[1]
      }
    ) %>%
    ungroup() %>%
    # Reorder columns: object_id, obs, top_match, any_match, rank_match, then rest
    select(object_id, obs, top_match, any_match, rank_match, everything())
  
  return(top_k_wide)
}


df_top_preds <- get_top_k_preds(df_pred_probs_obs, k=3)
df_top_preds %>%
  filter(row_number()==1) %>%
  select(contains("_top")) %>%
  pivot_longer(cols=everything(), names_to=c(".value", "rank"), 
               names_sep="_") %>%
  rename(probability="prob") %>%
  select(!rank)




# Predict on new image
## Load image, process image to RGBs, calculate stats, remove zvs and high corrs
## Run prediction
# Predict all class probabilities
single_artwork <- df_test_unprepped %>%
  select(!c(object_id, artist_clean)) %>%
  filter(row_number()==1) %>%
  mutate(object_id=0)

preds <- predict(final_fit_rf, single_artwork, type = "prob")


# Get top k predictions
## Return probs on single artwork
get_top_k_preds_for_artwork <- function(model, new_artwork_df, k = 3) {
  # Predict probabilities for the single artwork
  prob_df <- predict(model, new_data = new_artwork_df, type = "prob")
  
  # Convert to named numeric vector
  prob_vector <- as.numeric(prob_df[1, ])
  names(prob_vector) <- colnames(prob_df)
  
  # Get top-k predictions as tibble
  top_k <- sort(prob_vector, decreasing = TRUE)[1:k]
  
  tibble(
    artist = names(top_k) %>% 
      str_remove("^\\.pred_") %>% 
      str_replace_all("\\.", " "),
    probability = as.numeric(top_k)
  )
}


top_preds <- get_top_k_preds_for_artwork(final_fit_rf, single_artwork, k=9)
print(top_preds)

