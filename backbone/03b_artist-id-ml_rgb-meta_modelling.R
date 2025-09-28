# Artist Identification: Model Fitting, Tuning, and Evaluation
# Using RGB & Metadata


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, tidymodels, future)

source(here("fns_objs", "00_fn-backbone.R"))

fp_train_feat <- grab_newest_fp(dir=here("data"),
                                patt="^03_train-feat_")

fp_app_feat <- grab_newest_fp(dir=here("data"),
                              patt="^03_app-feat_")

df_train_final <- readRDS(fp_train_feat)
df_app_final <- readRDS(fp_app_feat)



# Build, Fit, & Assess Models=======================================================================
## Create folds--------------------
#note: manually done because of small sample sizes for some artists
set.seed(1)
folds_cv <- vfold_cv(df_train_final, v = 3, strata = artist_clean)
#warning: unstratified resampling

check_analysis_assess(df=df_train_final, n_folds=3)
#meets criteria for cross-validation


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


### Select the best parameters
show_best(tune_results_rf, metric="accuracy", n=5) #46-48 %
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
  control=control_grid(
    save_pred=TRUE,
    verbose=TRUE)
)


### Select the best parameters
show_best(tune_results_xgb, metric="accuracy", n=5) #21.9%
params_xgb_best <- select_best(tune_results_xgb, metric="accuracy")



# Select Best Model and Save For App================================================================
## Finalize workflow
final_rf <- finalize_workflow(workflow_rf, params_rf_best)


## Fit final model (on all training data)
final_fit_rf <- fit(final_rf, data=df_train_final) 


## Save model to file
fn_model <- paste0("03_artist_id-ml_rgb-meta", "_", Sys.Date(), ".rds")
# saveRDS(final_fit_rf, here("models", fn_model))


















