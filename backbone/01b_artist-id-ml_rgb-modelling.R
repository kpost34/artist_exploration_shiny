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
#warning: unstratified resamplling

#check artists per fold
folds_cv %>%
  mutate(data=map(splits, analysis)) %>%
  unnest(data) %>%
  select(!splits) %>% 
  group_by(artist_clean) %>% 
  summarize(n_folds=n_distinct(id)) %>%
  summarize(n_artists=n(),
            min_folds=min(n_folds))
#all 17 artists are in 3 folds

folds_cv %>%
  mutate(data=map(splits, assessment)) %>%
  unnest(data) %>%
  select(!splits) %>% 
  group_by(artist_clean) %>% 
  summarize(n_folds=n_distinct(id)) %>%
  summarize(n_artists=n(),
            min_folds=min(n_folds))
#all 35 artists are in 3 folds


## Create recipe--------------------
rec <- recipe(artist_clean ~ ., data=df_train_unprepped) %>%
  update_role(object_id, new_role="id") %>%
  step_normalize(all_predictors())


## Random forest--------------------
### Create model
model_rf <- rand_forest(
  mtry = tune(), 
  trees = 200, 
  min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("classification")


### Create workflow
workflow_rf <- workflow() %>%
  add_model(model_rf) %>%
  add_recipe(rec)


### Define parameter grid
params_rf <- parameters(model_rf) %>%
  update(mtry=mtry(range=c(1, 30)))

grid_rf <- grid_random(params_rf, size=10)


### Tune
set.seed(202)
tune_results_rf <- tune_grid(
  workflow_rf,
  resamples=folds_cv,
  grid=grid_rf,
  metrics=metric_set(accuracy, kap, roc_auc),
  control=control_grid(save_pred=TRUE)
)


### Select the best parameters
show_best(tune_results_rf, metric="accuracy", n=5) #22.2%
params_rf_best <- select_best(tune_results_rf, metric="accuracy")


### Finalize workflow
final_rf <- finalize_workflow(workflow_rf, params_rf_best)


### Fit final model (on all training data)
final_fit_rf <- fit(final_rf, data=df_train_unprepped) 


## XGBoost--------------------
### Create model
model_xgb <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), 
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")


### Create workflow
workflow_xgb <- workflow() %>%
  add_model(model_xgb) %>%
  add_recipe(rec)


### Define parameter grid
params_xgb <- parameters(model_xgb) 

grid_xgb <- grid_space_filling(params_xgb, size=5)


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
show_best(tune_results_xgb, metric="accuracy", n=5) #24.3%
params_xgb_best <- select_best(tune_results_xgb, metric="accuracy")


### Finalize workflow
final_xgb <- finalize_workflow(workflow_xgb, params_xgb_best)


### Fit final model (on all training data)
final_fit_xgb <- fit(final_xgb, data=df_train_unprepped) 


## KNN--------------------
### Create model
model_knn <- nearest_neighbor(neighbors=tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")


### Create workflow
workflow_knn <- workflow() %>%
  add_model(model_knn) %>%
  add_recipe(rec)


### Define parameter grid
params_knn <- parameters(model_knn) %>%
  update(neighbors=mtry(range=c(1, 15)))

grid_knn <- grid_random(params_knn, size=10)


### Tune
tune_results_knn <- tune_grid(
  workflow_knn,
  resamples=folds_cv,
  grid=grid_knn,
  metrics=metric_set(accuracy, kap, roc_auc),
  control=control_grid(save_pred=TRUE)
)


### Select the best parameters
show_best(tune_results_knn, metric="accuracy", n=5) #22.4%
params_knn_best <- select_best(tune_results_knn, metric="accuracy")


### Finalize workflow
final_knn <- finalize_workflow(workflow_knn, params_knn_best)


### Fit final model (on all training data)
final_fit_knn <- fit(final_knn, data=df_train_unprepped) 



## Evaluate on test set--------------------
### Apply best model to test set
df_pred_xgb <- predict(object=final_fit_xgb,
                       new_data=df_test_unprepped,
                       type="class")


### Get class probabilities
df_pred_probs_xgb <- predict(final_fit_xgb, 
                     new_data=df_test_unprepped,
                     type="prob")

#NOTE: need to pair with object_id --> use bind_cols()

# How to get top x preds
get_top_k_preds <- function(prob_row, class_labels, k = 3) {
  # Sort class probabilities in descending order
  top_k <- sort(prob_row, decreasing = TRUE)[1:k]
  tibble(
    artist = names(top_k),
    probability = as.numeric(top_k)
  )
}

# Assuming you predict one row at a time
class_labels <- colnames(df_pred_probs_xgb)
top_preds <- get_top_k_preds(df_pred_probs_xgb[1, ], class_labels, k = 3)
print(top_preds)


# Predict on new image
## Load image, process image to RGBs, calculate stats, remove zvs and high corrs
## Run prediction
# Predict class probabilities
preds <- predict(model, new_image_features, type = "prob")

# Get top-3 predictions
top_preds <- get_top_k_preds(preds[1, ], colnames(preds), k = 3)


