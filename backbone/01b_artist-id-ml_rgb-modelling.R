# Artist Identification: Model Fitting, Tuning, and Evaluation
# Using RGB stats and bins


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, tidymodels, magick, e1071)

source(here("fns_objs", "00_fn-backbone.R"))

fp_train_feat <- grab_newest_fp(dir=here("data"),
                               patt="^01_train-feat_")

fp_test_feat <- grab_newest_fp(dir=here("data"),
                               patt="^01_test-feat")



df_train_unprepped <- readRDS(fp_train_feat) %>%
  select(object_id, artist_clean, starts_with(c("R_", "G_", "B_")))

df_test_unprepped <- readRDS(fp_test_feat) %>%
  select(object_id, artist_clean, starts_with(c("R_", "G_", "B_")))



# Build and Fit Models==============================================================================
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



# Evaluate Best Model on Test Set==================================================================
## Finalize best workflow & model (RF)-------------------
### Finalize workflow
final_rf <- finalize_workflow(workflow_rf, params_rf_best)


### Fit final model (on all training data)
final_fit_rf <- fit(final_rf, data=df_train_unprepped) 
fn_model <- paste0("01_artist_id-ml_rgb", "_", Sys.Date(), ".rds")
# saveRDS(final_fit_rf, here("models", fn_model))


## Assess predictions-------------------
### Apply best model to test set
df_pred_rf <- predict(object=final_fit_rf,
                      new_data=df_test_unprepped,
                      type="class")


### Overall accuracy on test set
df_test_unprepped %>%
  select(object_id, obs="artist_clean") %>%
  bind_cols(df_pred_rf %>% rename(pred=".pred_class"))
#24.8%


### Get class probabilities
df_pred_probs_rf <- predict(final_fit_rf, 
                            new_data=df_test_unprepped,
                            type="prob")


### Get top x predictions for the whole test set
#### Pull in object_ids & true label
df_pred_probs_obs <- df_test_unprepped %>%
  select(object_id, obs="artist_clean") %>%
  bind_cols(df_pred_probs_rf)


### Grab top predictions, their probabilities, and matches
df_top_preds <- get_top_k_preds(df_pred_probs_obs, k=3)



# Predict on New Image==============================================================================
## Load and extract image features-------------------
### Get image fp
fp_img_new <- here("img", "starry night.jpeg")


### Import and process image
img_new_processed <- fp_img_new %>%
  image_read() %>%
  image_strip() %>%
  image_resize("100x100") %>%
  image_extent("100x100", gravity="center", color="white")


### Convert to feature vector
img_new_feat <- img_new_processed %>%
  image_data() %>% 
  as.vector() %>%
  as.integer()


### Extract features
#vector of cols to remove (high collinearity or zero variance)
feat_remove <- c("B_bin1", "B_mean", "G_bin1", "R_mean", "B_sd", "G_sd", "R_bin1", "R_sd",
                 "R_range", "B_max")

#extraction, combination, and removal
df_img_new_feat <- bind_cols(
  object_id=0,
  extract_rgb_features(img_new_feat),
  extract_rgb_bins(img_new_feat)
) %>%
select(!all_of(feat_remove))



## Run prediction-------------------
### Predict all class probabilities
preds_img_new <- predict(final_fit_rf, df_img_new_feat, type = "prob")


### Get top k predictions
top_preds_img_new <- get_top_k_preds_for_artwork(df_img_new_feat, final_fit_rf, k=5)
print(top_preds_img_new)
#Van Gogh is 3

