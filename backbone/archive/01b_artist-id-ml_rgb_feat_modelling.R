# Artist Identification: Model Fitting, Tuning, and Evaluation
# Using RGB stats and bins


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, tidymodels)

source(here("fns_objs", "00_fn-backbone.R"))

fp_train_feat <- grab_newest_fp(dir=here("data"),
                               patt="^01_train-feat_")
fp_valid_feat <- grab_newest_fp(dir=here("data"),
                               patt="^01_valid-feat_")
fp_test_feat <- grab_newest_fp(dir=here("data"),
                               patt="^01_test-feat")


df_train_unprepped <- readRDS(fp_train_feat)

df_valid_unprepped <- readRDS(fp_valid_feat)

df_test_unprepped <- readRDS(fp_test_feat) 



# Build & Fit Model=================================================================================
## Recipe
### Create recipe
cols_remove <- names(df_train_unprepped) %>%
  str_subset("^(R_|G_|B_)|artist_clean|object_id", negate=TRUE)

rec <- recipe(artist_clean ~ ., data=df_train_unprepped) %>%
  update_role(object_id, new_role="id") %>%
  step_rm(all_of(cols_remove)) %>%
  step_zv(matches("^(R_|G_|B_)")) %>%
  step_normalize(matches("^(R_|G_|B_)"))


### Prepare recipe
rec_prep <- prep(rec, training=df_train_unprepped)


### Apply recipe to each dataset
df_train_baked <- bake(rec_prep, new_data=df_train_unprepped)
df_valid_baked <- bake(rec_prep, new_data=df_valid_unprepped)
df_test_baked <- bake(rec_prep, new_data=df_test_unprepped)


## Random forest
### Create model
model_rf <- rand_forest(mtry = 15, trees = 200, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("classification")


### Create workflow
workflow_rf <- workflow() %>%
  add_model(model_rf) %>%
  add_recipe(rec)


### Fit model
fit_rf <- fit(workflow_rf, data = df_train_unprepped)


### Evaluate model on validation data
preds_valid_rf <- predict(fit_rf, df_valid_unprepped) %>%
  bind_cols(df_valid_baked)

metrics(preds_valid_rf, truth = artist_clean, estimate = .pred_class)
#accuracy = 0.137
#kap = 0.125


## XGBoost
### Create model
model_xgb <- boost_tree(
  trees = 1000, #tree number
  tree_depth = 6, #max tree depth
  learn_rate = 0.01,#learn rate
  loss_reduction = 0, #min loss reduction (gamma)
  sample_size = 0.8, #subsample ratio
  mtry = 10 #n predictors to sample at each split
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")


### Create workflow
workflow_xgb <- workflow() %>%
  add_model(model_xgb) %>%
  add_recipe(rec)


### Fit model
fit_xgb <- fit(workflow_xgb, data = df_train_unprepped)


### Evaluate model on validation data
preds_valid_xgb <- predict(fit_xgb, df_valid_unprepped) %>%
  bind_cols(df_valid_baked)

metrics(preds_valid_xgb, truth = artist_clean, estimate = .pred_class)
#accuracy = 0.153
#kap = 0.14







