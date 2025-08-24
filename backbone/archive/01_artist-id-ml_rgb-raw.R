# Artist Identification Model: Data Wrangling, Model Prep, and Model Fitting
# Using raw RGB values


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, tidymodels)

source(here("fns_objs", "00_fn-backbone.R"))

fp_art_feat <- grab_newest_fp(dir=here("data"), 
                              patt="^00_art-info-final_feat")

df_art_feat0 <- readRDS(fp_art_feat)



# Aside: Find missing data=======================================================================
# df_art_feat1 <- df_art_feat0 %>% slice_sample(n=1000)
# df_art_feat2 <- df_art_feat0 %>% 
#   anti_join(df_art_feat1, by="object_id")
# 
# df_art_feat_wide1 <- df_art_feat1 %>% unnest_wider(feature_vector, names_sep="_")
# df_art_feat_wide2 <- df_art_feat2 %>% unnest_wider(feature_vector, names_sep="_")
# 
# cols_rgb <- names(df_art_feat_wide1) %>% str_subset("^feature")
# 
# df_art_feat_wide1[!complete.cases(df_art_feat_wide1),] %>% 
#   pull(object_id) #435846
# 
# df_art_feat_wide2[!complete.cases(df_art_feat_wide2),] %>% 
#   pull(object_id) #none

artist_incomplete <- df_art_feat0 %>% 
  filter(object_id==435846) %>% 
  pull(artist_clean)

df_art_feat <- df_art_feat0 %>%
  filter(artist_clean!=artist_incomplete) 
#only 1 artwork (so does not affect distribution of artworks across datasets)



# Data Splitting====================================================================================
## Explore data
### Get breakdown of n artworks and artists by n artworks/artist
df_art_ns <- df_art_feat %>%
  #count n artwork by artist
  count(artist_clean, name="n_artworks") %>%
  #group by n artwork and then count total paintings & n artists
  group_by(n_artworks) %>%
  summarize(total_artworks=sum(n_artworks),
            n_artists=n_distinct(artist_clean),
            artists=paste(artist_clean, collapse="; ")) 


### Compare all to 5+ artworks per artist
df_art_ns %>%
  summarize(grand_total_artwork=sum(total_artworks),
            total_artists=sum(n_artists)) #1998 artworks from 799 artists

df_art_ns %>%
  filter(n_artworks >= 5) %>%
  summarize(grand_total_artwork=sum(total_artworks),
            total_artists=sum(n_artists)) #884 from 83


## Splitting data (min threshold + asymmetry)
### Group into the following: 5, 6, 7, and 8+ artworks
#5 artists
df_art_feat5 <- filter_join_artists(df_art_ns, n_min=5, n_max=5) 
#75 total: 15 artists: 45-15-15

#6 artists
df_art_feat6 <- filter_join_artists(df_art_ns, n_min=6, n_max=6)
#60 total: 10 artists: 30-10-20

#7 artists
df_art_feat7 <- filter_join_artists(df_art_ns, n_min=7, n_max=7)
#70 total: 10 artists: 40-10-20

#8+ artists
df_art_feat8plus <- filter_join_artists(df_art_ns, n_min=8)
#679 total: 48 artists: 487-96-96


#totals: 602-131-151


### Assign datasets to each n_artwork bucket
set.seed(41)

df_tvt5 <- df_art_feat5 %>%
  group_by(artist_clean) %>%
  group_modify(~assign_split_custom(.x, n_valid=1, n_test=1)) %>%
  ungroup() 

check_artwork_range(df_tvt5) #1-1 for test & valid, 3-3 for train
check_n_art(df_tvt5) #15 artists for each set & 15 artworks for test & valid and 15-45 for train


df_tvt6 <- df_art_feat6 %>%
  group_by(artist_clean) %>%
  group_modify(~assign_split_custom(.x, n_valid=1, n_test=2)) %>%
  ungroup() 

check_artwork_range(df_tvt6) #2-2 for test, 1-1 for valid, and 3-3 for train
check_n_art(df_tvt6) #10 artists for each set & 20-10-30 artworks for valid-test-train


df_tvt7 <- df_art_feat7 %>%
  group_by(artist_clean) %>%
  group_modify(~assign_split_custom(.x, n_valid=1, n_test=2)) %>%
  ungroup() 

check_artwork_range(df_tvt7) #2-2 for test, 1-1 for valid, and 4-4 for train
check_n_art(df_tvt7) #10 artists for each set & 20-40-10 artworks for test-train-valid


df_tvt8plus <- df_art_feat8plus %>%
  group_by(artist_clean) %>%
  group_modify(~assign_split_custom(.x, n_valid=2, n_test=2)) %>%
  ungroup() 

check_artwork_range(df_tvt8plus) #2-2 for test & valid, and 4-33 for train
check_n_art(df_tvt8plus) #48 artists for each set & 96 artworks for test and valid & 487 for train


### Combine DFs, split by dataset, and write to environment
bind_rows(df_tvt5, df_tvt6, df_tvt7, df_tvt8plus) %>%
  mutate(set=paste0("df_", set)) %>%
  split(.$set) %>%
  purrr::map(select, !set) %>%
  list2env(envir=.GlobalEnv)


### Flatten feature vector (separately due to limited RAM)
rm(list=setdiff(ls(), c("df_train", "df_valid", "df_test")))

df_train_wide <- df_train %>% unnest_wider(feature_vector, names_sep="_")
df_valid_wide <- df_valid %>% unnest_wider(feature_vector, names_sep="_")
df_test_wide <- df_test %>% unnest_wider(feature_vector, names_sep="_")


### Make splits 
# train_test_split <- make_splits(
#   x=bind_rows(df_train, df_valid),
#   assessment=df_test
# )
# 
# train_valid_split <- make_splits(
#   x=df_train,
#   assessment=df_valid
# )


### Data hygiene
rm(list=setdiff(ls(), c("df_train_wide", "df_valid_wide", "df_test_wide")))



# Model Prep========================================================================================
## Create dfs from splits
# df_train <- training(train_valid_split)
# df_valid <- testing(train_valid_split)
# df_test <- testing(train_test_split)


## Create and prep recipe 
rec_art <- recipe(~ ., data=df_train_wide) %>%
  step_zv(starts_with("feature")) %>% #removes features with 0 variance
  step_normalize(starts_with("feature")) %>% #normalizes features
  step_pca(starts_with("feature"), threshold=0.9) #runs pca


rec_prep_art <- prep(rec_art)

#check pca inputs
tidy(rec_prep_art, number=3) %>%
  filter(!str_detect(terms, "feature")) 
#empty tibble because no other features--expected


## Normalize features & run PCA
### Assess threshold
#### Get the prcomp object from the recipe
pca_obj <- rec_prep_art$steps[[3]]$res


#### Extract proportion of var, cum var, and evs
var_explained <- summary(pca_obj)$importance
pca_evs <- pca_obj$sdev^2


#### Make a DF for plotting
df_pca_info <- tibble(
  PC = seq_along(pca_evs),
  Eigenvalue = pca_evs,
  Proportion = pca_evs / sum(pca_evs),
  Cumulative = cumsum(pca_evs) / sum(pca_evs)
)


#### Plot cumulative variance
df_pca_info %>%
  ggplot(aes(x=PC, y=Cumulative)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=0.95, linetype="dashed", color="red") +
  labs(title="Cumulative Variance Explained by PCA Components",
       x="Principal Component",
       y="Cumulative Variance Explained") +
  ylim(c(.8, 1)) +
  theme_minimal()

which(df_pca_info$Cumulative >= 0.9)[1] #79 PCs
which(df_pca_info$Cumulative >= 0.925)[1] #112 PCs
which(df_pca_info$Cumulative >= 0.95)[1] #165 PCs


#### Make scree plot (to ID elbow)
df_pca_info[50:200,] %>%
  ggplot(aes(x=PC, y=Eigenvalue)) +
  geom_col() +
  theme_bw()
#no discernible elbow


#### Find PCs where EVs are above 1
df_pca_info %>%
  filter(Eigenvalue > 1) %>%
  pull(PC) %>%
  max()
#532


# Clear extraneous objects
rm(list=setdiff(ls(), c("rec_prep_art", "df_train_wide", "df_valid_wide", "df_test_wide")))


# Run PCA (using 0.9 threshold from above)
df_train_pca <- bake(rec_prep_art, new_data=df_train_wide)
df_valid_pca <- bake(rec_prep_art, new_data=df_valid_wide)
df_test_pca <- bake(rec_prep_art, new_data=df_test_wide)



# Save PCAs to File=================================================================================
fp_pca_train <- here("data", paste0("01_pca-train_", 
                                     Sys.Date(),
                                     ".rds"))
fp_pca_valid <- here("data", paste0("01_pca-valid_", 
                                     Sys.Date(),
                                     ".rds"))
fp_pca_test <- here("data", paste0("01_pca-test_", 
                                    Sys.Date(),
                                    ".rds"))

# saveRDS(df_train_pca, fp_pca_train)
# saveRDS(df_valid_pca, fp_pca_valid)
# saveRDS(df_test_pca, fp_pca_test)



# Model Fitting=====================================================================================
## Select subset of cols
df_train_unprepped <- df_train_pca %>%
  select(object_id, artist_clean, starts_with("PC"))

df_valid_unprepped <- df_valid_pca %>%
  select(object_id, artist_clean, starts_with("PC"))

df_test_unprepped <- df_test_pca %>%
  select(object_id, artist_clean, starts_with("PC"))


## Recipe
### Create recipe
rec_mod <- recipe(artist_clean ~ ., data = df_train_unprepped) %>%
  update_role(object_id, new_role = "id")


### Prepare recipe
rec_mod_prep <- prep(rec_mod, training=df_train_unprepped)


### Apply recipe to each dataset
df_train_baked <- bake(rec_mod_prep, new_data=df_train_unprepped)
df_valid_baked <- bake(rec_mod_prep, new_data=df_valid_unprepped)
df_test_baked <- bake(rec_mod_prep, new_data=df_test_unprepped)


## Random forest
### Create model
model_rf <- rand_forest(mtry = 15, trees = 200, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("classification")


### Create workflow
workflow_rf <- workflow() %>%
  add_model(model_rf) %>%
  add_recipe(rec_mod)


### Fit model
fit_rf <- fit(workflow_rf, data = df_train_unprepped)


### Evaluate model on validation data
preds_valid_rf <- predict(fit_rf, df_valid_unprepped) %>%
  bind_cols(df_valid_baked)

metrics(preds_valid_rf, truth = artist_clean, estimate = .pred_class)
#accuracy = 0.130
#kap = 0.117

#poor accuracy: low n, high p problem and not using a CNN --> extract features from vectors

