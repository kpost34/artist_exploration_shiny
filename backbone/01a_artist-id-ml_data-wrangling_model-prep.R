# Artist Identification Model: Data Wrangling & Model Prep


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, rsample)

source(here("fns_objs", "00_fn.R"))

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
#split data (1999 obj ids) into training and validation sets
#70% training, 15% validation, and 15% test
#if artist has 1 artwork then training, if 3+ then at least 1 in each set, etc.

## Explore data
#get breakdown of n artworks and artists with 1 or >1 artworks
df_art_feat %>%
  count(artist_clean) %>% 
  mutate(n_gt1 = n > 1) %>%
  group_by(n_gt1) %>%
  summarize(n_artworks=sum(n),
            n_artists=n())
#466 artists with 1 artwork
#333 artists with 1532 artworks


#get breakdown of n artworks and artists by n artworks/artist
df_art_ns <- df_art_feat %>%
  #count n artwork by artist
  count(artist_clean, name="n_artworks") %>%
  #group by n artwork and then count total paintings & n artists
  group_by(n_artworks) %>%
  summarize(total_artworks=n(),
            n_artists=n_distinct(artist_clean),
            artists=paste(artist_clean, collapse="; ")) 


df_art_ns %>%
  #filter for cases of 3+ artists (needed for validation and test sets)
  filter(n_artworks >= 3, n_artworks < 6) %>% 
  summarize(total_artists=sum(n_artists)) 
#120

df_art_ns %>%
  filter(n_artworks >= 6, n_artworks < 9) %>% 
  summarize(total_artists=sum(n_artists)) 
#28 * 2 = 56
  
df_art_ns %>%
  filter(n_artworks >= 9) %>% 
  summarize(total_artists=sum(n_artists)) 
#40 * 3 = 120
#120 + 56 + 120 = 296 which is ~15%


## Split data
### Singular and double artworks
df_train1 <- filter_join_artists(df_art_ns, n_max=2) %>%
  mutate(set="train")

#checks
df_train1 %>% count(artist_clean) %>% distinct(n) #1 & 2
df_train1 %>% summarize(n_obj_id=n_distinct(object_id)) #756 (466 + (145*2))


### Create groups of other n_artworks
#3-5 artworks per artist
df_art_feat35 <- filter_join_artists(df_art_ns, n_min=3, n_max=5) #433

#6-8 artworks per artist
df_art_feat68 <- filter_join_artists(df_art_ns, n_min=6, n_max=8) #194

#9+ artworks per artist
df_art_feat9plus <- filter_join_artists(df_art_ns, n_min=9) #615


### Assign datasets to each n_artwork bucket
set.seed(29)

df_tvt35 <- df_art_feat35 %>%
  group_by(artist_clean) %>%
  group_modify(~assign_split(.x, n_valid_test=1)) %>%
  ungroup() 

check_artwork_range(df_tvt35) #1-1 for test & valid, 1-3 for train
check_n_artists(df_tvt35) #120 for each 


df_tvt68 <- df_art_feat68 %>%
  group_by(artist_clean) %>%
  group_modify(~assign_split(.x, n_valid_test=2)) %>%
  ungroup() 

check_artwork_range(df_tvt68) #2-2 for test & valid, 2-4 for train
check_n_artists(df_tvt68) #28 for each 


df_tvt9plus <- df_art_feat9plus %>%
  group_by(artist_clean) %>%
  group_modify(~assign_split(.x, n_valid_test=3)) %>%
  ungroup() 

check_artwork_range(df_tvt9plus) #3-3 for test & valid, 3-31 for train
check_n_artists(df_tvt9plus) #40 for each 


### Combine DFs, split by dataset, and write to environment
bind_rows(df_train1, df_tvt35, df_tvt68, df_tvt9plus) %>%
  mutate(set=paste0("df_", set)) %>%
  split(.$set) %>%
  purrr::map(select, !set) %>%
  list2env(envir=.GlobalEnv)



### Flatten feature vector (separately due to limited RAM)
rm(list=setdiff(ls(), c("df_train", "df_valid", "df_test")))

df_train <- df_train %>% unnest_wider(feature_vector, names_sep="_")
df_valid <- df_valid %>% unnest_wider(feature_vector, names_sep="_")
df_test <- df_test %>% unnest_wider(feature_vector, names_sep="_")


### Make splits 
train_test_split <- make_splits(
  x=bind_rows(df_train, df_valid),
  assessment=df_test
)

train_valid_split <- make_splits(
  x=df_train,
  assessment=df_valid
)


### Data hygiene
rm(list=setdiff(ls(), c("train_test_split", "train_valid_split")))



# Model Prep========================================================================================
## Create dfs from splits
df_train <- training(train_valid_split)
df_valid <- testing(train_valid_split)
df_test <- testing(train_test_split)


## Create and prep recipe 
cols_rgb <- names(df_test) %>% str_subset("^feature")

rec_art <- recipe(~ ., data=df_train) %>%
  # update_role(~all_of(cols_rgb), new_role="id") %>%
  step_normalize(all_of(cols_rgb)) %>%
  step_pca(all_of(cols_rgb), threshold=0.9)

rec_prep_art <- prep(rec_art)


## Normalize features & run PCA
### Assess threshold
#### Get the prcomp object from the recipe
pca_obj <- rec_prep_art$steps[[2]]$res


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
  geom_hline(yintercept=0.9, linetype="dashed", color="red") +
  labs(title="Cumulative Variance Explained by PCA Components",
       x="Principal Component",
       y="Cumulative Variance Explained") +
  ylim(c(.8, 1)) +
  theme_minimal()

which(df_pca_info$Cumulative >= 0.8)[1] #25 PCs
which(df_pca_info$Cumulative >= 0.9)[1] #97 PCs
which(df_pca_info$Cumulative >= 0.95)[1] #237 PCs


#### Make scree plot (to ID elbow)
df_pca_info[50:250,] %>%
  ggplot(aes(x=PC, y=Eigenvalue)) +
  geom_col() +
  theme_bw()
#no discernible elbow


#### Find PCs where EVs are above 1
df_pca_info %>%
  filter(Eigenvalue > 1) %>%
  pull(PC) %>%
  max()
#712!


# Clear extraneous objects
rm(list=setdiff(ls(), c("rec_prep_art", "df_train", "df_valid", "df_test")))


# Run PCA (using 0.9 threshold from above)
pca_train <- bake(rec_prep_art, new_data=df_train)
pca_valid <- bake(rec_prep_art, new_data=df_valid)
pca_test <- bake(rec_prep_art, new_data=df_test)



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

# saveRDS(pca_train, fp_pca_train)
# saveRDS(pca_valid, fp_pca_valid)
# saveRDS(pca_test, fp_pca_test)
