# Artist Information Model: Data Wrangling & Model Prep
# Using RGB stats and bins


# Load Packages, Functions, and Data================================================================
pacman::p_load(here, tidyverse, httr, tidymodels, e1071)

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



# Data Filtering & Splitting========================================================================
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


### Compare all artworks and artists using different filters
#none
df_art_ns %>%
  summarize(grand_total_artwork=sum(total_artworks),
            total_artists=sum(n_artists)) 
#1998 artworks from 799 artists

#5
df_art_ns %>%
  filter(n_artworks >= 5) %>%
  summarize(grand_total_artwork=sum(total_artworks),
            total_artists=sum(n_artists)) 
#884 from 83

#10
df_art_ns %>%
  filter(n_artworks >= 10) %>%
  summarize(grand_total_artwork=sum(total_artworks),
            total_artists=sum(n_artists)) 
#570 from 35


### Choose artists with at least 10 artworks each
vec_artist_clean <- df_art_feat %>%
  #count n artwork by artist
  count(artist_clean, name="n_artworks") %>%
  filter(n_artworks>=10) %>% 
  pull(artist_clean)

df_art_feat_mod <- df_art_feat %>%
  filter(artist_clean %in% vec_artist_clean)


### Split data
#### Execute
set.seed(33)
df_train <- df_art_feat_mod %>%
  group_by(artist_clean) %>%
  slice_sample(prop=0.8) %>%
  ungroup()

df_test <- df_art_feat_mod %>%
  anti_join(df_train, by="object_id")


#### Check splits
check_artwork_range(df_art_feat_mod) #10-37
check_artwork_range(df_train) #8-29
check_artwork_range(df_test) #2-8
#checks out given the 4:1 split

check_n_art(df_art_feat_mod) #35, 570
check_n_art(df_train) #35, 441
check_n_art(df_test) #35, 129
#checks out given that each df has 35 artists and the totals are ~4:1


### Data hygiene
rm(list=setdiff(ls(), c("df_train", "df_test", "mad", "extract_rgb_features", "count_bin", 
                        "extract_rgb_bins")))



# Feature Extraction================================================================================
## Run extraction functions on dataset
df_train_feat0 <- df_train %>%
  mutate(rgb_stats=map(feature_vector, extract_rgb_features),
         rgb_bins=map(feature_vector, extract_rgb_bins)) %>%
  unnest(rgb_stats) %>%
  unnest(rgb_bins) %>%
  select(!feature_vector)


df_test_feat0 <- df_test %>%
  mutate(rgb_stats=map(feature_vector, extract_rgb_features),
         rgb_bins=map(feature_vector, extract_rgb_bins)) %>%
  unnest(rgb_stats) %>%
  unnest(rgb_bins) %>%
  select(!feature_vector)


## Assess collinearity
### Compute pairwise correlations
df_train_corr <- df_train_feat0 %>% 
  select(starts_with(c("R_", "G_", "B_"))) %>%
  cor(method="spearman") %>%
  abs() %>%
  round(3) %>%
  as.data.frame() 


### Find fields that contain at least one highly correlated pair
df_train_corr_collin <- df_train_corr %>% 
  mutate(across(everything(), ~if_else(.x>=0.9 & .x<1,
                                       TRUE, NA, NA))) %>% 
  select(where(~sum(.x, na.rm=TRUE)>0))


## Find features to drop because of their multicollinearity
### Find highly correlated pairs and whether 
df_train_corr_sig <- names(df_train_corr_collin) %>%
  purrr::map_df(function(var) {
    df_train_corr %>%
      select(all_of(var)) %>%
      filter(!!sym(var) >= 0.9 & !!sym(var) < 1) %>%
      mutate(!!sym(var):=var) %>%
      rename("var2"=var) %>%
      rownames_to_column(var="var1") %>%
      rowwise() %>%
      mutate(var_pair=paste(sort(c(var1, var2)), collapse="__")) %>%
      distinct(var_pair) %>%
      separate_wider_delim(var_pair, delim="__",
                           names=c("var1", "var2"),
                           cols_remove=TRUE) 
  }) %>%
  distinct(var1, var2) 


### Find features in multiple pairs
vec_var_collin_mult <- df_train_corr_sig %>%
  select(var="var1") %>%
  bind_rows(
    df_train_corr_sig %>%
      select(var="var2")
  ) %>%
  count(var) %>%
  filter(n>1) %>%
  pull(var)


### Find median correlations of features with other features
df_train_corr_med <- df_train_corr %>%
  summarize(across(everything(), ~median(.x, na.rm=TRUE))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var="var") %>%
  rename(corr_median="V1")


### Create final list of features to drop 
feat_collinear <- df_train_corr_sig %>%
  #pull in median correlations
  left_join(df_train_corr_med, by=c("var1"="var")) %>%
  left_join(df_train_corr_med, by=c("var2"="var"),
            suffix=c("_var1", "_var2")) %>%
  #assess if same color channel and whether in multiple highly correlated pairs
  mutate(same_color=str_sub(var1, 1, 1)==str_sub(var2, 1, 1),
         var1_contains_high_corr=var1 %in% vec_var_collin_mult,
         var2_contains_high_corr=var2 %in% vec_var_collin_mult) %>%
  arrange(desc(var1_contains_high_corr), var1, desc(var2_contains_high_corr), var2) %>%
  #remove cross-channel corrs
  filter(same_color) %>%
  #decision-making for dropping
  mutate(var1_decision=case_when(
    var1_contains_high_corr & !var2_contains_high_corr  ~ "Drop",
    !var1_contains_high_corr & var2_contains_high_corr  ~ "Keep",
      corr_median_var1 >= corr_median_var2              ~ "Drop",
      corr_median_var1 < corr_median_var2               ~ "Keep"),
    var_drop=ifelse(var1_decision=="Drop", var1, var2)) %>%
  pull(var_drop) %>%
  unique()



# Remove one feature highly correlated with at least one same-color feature
df_train_feat <- df_train_feat0 %>% select(!all_of(feat_collinear))
df_test_feat <- df_test_feat0 %>% select(!all_of(feat_collinear))



# Save to RDS=======================================================================================
fp_train_feat <- here("data", paste0("01_train-feat_", 
                                       Sys.Date(),
                                       ".rds"))

fp_test_feat <- here("data", paste0("01_test-feat_", 
                                        Sys.Date(),
                                        ".rds"))

saveRDS(df_train_feat, fp_train_feat)
saveRDS(df_test_feat, fp_test_feat)



