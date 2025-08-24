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


### Data hygiene
rm(list=setdiff(ls(), c("df_train", "df_valid", "df_test")))



# Feature Extraction================================================================================
## Create functions
mad <- function(x) {
  # Calculate the mean of the data
  data_mean <- mean(x, na.rm = TRUE) # na.rm = TRUE handles missing values
  
  # Calculate the absolute deviations from the mean
  absolute_deviations <- abs(x - data_mean)
  
  # Calculate the mean of the absolute deviations
  mad_value <- mean(absolute_deviations, na.rm = TRUE)
  
  return(mad_value)
}


extract_rgb_features <- function(vec) {
  # Pixels are flattened by color
  R <- vec[1:10000]
  G <- vec[10001:20000]
  B <- vec[20001:30000]
  
  # Calculate summary stats on each color
  tibble(
    R_mean=mean(R), R_median=median(R), R_sd=sd(R), R_min=min(R), R_max=max(R), 
      R_q1=quantile(R, 0.25), R_q3=quantile(R, 0.75), R_iqr=quantile(R, 0.75)-quantile(R, 0.25),
      R_range=max(R)-min(R), R_skew=skewness(R), R_kurtosis=kurtosis(R), R_mad=mad(R),
    G_mean=mean(G), G_median=median(G), G_sd=sd(G), G_min=min(G), G_max=max(G), 
      G_q1=quantile(G, 0.25), G_q3=quantile(G, 0.75), G_iqr=quantile(G, 0.75)-quantile(G, 0.25),
      G_range=max(G)-min(G), G_skew=skewness(G), G_kurtosis=kurtosis(G), G_mad=mad(G),
    B_mean=mean(B), B_median=median(B), B_sd=sd(B), B_min=min(B), B_max=max(B), 
      B_q1=quantile(B, 0.25), B_q3=quantile(B, 0.75), B_iqr=quantile(B, 0.75)-quantile(B, 0.25),
      B_range=max(B)-min(B), B_skew=skewness(B), B_kurtosis=kurtosis(B), B_mad=mad(B)
  )
}


count_bin <- function(x, bin) {
  # Count values per bin
  if(bin==1){
    bin_count <- between(x, 0, 51) %>% sum()
  } else if(bin==2){
    bin_count <- between(x, 52, 102) %>% sum()
  } else if(bin==3){
    bin_count <- between(x, 103, 153) %>% sum()
  } else if(bin==4){
    bin_count <- between(x, 154, 204) %>% sum()
  } else if(bin==5){
    bin_count <- between(x, 205, 255) %>% sum()
  } else{stop("'bin' must be an integer from 1 through 5")
  }
  
  # Return value
  return(bin_count)
}

extract_rgb_bins <- function(vec) {
  # Pixels are flattened by color
  R <- vec[1:10000]
  G <- vec[10001:20000]
  B <- vec[20001:30000]
  
  # Calculate color pixel counts by intensity range
  tibble(
    R_bin1=count_bin(R, 1), R_bin2=count_bin(R, 2), R_bin3=count_bin(R, 3),
      R_bin4=count_bin(R, 4), R_bin5=count_bin(R, 5),
    G_bin1=count_bin(G, 1), G_bin2=count_bin(G, 2), G_bin3=count_bin(G, 3),
      G_bin4=count_bin(G, 4), G_bin5=count_bin(G, 5),
    B_bin1=count_bin(B, 1), B_bin2=count_bin(B, 2), B_bin3=count_bin(B, 3),
      B_bin4=count_bin(B, 4), B_bin5=count_bin(B, 5)
  )
  
}

## Apply functions to dataset
df_train_feat0 <- df_train %>%
  mutate(rgb_stats=map(feature_vector, extract_rgb_features),
         rgb_bins=map(feature_vector, extract_rgb_bins)) %>%
  unnest(rgb_stats) %>%
  unnest(rgb_bins) %>%
  select(!feature_vector)

df_valid_feat0 <- df_valid %>%
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
df_train_corr <- df_train_feat0 %>% 
  select(starts_with(c("R_", "G_", "B_"))) %>%
  cor(method="spearman") %>%
  round(3) %>%
  as.data.frame() 

df_train_corr %>%
  mutate(across(everything(), ~if_else(.x>=0.9 & .x<1,
                                       TRUE, NA, NA))) %>% 
  select(where(~sum(.x, na.rm=TRUE)>0)) 

#Here are highly correlated features (8 pairs)
#R_mean-R_median
#R_mean-R_q1
#R_sd-R_mad
  #R_q3-B_q3
  #R_bin5-B_bin5
#G_sd-G_mad
#B_mean-B_median
#B_sd-B_mad

#Assess their overall collinearity
df_train_corr %>%
  summarize(across(c(R_sd, R_mad, G_sd, G_mad, B_mean, B_median, B_sd, B_mad),
            ~median(.x, na.rm=TRUE)))

#Drop the following:
#R_mean (highly correlated with R_median and R_q1)
#R_mad, G_sd, B_mean, B_mad

feat_collinear <- c("R_mean", "R_mad", "G_sd", "B_mean", "B_mad")
  

# Remove one feature highly correlated with at least one same-color feature
df_train_feat <- df_train_feat0 %>% select(!all_of(feat_collinear))
df_valid_feat <- df_valid_feat0 %>% select(!all_of(feat_collinear))
df_test_feat <- df_test_feat0 %>% select(!all_of(feat_collinear))



# Save to RDS=======================================================================================
fp_train_feat <- here("data", paste0("01_train-feat_", 
                                       Sys.Date(),
                                       ".rds"))

fp_valid_feat <- here("data", paste0("01_valid-feat_", 
                                        Sys.Date(),
                                        ".rds"))

fp_test_feat <- here("data", paste0("01_test-feat_", 
                                        Sys.Date(),
                                        ".rds"))

# saveRDS(df_train_feat, fp_train_feat)
# saveRDS(df_valid_feat, fp_valid_feat)
# saveRDS(df_test_feat, fp_test_feat)



