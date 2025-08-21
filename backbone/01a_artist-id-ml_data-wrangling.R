# Artist Identification Model: Data Wrangling


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
### Create functions
#### Filter Ns DF by size and join back to large DF
filter_join_artists <- function(df, n_min=NA, n_max=NA){
  df_new <- df %>%
    {if(!is.na(n_min)) filter(., n_artworks >= n_min) else .} %>%
    {if(!is.na(n_max)) filter(., n_artworks <= n_max) else .} %>%
    separate_longer_delim(artists, delim="; ") %>%
    select(artist_clean="artists") %>%
    inner_join(df_art_feat) %>%
    relocate(artist_clean, .before="artist_simple") 
  
  return(df_new)
}


#### Assign dataset to rows
assign_split <- function(group_df, n_valid_test = 1) {
  n <- nrow(group_df)
  if (n < 2 * n_valid_test) {
    stop(paste("Not enough rows in group", unique(group_df$group), "to assign", n_valid_test, "to valid and test"))
  }

  indices <- sample(n)
  
  valid_idx <- indices[1:n_valid_test]
  test_idx <- indices[(n_valid_test + 1):(2 * n_valid_test)]
  
  group_df$set <- "train"
  group_df$set[valid_idx] <- "valid"
  group_df$set[test_idx] <- "test"
  
  return(group_df)
}


#### QC functions
#check min and max artworks per artist by dataset
check_artwork_range <- function(df) {
  df1 <- df %>%
    group_by(artist_clean, set) %>%
    count() %>% 
    group_by(set) %>%
    summarize(min=min(n),
              max=max(n))
  
  return(df1)
}

#check number of artists per dataset
check_n_artists <- function(df) {
  df1 <- df %>%
    group_by(set) %>%
    summarize(n_artists=n_distinct(artist_clean))
  
  return(df1)
}


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



# Save Splits to RDS================================================================================
fp_train_test_split <- here("data", paste0("01_train-test-split_", 
                                              Sys.Date(),
                                              ".rds"))
fp_train_valid_split <- here("data", paste0("01_train-valid-split_", 
                                              Sys.Date(),
                                              ".rds"))

# saveRDS(train_test_split, fp_train_test_split)
# saveRDS(train_valid_split, fp_train_valid_split)
