# Artist Identification: Data Wrangling & Feature Extraction
# Using RGB & Metadata


# Load Packages, Functions, & Data==================================================================
pacman::p_load(here, tidyverse, httr, tidymodels, tidytext, e1071, skimr)

source(here("fns_objs", "00_fn-backbone.R"))

fp_art_feat <- grab_newest_fp(dir=here("data"), 
                              patt="^00_art-info-final_feat")

df_art_feat0 <- readRDS(fp_art_feat)



# Remove Row(s) with Missing Data===================================================================
df_art_feat <- df_art_feat0 %>%
  filter(object_id!=435846) 



# Data Exploration==================================================================================
## Non-public and public art
df_art_feat %>%
  count(public)
#1749 non-public and 249 public


### Get breakdown of n artworks and artists by public field and n artworks/artist
#create function
count_works_by_public <- function(df, filt_pub=TRUE) {
  df1 <- df %>%
    mutate(public=ifelse(public, "public", "non_public")) %>%
    count(artist_clean, public) %>%
    pivot_wider(names_from=public, values_from=n) %>%
    #artist must have public artworks
    {if(filt_pub) filter(., !is.na(public)) else .} %>%
    rowwise() %>%
    mutate(total=sum(non_public, public, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(desc(total), desc(public)) 
  
  return(df1)
}


#### Counts by artist
df_art_public_ns <- count_works_by_public(df_art_feat) %>% 
  filter(total>=5) #filter for artists with at least 5 artworks


#### Grouped by total artworks (know they have at least 1 public artwork)
df_art_public_ns %>%
  rename(n_artworks="total") %>%
  group_by(n_artworks) %>%
  summarize(n_artists=length(artist_clean),
            artists=paste(sort(artist_clean), collapse=", "),
            total_artworks=sum(n_artworks))


#### Overall summary data
df_art_public_ns %>% 
  summarize(n_artists=n(),
            public_total=sum(public),
            non_public_total=sum(non_public, na.rm=TRUE),
            grand_total=sum(total))
#39 artists produced 501 artworks (113 public & 388 non-public)



# Filter and Split Data=============================================================================
## Filter data using artists with at least 5 artworks 
vec_artist_clean <- df_art_feat %>%
  count(artist_clean, name="n_artworks") %>%
  filter(n_artworks >= 5) %>%
  pull(artist_clean)

df_art_feat_mod <- df_art_feat %>%
  filter(artist_clean %in% vec_artist_clean)


## Split data: train + validate and app
### Grab filtering vectors/dfs of artists based on cut-off criteria
#### At least 4 non-public artworks
vec_4plus_np <- df_art_public_ns %>%
  filter(non_public>=4) %>%
  pull(artist_clean)


#### Less than 4 non-public artworks
#df of public art ns for filtering
df_lt4_np_pub_ns <- df_art_public_ns %>%
  filter(non_public<4) %>%
  mutate(public_filt=4-non_public) %>%
  select(artist_clean, public_filt)

#vector of applicable artists
vec_lt4_np <- df_lt4_np_pub_ns %>%
  pull(artist_clean)

#get object ids for filtering (for public artworks)
df_lt4_np_pub_filt <- df_art_feat %>%
  #filter by artist and public
  filter(artist_clean %in% vec_lt4_np,
         public) %>%
  select(artist_clean, object_id, public) %>%
  #sample by numbers joined in
  left_join(df_lt4_np_pub_ns, by="artist_clean") %>%
  group_by(artist_clean) %>%
  mutate(object_ids=paste(sample(object_id, unique(public_filt)),
                             collapse=", ")) %>%
  ungroup() %>%
  select(artist_clean, object_ids) %>%
  distinct() %>%
  #move object_ids into separate rows
  separate_longer_delim(object_ids, ", ") %>%
  rename(object_id="object_ids") %>%
  mutate(object_id=as.integer(object_id))


## Filter data 
set.seed(131)

### Artist with at least 4 non-public artworks
df_4plus_np <- df_art_feat %>%
  filter(artist_clean %in% vec_4plus_np,
         !public)
  

### Artists with less than 4 non-public artworks (but 5+ total)
#get non-public artworks
df_lt4_np_np <- df_art_feat %>%
  filter(artist_clean %in% df_lt4_np_pub_filt$artist_clean,
         !public)

#get public artworks
df_lt4_np_pub <- df_art_feat %>%
  semi_join(df_lt4_np_pub_filt, 
            by=c("object_id", "artist_clean"))


### Combine DFs for train-validation set & determine app set
df_train0 <- df_4plus_np %>% #4+ np artists (non-public)
  bind_rows(df_lt4_np_np, #< 4 np artists (non-public)
            df_lt4_np_pub) #< 4 np artists (public)

df_app0 <- df_art_feat %>%
  filter(artist_clean %in% df_art_public_ns$artist_clean) %>%
  anti_join(df_train0, by="object_id")


### Check counts
dim(df_train0); dim(df_app0) #397 and 104 rows

count_works_by_public(df_train0, filt_pub=FALSE) %>%
  print(n=40)
#every artist with at least 4 artworks
#every artist has np art retained
#public art retained by artists with < 4 np art

df_app0 %>%
  count(artist_clean, public, sort=TRUE) %>%
  print(n=40)
#39 artists, public art only, counts vary from 1-10



# Feature Engineering: Training Data================================================================
cols_mod <- c("object_id", "artist_clean", "public", "date_start", "date_end", "medium")


## Title features
### Get TF-IDF values
df_train_tfidf <- df_train0 %>%
  select(object_id, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>% 
  group_by(word) %>%
  mutate(n_title=n_distinct(object_id)) %>%
  ungroup() %>%
  arrange(desc(n_title)) %>% 
  filter(n_title>=2) %>% #word must appear in 2+ titles
  select(!n_title) %>% #drops 89 more object ids
  count(object_id, word, sort=TRUE) %>%
  bind_tf_idf(word, object_id, n) %>%
#NOTE: 673 rows (obj_id-word) but only 186 words (no need to reduce further)
  select(object_id, word, tf_idf) %>%
  pivot_wider(id_cols="object_id", names_from="word", values_from="tf_idf") %>%
  full_join(df_train0 %>% select(object_id),
            by="object_id") %>%
  mutate(across(!object_id, ~replace_na(.x, 0))) 


### Extract and save vocabulary (for filtering app TF-IDF vocab)
train_vocab <- df_train_tfidf %>%
  select(!object_id) %>%
  names()


## Dimension features
### Extract height, width, whether painted surface (or overall) & multi (T/F) from dimensions
df_train_dims <- calc_single_dims(df_train0) %>%
  bind_rows(
    calc_multi_dims(df_train0)
  )
  
df_app_dims <- calc_single_dims(df_app0) %>%
  bind_rows(
    calc_multi_dims(df_app0)
  )

#check for any dupes
duplicated(df_train_dims$object_id) %>% sum() #0
duplicated(df_app_dims$object_id) %>% sum() #0
duplicated(c(df_train_dims$object_id, 
             df_app_dims$object_id)) %>% sum() #0


### Extract shape & combine with other dim features
df_train_dims_plus <- df_train0 %>%
  extract_shape(df_train_dims)


## All metadata features
df_train_meta <- df_train0 %>% 
  select(all_of(cols_mod)) %>% 
  mutate(
    #date features
    creation_length=date_end-date_start,
    creation_length_large=creation_length>= 50,
    date_middle=(date_end + date_start)/2,
    creation_century=floor(date_middle/100) + 1,
    exact_year=date_start==date_end,
    is_pre_1800=date_middle < 1800
  ) %>% 
  #medium
  classify_medium() %>%
  mutate(has_gold=str_detect(medium, "gold"), #religious/ceremonial/medieval/renaissance/high-value
         has_parchment=str_detect(medium, "parchment"), #ancient/fragile
         is_transferred=str_detect(medium, "transferred") #restoration/alteration
  ) %>% 
  #dimension features
  left_join(df_train_dims_plus, by="object_id") %>% 
  select(!medium) %>% 
  #title features 
  left_join(df_train_tfidf, by="object_id") %>%
  #create factors
  mutate(across(c(artist_clean, medium_group, shape), ~as.factor(.x)))


## RGB data
df_train_rgb <- df_train0 %>%
  select(object_id, feature_vector) %>%
  mutate(rgb_stats=map(feature_vector, extract_rgb_features),
         rgb_bins=map(feature_vector, extract_rgb_bins)) %>%
  unnest(rgb_stats) %>%
  unnest(rgb_bins) %>%
  select(!feature_vector)
      

## Combine meta and rgb
df_train_combined <- df_train_meta %>%
  left_join(df_train_rgb, by="object_id")


## Checks
### Missing values
skim(df_train_combined) #none


### Zero variance
df_train_combined %>%
  select(!c(artist_clean, medium_group, shape)) %>% #>1 value per skim()
  purrr::map_df(var) %>%
  t() %>%
  as.data.frame() %>%
  filter(V1==0)
#B_max has zero variance --> remove
  

### Duplicate features
#### Transpose the data frame and convert to character to handle numeric precision
df_train_combined_t <- df_train_combined %>%
  t() %>%
  as.data.frame()


#### Find duplicated rows in the transposed data (i.e., columns in the original)
dupe_cols_logical <- duplicated(df_train_combined_t)


#### Get the names of the duplicated columns
train_duplicate_columns <- rownames(df_train_combined_t)[dupe_cols_logical]


#### View the result
train_duplicate_columns #--> remove


#### factor level mismatch (fct_expand): artist_clean, medium_group, shape
#app DF missing Fresco in medium_group and Irregular in shape--so added


### Drop extraneous features
df_train_final <- df_train_combined %>%
  select(!c(public, B_max, all_of(train_duplicate_columns)))



# Feature Engineering: App Data=====================================================================
## Title features
df_app_tfidf <- df_app0 %>%
  select(object_id, title) %>%
  unnest_tokens(word, title) %>%
  #filter words that are in training vocab only
  filter(word %in% train_vocab) %>%
  count(object_id, word, sort=TRUE) %>%
  #reuse training idf b/c no app idf
  left_join(
    df_train_tfidf %>% 
      pivot_longer(-object_id, names_to = "word", values_to = "tf_idf") %>%
      group_by(word) %>%
      summarize(idf = max(tf_idf/max(tf_idf)), .groups = 'drop'),
    by = "word"
  ) %>%
  #calculate TF-IDF using TFs from app DF and IDFs from training DF
  mutate(tf_idf = n*idf) %>%
  select(object_id, word, tf_idf) %>%
  pivot_wider(id_cols = object_id, names_from = word, values_from = tf_idf) %>%
  full_join(df_app0 %>% select(object_id), by = "object_id") %>%
  mutate(across(!object_id, ~replace_na(.x, 0)))


## Dimension features
### Extract height, width, whether painted surface (or overall) & multi (T/F) from dimensions
#see training section


### Extract shape & combine with other dim features
df_app_dims_plus <- df_app0 %>%
  extract_shape(df_app_dims)


## All metadata features
df_app_meta <- df_app0 %>% 
  select(all_of(cols_mod)) %>% 
  mutate(
    #date features
    creation_length=date_end-date_start,
    creation_length_large=creation_length>= 50,
    date_middle=(date_end + date_start)/2,
    creation_century=floor(date_middle/100) + 1,
    exact_year=date_start==date_end,
    is_pre_1800=date_middle < 1800
  ) %>% 
  #medium
  classify_medium() %>%
  mutate(has_gold=str_detect(medium, "gold"), #religious/ceremonial/medieval/renaissance/high-value
         has_parchment=str_detect(medium, "parchment"), #ancient/fragile
         is_transferred=str_detect(medium, "transferred") #restoration/alteration
  ) %>% 
  #dimension features
  left_join(df_app_dims_plus, by="object_id") %>% 
  select(!medium) %>% 
  #title features 
  left_join(df_app_tfidf, by="object_id") %>%
  #create factors
  mutate(across(c(artist_clean, medium_group, shape), ~as.factor(.x))) %>%
  #add missing levels
  mutate(medium_group=fct_expand(medium_group, "Fresco"),
         shape=fct_expand(shape, "Irregular"))


## RGB data
df_app_rgb <- df_app0 %>%
  select(object_id, feature_vector) %>%
  mutate(rgb_stats=map(feature_vector, extract_rgb_features),
         rgb_bins=map(feature_vector, extract_rgb_bins)) %>%
  unnest(rgb_stats) %>%
  unnest(rgb_bins) %>%
  select(!feature_vector)


## Combine meta and rgb
df_app_combined <- df_app_meta %>%
  left_join(df_app_rgb, by="object_id")


## Drop extraneous features
### Find which duplicated training set words are in app set
test_duplicate_columns <- intersect(train_duplicate_columns,
                                    df_app_tfidf %>%
                                      select(!object_id) %>%
                                      names())

### Remove extraneous features
df_app_final <- df_app_combined %>%
  select(!c(public, B_max, all_of(test_duplicate_columns)))



# Save to RDS=======================================================================================
fp_train_feat <- here("data", paste0("03_train-feat_",
                                     Sys.Date(),
                                     ".rds"))

fp_app_feat <- here("data", paste0("03_app-feat_",
                                     Sys.Date(),
                                     ".rds"))

# saveRDS(df_train_final, fp_train_feat)
# saveRDS(df_app_final, fp_app_feat)

