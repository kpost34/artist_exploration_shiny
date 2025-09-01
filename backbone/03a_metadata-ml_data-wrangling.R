# Artist Identification using Image & Metadata: Data Wrangling & Feature Extraction


# Load Packages, Functions, & Data==================================================================
pacman::p_load(here, tidyverse, httr, tidymodels, tidytext)

source(here("fns_objs", "00_fn-backbone.R"))

fp_art_feat <- grab_newest_fp(dir=here("data"), 
                              patt="^00_art-info-final_feat")

df_art_feat0 <- readRDS(fp_art_feat)



# Remove Row(s) with Missing Data===================================================================
artist_incomplete <- df_art_feat0 %>% 
  filter(object_id==435846) %>% 
  pull(artist_clean)

df_art_feat <- df_art_feat0 %>%
  filter(artist_clean!=artist_incomplete) 



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
    {if(filt_pub) filter(., !is.na(public)) else .} %>%
    rowwise() %>%
    mutate(total=sum(non_public, public, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(desc(total), desc(public)) 
  
  return(df1)
}


#### Counts by artist
df_art_public_ns <- count_works_by_public(df_art_feat) %>% 
  filter(total>=5)

# df_art_public_ns <- df_art_feat %>%
#   mutate(public=ifelse(public, "public", "non_public")) %>%
#   count(artist_clean, public) %>%
#   pivot_wider(names_from=public, values_from=n) %>%
#   #need to have public artworks for app
#   filter(!is.na(public)) %>%
#   rowwise() %>%
#   mutate(total=sum(non_public, public, na.rm=TRUE)) %>%
#   ungroup() %>%
#   arrange(desc(total), desc(public)) %>% 
#   #use threshold of 5 artworks total
#   filter(total>=5) 


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
  


# Filter and Split Data=============================================================================
## Filter data using artists with at least 5 artworks each
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

#vector of artists
vec_lt4_np <- df_lt4_np_pub_ns %>%
  pull(artist_clean)

#get object ids for filtering
df_lt4_np_pub_filt <- df_art_feat %>%
  filter(artist_clean %in% vec_lt4_np,
         public) %>%
  select(artist_clean, object_id, public) %>%
  left_join(df_lt4_np_pub_ns, by="artist_clean") %>%
  group_by(artist_clean) %>%
  mutate(object_ids=paste(sample(object_id, unique(public_filt)),
                             collapse=", ")) %>%
  ungroup() %>%
  select(artist_clean, object_ids) %>%
  distinct() %>%
  separate_longer_delim(object_ids, ", ") %>%
  rename(object_id="object_ids") %>%
  mutate(object_id=as.integer(object_id))


### Filter data 
set.seed(131)

### Artist with at least 4 non-public artworks
df_4plus_np <- df_art_feat %>%
  filter(artist_clean %in% vec_4plus_np,
         !public)
  

### Artists with less than 4 non-public artworks (but 5+ total)
df_lt4_np_np <- df_art_feat %>%
  filter(artist_clean %in% df_lt4_np_pub_filt$artist_clean,
         !public)

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



# Feature Engineering===============================================================================
cols_mod <- c("object_id", "artist_clean", "date_start", "date_end", "medium",
              "dimensions", "feature_vector", "public")


## Medium
### Create function
classify_medium <- function(df) {
  # Create mixed medium vector
  medium_mixed <- c("Tempera, oil, and gold on wood", 
                    "Tempera and gold on canvas, transferred from wood",
                    "Oil on panel, transferred to canvas", 
                    "Tempera and oil on wood")
  
  # Categorize medium values into larger groups
  df1 <- df %>%
    mutate(medium_group=case_when(
      medium=="Oil on copper"                                ~ "Oil on metal",
      medium=="Fresco, transferred to canvas"                ~ "Fresco",
      medium %in% medium_mixed                               ~ "Mixed or Other",
      str_detect(medium, "^Oil.*on (paper|parchment)")       ~ "Oil on paper",
      str_detect(medium, "^Oil.*on canvas")                  ~ "Oil on canvas",
      str_detect(medium, "^Oil.*on (wood|oak|beech|linden)") ~ "Oil on wood",
      str_detect(medium, "^Tempera")                         ~ "Tempera on wood",
      str_detect(medium, "^Mixed media")                     ~ "Mixed or Other",
      TRUE                                                   ~ "NEEDS CATEGORY")
    )
  
  return(df1)
}



## Title features
df_train_tfidf <- df_train0 %>%
  select(object_id, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>% #drops 436168
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


## Dimension features
### Functions
#extract width
grab_width <- function(string, special=FALSE, num=FALSE) {
  if(!special) {
    width <- str_extract(string, "[0-9.]{1,}(?= cm\\))") 
  } else if(special) {
    width <- str_extract(string, "(?<=([Pp]archment|[Pp]ainted [Ss]urface)).+") %>%
    # width <- str_remove(dimensions, ".+ painted surface") %>%
      str_extract("[0-9.]{1,}(?= cm\\))") 
  }
  
  if(num) {
    width <- as.numeric(width)
  }
  return(width)
}

#extract height
grab_height <- function(string, special=FALSE, num=FALSE) {
  if(!special) {
    height <- str_extract(string, "[0-9.]{1,}(?= [x|\u00d7] [0-9.]{1,} cm\\))")
  } else if(special){
    height <- str_extract(string, "(?<=([Pp]archment|[Pp]ainted [Ss]urface)).+") %>%
      str_extract("[0-9.]{1,}(?= [x|\u00d7] [0-9.]{1,} cm\\))") 
  }
  
  if(num) {
    height <- as.numeric(height)
  }
  return(height)
}



#height and width of multi-piece paintings (both sets)
calc_multi_dims <- function(df) {
  df1 <- df %>%
    select(object_id, dimensions) %>%
    filter(str_detect(dimensions, "Each|wing|Angel|panel|\\(a\\)")) %>%
    mutate(
      each=str_detect(dimensions, "[Ee]ach"),
      dim_each=ifelse(each, 
                      str_extract(dimensions, "(?<=[Ee]ach).+\\)") %>%
                        str_extract("\\(.+?cm\\)"), NA_character_),
      dim_extract=ifelse(str_detect(dimensions, "[Pp]ainted surface"),
                         str_extract_all(dimensions, "(?<=[Pp]ainted surface).+?\\)"),
                         str_extract_all(dimensions, "(?<= in).+?\\)"))
    ) %>% 
    select(!each) %>%
    unnest(cols=dim_extract) %>% 
    mutate(dim_extract=str_extract(dim_extract, "\\(.+?cm\\)")) %>% 
    mutate(dim_extract=paste(dim_extract, dim_each, sep="_")) 
  
  df2 <- df1 %>%
    bind_rows(
      df1 %>%
        select(object_id, dimensions, dim_extract="dim_each") %>%
        filter(!is.na(dim_extract)) %>%
        distinct()
    ) %>%
    select(!dim_each) %>%
    mutate(dim_extract=str_replace(dim_extract, "x(?=[0-9])", "x "),
           height_cm=grab_height(dim_extract, num=TRUE),
           width_cm=grab_width(dim_extract, num=TRUE)) %>%
    group_by(object_id, dimensions) %>%
    summarize(height_cm=max(height_cm),
              width_cm=sum(width_cm)) %>%
    ungroup() %>%
    mutate(
      multi_piece=TRUE,
      overall=str_detect(dimensions, "[Oo]verall"),
      ps=str_detect(dimensions, "[Pp]ainted|[Pp]archment"),
      painted_surface=ps|(!ps & !overall)
    ) %>%
    select(object_id, height_cm, width_cm, painted_surface, multi_piece)
  
  return(df2)
  
}


#height and width of single piece works
calc_single_dims <- function(df) {
  df1 <- df %>%
    select(object_id, dimensions) %>% 
    #remove multi-piece works
    filter(!str_detect(dimensions, "Each|wing|Angel|panel|\\(a\\)")) %>%
    #note: painted_surface here is just for filtering as it's missing examples with 1 set of dims
    mutate(
      n_sets=str_count(dimensions, " x | \u00d7 ")/2,
      diameter=str_detect(dimensions, "[Dd]iameter"),
      overall=str_detect(dimensions, "[Oo]verall"),
      ps=str_detect(dimensions, "[Pp]ainted|[Pp]archment")
    ) %>%
    mutate(
      dimensions=str_replace_all(dimensions, "(?<=[0-9]{1})cm", " cm"),
      height_cm=case_when(
        (n_sets==0|diameter) & !ps  ~ grab_width(dimensions),
        (n_sets==0|diameter) & ps   ~ grab_width(dimensions, special=TRUE),
        n_sets==1|n_sets==1.5       ~ grab_height(dimensions), #ps or !ps is the same
        n_sets==2 & !ps             ~ grab_height(dimensions),
        n_sets==2 & ps              ~ grab_height(dimensions, special=TRUE),
        TRUE                        ~ "NEEDS CATEGORY"),
      width_cm=case_when(
        (n_sets==0|diameter) & !ps  ~ grab_width(dimensions),
        (n_sets==0|diameter) & ps   ~ grab_width(dimensions, special=TRUE),
        n_sets==1|n_sets==1.5       ~ grab_width(dimensions), #ps or !ps is the same
        n_sets==2 & !ps             ~ grab_width(dimensions),
        n_sets==2 & ps              ~ grab_width(dimensions, special=TRUE),
        TRUE                        ~ "NEEDS CATEGORY")
    ) %>%
    mutate(across(c(height_cm, width_cm), ~as.integer(.x)),
           multi_piece=FALSE,
           painted_surface=ps|(!ps & !overall)) %>%
    select(object_id, height_cm, width_cm, painted_surface, multi_piece)
  
  return(df1)
}



### Extract height, width, whether painted surface (or overall) & multi (T/F) from dimensions
df_train_dims <- calc_single_dims(df_train0) %>%
  bind_rows(
    calc_multi_dims(df_train0)
  )
  

df_app_dims <- calc_single_dims(df_app0) %>%
  bind_rows(
    calc_multi_dims(df_app0)
  )





### Extract shape and whether only overall size given


df_train0 %>% 
  mutate(
    diameter=str_detect(dimensions, "[Dd]iameter"),
    shape=case_when(
      str_detect(dimensions, "[Dd]iameter|[Rr]ound")          ~ "Circle",
      !diameter & height_cm==width_cm                         ~ "Square",
      str_detect(dimensions, "[Ii]rregular [Oo]val|[Oo]val")  ~ "Oval",
      str_detect(dimensions, "[Ii]rregular")                  ~ "Irregular",
      str_detect(dimensions, "[Ss]hape|[Aa]rched")            ~ "Arched",
      TRUE                                                    ~ "Rectangle")
)





  #multi need special function [9]
  #n_sets==0 or diameter==TRUE & ps==FALSE then take value within () and use as height and width [4]
  #n_sets==0 or diameter==TRUE & ps==TRUE then take value within () after [Pp]ainted surface [3
  #n_sets==1 and ps==TRUE then take values within () [6]
  #n_sets==1 and ps==FALSE then take values with () and use as height and width [if contains overall 
    #then painted_surface=FALSE] [420]
  #n_sets==1.5 then use values in () with cm [1]
  #n_sets==2 and ps==FALSE then use first set in () [17]
  #n_sets==2 and ps==TRUE then use second set in () after '[Pp]ainted surface' [41]

  

df_app0 %>%
  select(object_id, dimensions) %>% 
  #note: painted_surface here is just for filtering as it's missing ex with 1 set of dims
  mutate(
    n_sets=str_count(dimensions, " x | \u00d7 ")/2,
    diameter=str_detect(dimensions, "[Dd]iameter"),
    overall=str_detect(dimensions, "[Oo]verall"),
    ps=str_detect(dimensions, "[Pp]ainted|[Pp]archment"),
    multi=str_detect(dimensions, "Each|wing|\\(a\\)")) %>% View()

  #take dims if one set provided
  #if painted surface then use that set
  #if multiple parts (wings) (or a-d), use max height and sum of widths
  #ignore 'with added strips' and framed dims
  #if only an overall or framed dim provided, use that
  
  
  #other fields:
  #painted_surface: most TRUE but FALSE if only Overall, Framed, with Strips dims provided
  #contain_multiple_parts: TRUE/FALSE
  #n_parts = 1, 2, 3, etc



  
df_train0 %>% #select(medium) %>% distinct(medium) %>% print(n=40)
  select(all_of(cols_mod)) %>%
  #title features (via join)
  left_join(df_train_tfidf, by="object_id") %>%
  mutate(
    #dimension features
    
    #date features
    date_middle=(date_end - date_start)/2,
    creation_length=date_end - date_start,
    creation_century=floor(date_middle/100) + 1,
    exact_year=date_start==date_end,
    is_pre_1800=date_middle < 1800) %>%
  #medium
  classify_medium() %>%
  mutate(has_gold=str_detect(medium, "gold"), #religious/ceremonial/medieval/renaissance/high-value
         has_parchment=str_detect(medium, "parchment"), #ancient/fragile
         is_transferred=str_detect(medium, "transferred") #restoration/alteration
    
    distinct(medium_group, medium) %>% 
    arrange(medium_group) %>%
    print(n=40)

      
      
    







# Archive (will delete)=============================================================================

df_multi_tmp <- df_app0 %>%
  # df_multi_tmp <- df_train0 %>%
  # bind_rows(df_app0, .id="set") %>%
  # mutate(set=ifelse(set=="1", "train", "app")) %>%
  select(object_id, dimensions) %>%
  # select(set, object_id, dimensions) %>%
  filter(str_detect(dimensions, "Each|wing|Angel|panel|\\(a\\)")) %>%
  mutate(
    each=str_detect(dimensions, "[Ee]ach"),
    dim_each=ifelse(each, 
                    str_extract(dimensions, "(?<=[Ee]ach).+\\)") %>%
                      str_extract("\\(.+?cm\\)"), NA_character_),
    dim_extract=ifelse(str_detect(dimensions, "[Pp]ainted surface"),
                       str_extract_all(dimensions, "(?<=[Pp]ainted surface).+?\\)"),
                       str_extract_all(dimensions, "(?<= in).+?\\)"))
  ) %>% 
  select(!each) %>%
  unnest(cols=dim_extract) %>% 
  mutate(dim_extract=str_extract(dim_extract, "\\(.+?cm\\)")) %>% 
  mutate(dim_extract=paste(dim_extract, dim_each, sep="_")) 


df_multi2 <- df_multi_tmp %>%
  bind_rows(
    df_multi_tmp %>%
      select(object_id, dimensions, dim_extract="dim_each") %>%
      # select(set, object_id, dimensions, dim_extract="dim_each") %>%
      filter(!is.na(dim_extract)) %>%
      distinct()
  ) %>%
  select(!dim_each) %>%
  mutate(dim_extract=str_replace(dim_extract, "x(?=[0-9])", "x "),
         height_cm=grab_height(dim_extract, num=TRUE),
         width_cm=grab_width(dim_extract, num=TRUE)) %>%
  group_by(object_id, dimensions) %>%
  # group_by(set, object_id) %>%
  summarize(height_cm=max(height_cm),
            width_cm=sum(width_cm)) %>%
  ungroup() %>%
  mutate(
    overall=str_detect(dimensions, "[Oo]verall"),
    ps=str_detect(dimensions, "[Pp]ainted|[Pp]archment"),
    painted_surface=ps|(!ps & !overall)
  ) %>%
  select(object_id, height_cm, width_cm, multi, painted_surface)


df_app0 %>%
  # df_train0 %>%
  # bind_rows(df_app0) %>%
  select(object_id, dimensions) %>% 
  #remove multi-piece works
  filter(!str_detect(dimensions, "Each|wing|Angel|panel|\\(a\\)")) %>%
  #note: painted_surface here is just for filtering as it's missing examples with 1 set of dims
  mutate(
    n_sets=str_count(dimensions, " x | \u00d7 ")/2,
    diameter=str_detect(dimensions, "[Dd]iameter"),
    overall=str_detect(dimensions, "[Oo]verall"),
    ps=str_detect(dimensions, "[Pp]ainted|[Pp]archment")
  ) %>%
  mutate(
    dimensions=str_replace_all(dimensions, "(?<=[0-9]{1})cm", " cm"),
    height_cm=case_when(
      (n_sets==0|diameter) & !ps  ~ grab_width(dimensions),
      (n_sets==0|diameter) & ps   ~ grab_width(dimensions, special=TRUE),
      n_sets==1|n_sets==1.5       ~ grab_height(dimensions), #ps or !ps is the same
      n_sets==2 & !ps             ~ grab_height(dimensions),
      n_sets==2 & ps              ~ grab_height(dimensions, special=TRUE),
      TRUE                        ~ "NEEDS CATEGORY"),
    width_cm=case_when(
      (n_sets==0|diameter) & !ps  ~ grab_width(dimensions),
      (n_sets==0|diameter) & ps   ~ grab_width(dimensions, special=TRUE),
      n_sets==1|n_sets==1.5       ~ grab_width(dimensions), #ps or !ps is the same
      n_sets==2 & !ps             ~ grab_width(dimensions),
      n_sets==2 & ps              ~ grab_width(dimensions, special=TRUE),
      TRUE                        ~ "NEEDS CATEGORY")
  ) %>%
  select(object_id, height_cm, width_cm) -> df_app_tmp






