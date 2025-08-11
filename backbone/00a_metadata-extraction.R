# Pull metadata from The Met for all and public artwork and save to RDS

# Load Packages and Functions & Create Obj==========================================================
pacman::p_load(here, httr, jsonlite, tidyverse)

source(here("fns_objs", "00_fn.R"))



# Public Metadata Extraction========================================================================
## Get artwork info
### Grab all relevant object IDs
vec_art_objs <- search_paintings(nationality="Europe", public=TRUE) %>% unlist()
length(vec_art_objs) #428


### Apply function with delay, skipping invalid ones
#### Do in batches of 50 objects
t_art_info_full <- list()
n <- 1

for(i in seq(1, 351, 50)){
  t_art_info <- purrr::map(vec_art_objs[i:(i+49)], function(obj_id) {
    Sys.sleep(1 + runif(1, 0, 1))
    get_artwork_info(obj_id)
  })
  Sys.sleep(30)
  t_art_info_full <- c(t_art_info_full, t_art_info)
  print(paste("Batch", n, "Completed"))
  n <- n + 1
}


#### Assess remainder (last 28 objects) & combine
t_art_info2 <- purrr::map(vec_art_objs[401:428], function(obj_id) {
  get_artwork_info(obj_id)
})

t_art_info_full <- c(t_art_info_full, t_art_info2)


### Filter out NULLs and combine
df_art_info <- t_art_info_full %>%
  compact() %>% #remove NULL entries
  bind_rows()


## Save DF
fn_art_explore <- paste0("00_art-exploration_", Sys.Date(), ".rds")
fp_art_explore <- here("data", fn_art_explore)
# saveRDS(df_art_info, fp_art_explore)



# Full Metadata Extraction==========================================================================
## Get the object IDs of possible artworks (remember that some don't have valid URLs) & split
  #into reasonable sizes for next step
### Grab all object IDs
vec_art_objs <- search_paintings(nationality="Europe", public=NULL) %>% unlist()
length(vec_art_objs) #12697


### Split up vector
#### Calculate processing time
length(vec_art_objs) * 1.5 #19045.5 sec
19045.5/60 #~317 min

length(vec_art_objs)/50 #~254 groups of 50 obj IDs

254 * 30 #7620 seconds of breaks between groups
7620/60 #127 minutes

317 + 127 #444 min
444/60 #7.4 hours


#### Split up obj id vector and save
#splitting
grp <- 1:8
indexes_end <- grp * 1750
indexes_start <- indexes_end - 1749
indexes_end[length(grp)] <- length(vec_art_objs)


t_vec_art_objs <- purrr::map2(indexes_start, indexes_end, function(x, y) {
    vec_art_objs[x:y]
}) %>%
  set_names((paste0("vec_art_obj", grp)))


#saving
# purrr::map(grp, function(x) {
#   filename <- paste0("vec_art_objs", x, ".rds")
#   filepath <- here("data", "obj_id_vecs", filename)
# 
#   vec <- t_vec_art_objs[[x]]
# 
#   saveRDS(vec, filepath)
# })


## Retain object IDs & associated info that have valid URLs and non-null values for artist name
# filenum <- 8 #completed 1-7 first

# t_obj <- loop_get_artwork_info(fileno=filenum)


## Convert to DF and save
# df_obj <- t_obj %>%
#   compact() %>% #remove NULL entries
#   bind_rows()
# 
# fname_df <- paste0("df_art_info", filenum, ".rds")
# fp_out <- here("data", "art_info_dfs", fname_df)

# saveRDS(df_obj, fp_out) #saved after each run

















