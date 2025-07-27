# Artist Exploration
# Objective: using API, get sample of artist names and public domain artworks to use for
  #Artist Exploration tab


# Load Packages and Functions & Create Obj==========================================================
pacman::p_load(here, httr, jsonlite, tidyverse, magick, slickR)

source(here("fns_objs", "00_fn.R"))



# Data Extraction===================================================================================
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
fn_art_explore <- paste0("02_art-exploration_", Sys.Date(), ".rds")
fp_art_explore <- here("data", fn_art_explore)
# saveRDS(df_art_info, fp_art_explore)


## Find summary info of DF
df_art_info %>%
  count(artist_simple, nationality) %>%
  arrange(desc(n)) %>%
  filter(n>1) %>% View()



# Test Grabbing and Rendering Picture===============================================================
## Load DF
fp_art_explore <- list.files(here("data"), "^02_art-exploration", full.names=TRUE) %>% 
  sort(decreasing=TRUE)
df_art_info <- readRDS(fp_art_explore)


## Isolate image_url
imgs_url_vangogh <- df_art_info %>%
  filter(artist_simple=='Vincent van Gogh') %>%
  pull(image_url)


## Using magick
### Read image
img_vangogh1 <- image_read(imgs_url_vangogh[1])


### Print image
print(img_vangogh1) 


## Using slickR
slickR(imgs_url_vangogh)
#if you open these in a browser and zoom out, they render as a carousel









