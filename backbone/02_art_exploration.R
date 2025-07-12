# Artist Exploration
# Objective: using API, get sample of artist names and public domain artworks to use for
  #Artist Exploration tab


# Load Packages and Functions & Create Obj==========================================================
pacman::p_load(here, httr, jsonlite, tidyverse, magick, slickR)

source(here("fns_objs", "00_fn.R"))

fp_art_explore <- here("data", "02_art-exploration.rds")
fp_art_explore_new <- here("data", "02_art-exploration_new.rds")



# Data Extraction===================================================================================
## Get artwork info
### Grab all relevant object IDs
natls <- c("North America", "Europe")

vec_art_objs <- purrr::map(natls, search_paintings, public=TRUE) %>%
  unlist() 


### Apply function with delay, skipping invalid ones
t_art_info_full <- list()

for(i in seq(1, 601, 50)){
  t_art_info <- purrr::map(vec_art_objs[i:(i+49)], function(obj_id) {
    Sys.sleep(1 + runif(1, 0, 1))
    get_artwork_info(obj_id)
  })
  
  Sys.sleep(30)
  t_art_info_full <- c(t_art_info_full, t_art_info)
    
}


### Filter out NULLs and combine
df_art_info <- t_art_info_full %>%
  compact() %>% #remove NULL entries
  bind_rows()


## Save DF
# rm(fp_art_explore)
# saveRDS(df_art_info, fp_art_explore_new)


### Find summary info of DF
df_art_info %>%
  count(artist_simple) %>%
  arrange(desc(n)) %>%
  filter(n>1) %>% View()



# Test Grabbing and Rendering Picture===============================================================
## Load DF
df_art_info <- readRDS(fp_art_explore_new)


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









