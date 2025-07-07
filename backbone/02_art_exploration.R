# Artist Exploration
# Objective: using API, get sample of artist names and public domain artworks to use for
  #Artist Exploration tab


# Load Packages and Functions & Create Obj==========================================================
pacman::p_load(here, httr, jsonlite, tidyverse, magick)

source(here("fns_objs", "00_fn.R"))

fp_art_explore <- here("data", "02_art-exploration.rds")



# Data Extraction===================================================================================
## Get artwork info--------------------
### Grab all relevant object IDs
natls <- c("North America", "Europe")

vec_art_objs <- purrr::map(natls, search_paintings, public=TRUE) %>%
  unlist() 


### Apply function with delay, skipping invalid ones
t_art_info <- map(vec_art_objs[1:10], function(obj_id) {
  Sys.sleep(1 + runif(1, 0, 1))
  get_artwork_info(obj_id)
})


### Filter out NULLs and combine
df_art_info <- t_art_info %>%
  compact() %>% #remove NULL entries
  bind_rows()


## Example
url_art <- df_art_info[2,]$image_url
image_read(url_art) %>% print()



## Save DF
# saveRDS(df_art_info, fp_art_explore)




