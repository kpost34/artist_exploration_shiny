# Artist Exploration
# Objective: import public artwork metadata, find summary info, and view image(s)


# Load Packages=====================================================================================
pacman::p_load(here, tidyverse, magick, slickR)



# Test Grabbing and Rendering Picture===============================================================
## Load DF
fp_art_explore <- list.files(here("data"), "^02_art-exploration", full.names=TRUE) %>% 
  sort(decreasing=TRUE)

df_art_info_public <- readRDS(fp_art_explore)


## Find summary info of DF
df_art_info_public %>%
  count(artist_simple, nationality) %>%
  arrange(desc(n)) %>%
  filter(n>1) %>% View()


## Isolate image_url
imgs_url_vangogh <- df_art_info_public %>%
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

