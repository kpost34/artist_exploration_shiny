# Feature extraction and combination with final, clean metadata


# Load Packages, Functions, and Data=================================================================
pacman::p_load(here, tidyverse, httr, magick)

source(here("fns_objs", "00_fn.R"))

fp_art <- grab_newest_fp(dir=here("data"), patt="^00_art-info-final")

df_art0 <- readRDS(fp_art)


# Image Processing==================================================================================
## Hard-coded example
### Read an image
img <- image_read(df_art0$image_url[1])

### Resize the image to 100x100 pixels
img_resized <- image_resize(img, "100x100")

### Convert to grayscale
img_gray <- image_convert(img_resized, colorspace = "gray")

### Convert to a matrix (pixel intensities)
img_matrix <- as.integer(image_data(img_gray))


## Functionalized example
process_image_to_vector("www.aol.com") #returns NULL
feat_vec <- process_image_to_vector(df_art0$image_url[1])
length(feat_vec) #returns 30000


## Process images
### Create sorted, subsetted DF
df_objID_url <- df_art0 %>%
  arrange(object_id) %>%
  select(object_id, image_url)


### Batch-process url jpg files
# batch_image_processing(df_objID_url, start=1, n=200)
#NOTE: done 200 at a time from 1-2000 then the last 20


# Import and Combine DFs============================================================================
## Combine individual feature vectors
df_img_feat_vec <- list.files(here("data", "img_feat_vec_dfs"), full.names=TRUE) %>%
  purrr::map_df(readRDS)


## Merge feature vector DF with metadata DF
df_art <- df_art0 %>%
  inner_join(df_img_feat_vec, by="object_id") 



# Save DFs==========================================================================================
## Feature vector DF
fname_img_feat_vec <- paste0("00_art-img-feat-vec_", Sys.Date(), ".rds")
fp_img_feat_vec <- paste(here("data", fname_img_feat_vec))
# saveRDS(df_img_feat_vec, fp_img_feat_vec)


## Feature vector + metadata DF
fname_art_full <- paste0("00_art-info-final_feat-vec_", Sys.Date(), ".rds")
fp_art_full <- paste(here("data", fname_art_full))
# saveRDS(df_art, fp_art_full)




