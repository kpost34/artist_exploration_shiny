# App Functions for Artist Exploration Shiny App #


# Function for building game tab of UI
build_q_a_block <- function(id, n) {
  # Create ns
  ns <- NS(id)
  
  # Create names
  nm_root <- c("out_img_art", "ui_answer_art", "txt_answer_msg_art")
  nm <- paste0(nm_root, n)
  names(nm) <- c("image", "answer", "message")
  
  # Build UI block
  tagList(
    column(2,
      imageOutput(ns(nm["image"]),
                  height="250px", 
                  width="250px"),
      uiOutput(ns(nm["answer"])),
      strong(textOutput(ns(nm["message"])))
    )
  )
}



# Function to process loaded image
process_loaded_image <- function(fp_img) {
  img_processed <- fp_img %>%
    image_read() %>%
    image_strip() %>%
    image_resize("100x100") %>%
    image_extent("100x100", gravity="center", color="white") %>%
    image_data() %>%
    as.vector() %>%
    as.integer()
  
  return(img_processed)
}



# Function to extract RGB stats and bins from image
extract_final_rgb_feat <- function(img) {
  # Create vector of features to remove
  feat_remove <- c("B_bin1", "B_mean", "G_bin1", "R_mean", "B_sd", "G_sd", "R_bin1", "R_sd",
                   "R_range", "B_max")
  
  # Extract, combine, and prune features
  df_img_feat <- bind_cols(
    object_id=0,
    extract_rgb_features(img), 
    extract_rgb_bins(img)
  ) %>%
  select(!all_of(feat_remove))
  
  return(df_img_feat)
}



# Functions to convert artist name syntax
convert_artist_name <- function(names) {
  formatted_names <- purrr::map_chr(names, format_name)
  
  return(formatted_names)  
}

format_name <- function(name){
  formatted_name <- name %>%
    str_split_1(pattern=", ") %>%
    rev() %>%
    paste(collapse=" ")
  
  return(formatted_name)
}