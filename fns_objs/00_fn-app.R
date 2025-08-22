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