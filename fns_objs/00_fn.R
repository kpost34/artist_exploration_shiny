# Functions for Artist Exploration App #

# Backbone/Model-Building Functions=================================================================
## Function to perform the search by nationality
search_paintings <- function(nationality, public=NULL) {
  search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/search"

  query <- list(
    q = "painting",
    type = "painting",
    artistNationality = nationality,
    isPublicDomain = public
  )

  response <- GET(search_url, query = query)

  if (status_code(response) == 200) {
    data <- content(response, as = "parsed", encoding = "UTF-8")

    if (!is.null(data$objectIDs)) {
      return(data$objectIDs)
    } else {
      message("No matching objects found.")
      return(NULL)
    }
  } else {
    message("Search request failed with status: ", status_code(response))
    return(NULL)
  }
}

tmp <- search_paintings('Europe', public = TRUE)
length(tmp)


## Function to grab artist info if all info present
get_artwork_info <- function(object_id) {
  # Append object_id to url
  object_url <- paste0("https://collectionapi.metmuseum.org/public/collection/v1/objects/", object_id)
  
  tryCatch({
    response <- GET(object_url, add_headers("User-Agent" = "R-client/1.0"))
    # First possible error
    if (status_code(response) != 200) {
      message("HTTP error for object ID: ", object_id, " - Status code: ", status_code(response))
      return(NULL)
    }

    # Grab object info
    obj_data <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON()

    title <- obj_data$title
    artist <- obj_data$artistDisplayName
    nationality <- obj_data$artistNationality
    bio <- obj_data$artistDisplayBio
    date <- obj_data$objectDate
    date_start <- obj_data$objectBeginDate
    date_end <- obj_data$objectEndDate
    medium <- obj_data$medium
    dimensions <- obj_data$dimensions
    period <- obj_data$period
    classification <- obj_data$classification
    image_url <- obj_data$primaryImage
    public <- obj_data$isPublicDomain

    # Check that artist and url info present
    if(is.null(artist) || artist == "" || is.null(image_url) || image_url == "") {
      message("Invalid data for object ID: ", object_id)
      return(NULL)
    } 

    # Put info into a DF
    tibble(
      object_id = object_id,
      title = title,
      artist_simple = artist,
      nationality = nationality,
      bio = bio,
      date = date,
      date_start = date_start,
      date_end = date_end,
      medium = medium,
      dimensions = dimensions,
      period = period,
      classification = classification,
      image_url = image_url,
      public = public
    )
  },
  error = function(e) {
    message("Error processing object ID: ", object_id, " - ", e$message)
    return(NULL)
  })
}



## Functions to convert artist name syntax
format_name <- function(name){
  formatted_name <- name %>%
    str_split_1(pattern=", ") %>%
    rev() %>%
    paste(collapse=" ")
  
  return(formatted_name)
}


convert_artist_name <- function(names) {
  formatted_names <- purrr::map_chr(names, format_name)
  
  return(formatted_names)  
}



# App-related Functions=============================================================================
## Function for building game tab of UI
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


