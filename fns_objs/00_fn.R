# Functions for Artist Exploration App #

# Backbone/Model-Building Functions=================================================================
## Function to perform the search for each nationality
search_paintings <- function(nationality, public=NULL) {
  
  search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/search"
  
  query <- list(
    q = "painting",                   #filter for paintings
    type = "Painting",                #specify the type to be "Painting"
    artistNationality = nationality,  #filter by specific nationality
    artistRole = "Artist",             #filter for painters (artists)
    isPublicDomain = public           #optional: Only include public domain artworks
  )
  
  ## Make the API request with the specified query parameters
  search_response <- GET(search_url, query = query)
  
  ## Check if the request was successful
  if(status_code(search_response) == 200) {
    #parse the response content
    search_content <- content(search_response, as = "text", encoding = "UTF-8")
    search_data <- fromJSON(search_content)
    
    #extract the object IDs from the search response
    object_ids <- search_data$objectIDs
    
    #check if any object IDs were found
    if(length(object_ids) > 0) {
      # print(paste("Found", length(object_ids), "paintings by", nationality, "artists."))
      return(object_ids)  #return the object IDs
    } else {
      print(paste("No matching objects found for", nationality, "artists."))
      return(NULL)
    }
  } else {
    print(paste("Search request failed with status:", status_code(search_response)))
    return(NULL)
  }
}


## Function to grab artist info if all info present
get_artwork_info <- function(object_id) {
  object_url <- paste0("https://collectionapi.metmuseum.org/public/collection/v1/objects/", object_id)
  
  tryCatch({
    response <- GET(object_url, add_headers("User-Agent" = "R-client/1.0"))
    
    if (status_code(response) != 200) {
      message("HTTP error for object ID: ", object_id, " - Status code: ", status_code(response))
      return(NULL)
    }

    obj_data <- content(response, as = "text", encoding = "UTF-8") %>% fromJSON()

    title <- obj_data$title
    artist <- obj_data$artistDisplayName
    date <- obj_data$objectDate
    image_url <- obj_data$primaryImage

    if (is.null(title) || title == "" || is.null(artist) || artist == "" || is.null(image_url) || image_url == "") {
      message("Invalid data for object ID: ", object_id)
      return(NULL)
    }

    tibble(
      object_id = object_id,
      title = title,
      artist_simple = artist,
      date = date,
      image_url = image_url
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


