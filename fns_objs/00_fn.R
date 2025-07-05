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


## Function to validate artwork (non-null values for artist & artwork)
validate_obj_id <- function(object_id) {
  object_url <- paste0("https://collectionapi.metmuseum.org/public/collection/v1/objects/", object_id)
  
  tryCatch({
    # Add User-Agent to prevent 403
    object_response <- GET(object_url, add_headers("User-Agent" = "R-client/1.0"))
    
    # Check if request succeeded
    if (status_code(object_response) == 200) {
      obj_text <- content(object_response, as = "text", encoding = "UTF-8")
      object_data <- fromJSON(obj_text)
      
      # Basic field validation
      title <- object_data$title
      artist <- object_data$artistDisplayName
      image_url <- object_data$primaryImage
      
      if (is.null(title) || nchar(title) == 0 ||
          is.null(artist) || nchar(artist) == 0 ||
          is.null(image_url) || nchar(image_url) == 0) {
        print(paste("Invalid data for object ID:", object_id))
        return(NA_integer_)
      } else {
        return(object_id)
      }
    } else {
      print(paste("HTTP error for object ID:", object_id, "- Status code:", status_code(object_response)))
      return(NA_integer_)
    }
  },
  error = function(e) {
    print(paste("Error processing object ID:", object_id, "-", e$message))
    return(NA_integer_)
  },
  warning = function(w) {
    print(paste("Warning processing object ID:", object_id, "-", w$message))
    return(NA_integer_)
  })
}




# validate_obj_id <- function(object_id){
#   search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/objects/"
#   tryCatch({
#     # Fetch object details using the object ID
#     object_url <- paste0(search_url, object_id)
#     object_response <- GET(object_url, add_headers("User-Agent" = "R-client/1.0"))
#   
#     # Check if the response was successful
#     if(status_code(object_response) == 200) {
#       object_data <- fromJSON(content(object_response, as = "text"))
#   
#       # Extract metadata
#       title <- object_data$title
#       artist <- object_data$artistDisplayName
#       image_url <- object_data$primaryImage
#       
#       # Escape condition
#       if(is.null(title)|nchar(title)==0|is.null(artist)|nchar(artist)==0|is.null(image_url)){
#         print(paste("Failed to fetch data for object ID:", object_id))
#         return(NA_integer_)
#       } else{
#         return(object_id)
#       }
#     } else{
#       print(paste("HTTP error for object ID:", object_id, "- Status code:", status_code(object_response)))
#       return(NA_integer_) # Added this return statement
#     }
#   },
#   error = function(e) {
#     print(paste("Error processing object ID:", object_id, "-",
#                 e$message))
#     return(NA_integer_)
#   },
#   warning = function(w) {
#     print(paste("Warning processing object ID:", object_id, "-", w$message))
#     return(NA_integer_)
#   })
#   
# }


## Function to get information associated with an object ID
get_artwork_info <- function(object_id) {
  search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/objects/"
  
  tryCatch({
    # Fetch object details using the object ID
    object_url <- paste0(search_url, object_id)
    object_response <- GET(object_url)
  
    # Check if the response was successful
    if(status_code(object_response) == 200) {
      object_data <- fromJSON(content(object_response, as = "text"))
  
      # Extract metadata
      title <- object_data$title
      artist <- object_data$artistDisplayName
      image_url <- object_data$primaryImage
      
      # Escape condition
      if(is.null(title)|nchar(title)==0|is.null(artist)|nchar(artist)==0|is.null(image_url)){
        print(paste("Failed to fetch data for object ID:", object_id))
        # return(NULL)
      } else{
        data <- list(title = title, artist = artist, image_url = image_url)
        return(data)
        
      }
    }
  },
  error = function(e) {
    print(paste("Error processing object ID:", object_id, "-",
                e$message))
    # return(NULL)
  },
  warning = function(w) {
    print(paste("Warning processing object ID:", object_id, "-", w$message))
    # return(NULL)
  })
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


