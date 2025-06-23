# Functions for Artist Exploration App #

## Define a function to perform the search for each nationality
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
      textOutput(ns(nm["message"]))
    )
  )
}


