# Connect to The Met's free API, view images of artwork, and gather metadata

# Load Packages=====================================================================================
pacman::p_load(httr, jsonlite, magick)



# Make a request to the API=========================================================================
## Define the API URL
url <- "https://collectionapi.metmuseum.org/public/collection/v1/objects"


## Make the GET request
response <- GET(url, add_headers("User-Agent" = "R-client/1.0"))


## Check if the request was successful
if (status_code(response) == 200) {
  content <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(content)
  print(list(total = data$total, objectIDs = data$objectIDs[1:10]))
} else {
  print(paste("Request failed with status:", status_code(response)))
}



# Retrieve and View Artwork=========================================================================
## Search for paintings by Van Gogh
#create objects
search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/search"
query <- list(q = "Van Gogh", type = "Painting")
search_response <- GET(search_url, query = query)

#check if the request was successful
if(status_code(search_response) == 200) {
  tryCatch({
    #parse the response content
    search_content <- content(search_response, as = "text", encoding = "UTF-8")
    search_data <- fromJSON(search_content)
    
    #extract object IDs from the search response
    object_ids <- search_data$objectIDs
    
    #create empty lists to store images and metadata
    image_list <- list()
    metadata_list <- list()
    
    for (i in 1:min(5, length(object_ids))) {  #limit to 10 images for this example
      object_id <- object_ids[i]
      object_url <- paste0("https://collectionapi.metmuseum.org/public/collection/v1/objects/", object_id)
      
      object_response <- GET(object_url)
      
      if(status_code(object_response) == 200) {
        object_content <- content(object_response, as = "text", encoding = "UTF-8")
        object_data <- fromJSON(object_content)
        
        # Extract the image URL
        image_url <- object_data$primaryImage
        
        if (!is.null(image_url)) {
          #attempt to read the image with error handling
          tryCatch({
            #store the image
            img <- image_read(image_url)
            image_list[[i]] <- img
          }, error=function(e) {
            print(paste("Error reading image for object ID:", object_id, "-",
                        e$message))
            image_list[[i]] <- NULL #store NULL if image reading fails
          })
        } else {
          image_list[[i]] <- NULL
        }
        
        #extract metadata
        metadata <- list(
          #key artwork info
          title=object_data$title,
          date=object_data$date,
          #artist-related info
          artist=object_data$artistDisplayName,
          role=object_data$artistRole,
          nationality=object_data$artistNationality,
          bio=object_data$artistDisplayBio,
          #movement/period-related info
          period=object_data$period,
          culture=object_data$culture,
          classification=object_data$classification,
          style=object_data$style,
          #additional metadata
          dimensions=object_data$dimensions,
          medium=object_data$medium,
          description=object_data$description
        )
        
        #store metadata
        metadata_list[[i]] <- metadata
        
      } else {
        print(paste("Object request failed with status:", status_code(object_response)))
      }
    }
  },
  error = function(e) {
    print(paste("Error processing object ID:", object_id, "-",
                e$message))
    return(NULL)
  },
  warning = function(w) {
    print(paste("Warning processing object ID:", object_id, "-", w$message))
    return(NULL)
  })
}


## Results of search
length(image_list) #5 Van Gogh paintings
image_list[[1]]
metadata_list[[1]]



# Assess Available Art for App======================================================================
## Define the search URL for the Metropolitan Museum API
search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/search"

## Define a function to perform the search for each nationality
search_art <- function(nationality, public=NULL) {
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


## Example: Find paintings by NA and European artists
north_american_artworks <- search_art("North America") #8869 (as of 6/15/25)
european_artworks <- search_art("Europe") #12,418 (as of 6/15/25)

#combine the results (if you want to combine them)
all_object_ids <- c(north_american_artworks, european_artworks)
print(paste("Total paintings found:", length(all_object_ids))) #21,314 (as of 6/15/25)


## Here's a breakdown of artworks based on continent and domain (separate search)
c("North America", "Europe") %>%
  purrr::map(search_art, public=TRUE) %>%
  purrr::map(length) #97 and 558



# Get information associated with an object ID
## Define the search URL for the Metropolitan Museum API
search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/objects/"


## Function to get metadata and image URL for each object ID
grab_artwork_info <- function(object_id, show_art=FALSE) {
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
  
      # Return metadata and image URL
      metadata <- list(title = title, artist = artist, image_url = image_url)
  
      # Extract the image URL
      if (!is.null(image_url)) {
        if(show_art){
        # Display the image
        img <- image_read(image_url)
        print(img)
        }
      }
      return(metadata)
    } else {
      print(paste("Failed to fetch data for object ID:", object_id))
      return(metadata)
    }
  },
  error = function(e) {
    print(paste("Error processing object ID:", object_id, "-",
                e$message))
    return(NULL)
  },
  warning = function(w) {
    print(paste("Warning processing object ID:", object_id, "-", w$message))
    return(NULL)
  })
}


## Loop through all object IDs and retrieve information
all_object_info <- lapply(all_object_ids[21:25], function(id) grab_artwork_info(id, show_art=TRUE))

# Example: Display metadata and image for the first artwork in the list
first_artwork <- all_object_info[[1]]

first_artwork

# Download and display the image
img <- image_read(first_artwork$image_url)
print(img)  # Display the image in RStudio's Viewer pane






      
      
  