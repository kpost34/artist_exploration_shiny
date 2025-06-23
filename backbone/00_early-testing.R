# Connect to The Met's free API, view images of artwork, and gather metadata

# Load Packages=====================================================================================
pacman::p_load(httr, jsonlite, magick)



# Make a request to the API=========================================================================
## Define the API URL
url <- "https://collectionapi.metmuseum.org/public/collection/v1/objects"


## Make the GET request
response <- GET(url)


## Check if the request was successful
if (status_code(response) == 200) {
  #parse the response content
  content <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(content)
  #print the first few results
  print(list(total=data[[1]], objectIDs=data[[2]][1:10]))
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
search_paintings <- function(nationality, public=NULL) {
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
north_american_artworks <- search_paintings("North America") #8869 (as of 6/15/25)
european_artworks <- search_paintings("Europe") #12,418 (as of 6/15/25)

#combine the results (if you want to combine them)
all_object_ids <- c(north_american_artworks, european_artworks)
print(paste("Total paintings found:", length(all_object_ids))) #21,314 (as of 6/15/25)


## Here's a breakdown of artworks based on continent and domain (separate search)
c("North America", "Europe") %>%
  purrr::map(search_paintings, public=TRUE) %>%
  purrr::map(length) #97 and 558



# Get information associated with an object ID
## Define the search URL for the Metropolitan Museum API
search_url <- "https://collectionapi.metmuseum.org/public/collection/v1/objects/"


## Function to get metadata and image URL for each object ID
get_artwork_info <- function(object_id, show_art=FALSE) {
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
all_object_info <- lapply(all_object_ids[21:25], function(id) get_artwork_info(id, show_art=TRUE))

# Example: Display metadata and image for the first artwork in the list
first_artwork <- all_object_info[[1]]

first_artwork

# Download and display the image
img <- image_read(first_artwork$image_url)
print(img)  # Display the image in RStudio's Viewer pane





# Models
# First model
#UI: user uploads image of painting, and app will identify artist and movement
#model trained on features of images only (predictors) and yield artist or movement (category)
#training data: use public and non-public domain data; choose k clusters with 80/20 breakdown,
  #fit model, apply hyperparameter tuning
#test data: user-imported image


# Second model & third models
# UI: a series of images or images plus metadata (different models); let's say 5 and user competes
  #against app in identifying artist and/or movememnt correctly
# model trained on feature of images only or on images and metadata
# training data:
# test data: public domain images from The Met only







#NOTE: chatgpt gave me the following info for developing an ML algo for this project
# To develop a machine learning algorithm to identify the artist and movement/period of an artwork, conceptually, you would follow these main steps:
# 
# 1. Data Collection:
# Images: Collect a large dataset of artwork images, along with labeled metadata such as artist and movement/period.
# Metadata: Include rich metadata like artist names, movement or period labels, styles, medium, and other descriptive fields.
# 2. Preprocessing:
# Image Preprocessing: Resize and normalize artwork images to prepare them for input into a model (e.g., converting images to a fixed size, applying color normalization).
# Metadata Preprocessing: Convert categorical data (artist names, period/movement) into numerical representations, possibly using one-hot encoding, label encoding, or embeddings.
# 3. Feature Extraction:
# Visual Features: Use a Convolutional Neural Network (CNN) to automatically extract features from the images (e.g., colors, brushstrokes, composition). Pretrained models like ResNet, VGG, or Inception can be used for feature extraction.
# Metadata Features: Extract relevant metadata features such as artist names, movement/period, medium, and classify them (e.g., using embeddings or one-hot encoding for categorical fields).
# 4. Model Architecture:
# Multi-Modal Model: Create a hybrid model that can process both image and metadata inputs:
# For images, use a CNN (either from scratch or using a pretrained model for transfer learning).
# For metadata, use a feedforward neural network (FNN) or embedding layers for categorical data.
# Combine both image features and metadata features into a unified representation before making predictions.
# Classifier: For the final output:
# Use a softmax classifier to predict probabilities for multiple classes (artist, movement/period).
# For multiple classes (artist and movement), either predict them sequentially (one after the other) or jointly (multi-output classification).
# 5. Training:
# Loss Function: Use categorical cross-entropy for classification tasks. If predicting artist and movement separately, you could use multiple loss functions.
# Data Augmentation: Apply data augmentation to artwork images (e.g., random cropping, rotations, flipping) to increase diversity and prevent overfitting.
# 6. Evaluation:
# Evaluate the model using appropriate metrics like accuracy, precision, and recall. If there are many classes (artists or movements), use top-k accuracy to check if the correct artist/movement is in the top k predictions.
# 7. Fine-Tuning & Optimization:
# Fine-tune the hyperparameters (e.g., learning rate, batch size) and model architecture (e.g., CNN depth, embedding dimensions).
# Use cross-validation or split the dataset into training, validation, and test sets to prevent overfitting.
# Conceptual Flow:
# Data Collection → 2. Preprocessing → 3. Feature Extraction (Image + Metadata) → 4. Model Design (CNN for images + FNN for metadata) → 5. Training → 6. Evaluation → 7. Fine-tuning & Deployment
# By combining image features with metadata, you can leverage both visual and textual information, enabling the model to better identify the artist and movement of an artwork.




      
      
  