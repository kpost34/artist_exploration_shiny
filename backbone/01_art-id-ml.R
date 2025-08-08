# Models
# First model
#UI: user uploads image of painting, and app will identify artist 
#model trained on features of images only (predictors) and yield artist or movement (category)
#training data: use public and non-public domain data; choose k clusters with 80/20 breakdown,
  #fit model, apply hyperparameter tuning
#test data: user-imported image


# Load Packages and Functions=======================================================================
pacman::p_load(here, tidyverse, httr, jsonlite, magick, skimr, visdat, stringi, janitor)

source(here("fns_objs", "00_fn.R"))



# 1. Image Data Collection==========================================================================
# Goal: Gather a dataset of images that will be used to train the machine learning model.

## Step 1: Get the object IDs of possible artworks (remember that some don't have valid URLs) & 
  #split into reasonable sizes for next step
### Grab all object IDs
vec_art_objs <- search_paintings(nationality="Europe", public=NULL) %>% unlist()
length(vec_art_objs) #12697


### Split up vector
#### Calculate processing time
length(vec_art_objs) * 1.5 #19045.5 sec
19045.5/60 #~317 min

length(vec_art_objs)/50 #~254 groups of 50 obj IDs

254 * 30 #7620 seconds of breaks between groups
7620/60 #127 minutes

317 + 127 #444 min
444/60 #7.4 hours


#### Split up obj id vector and save
#splitting
grp <- 1:8
indexes_end <- grp * 1750
indexes_start <- indexes_end - 1749
indexes_end[length(grp)] <- length(vec_art_objs)


t_vec_art_objs <- purrr::map2(indexes_start, indexes_end, function(x, y) {
    vec_art_objs[x:y]
}) %>%
  set_names((paste0("vec_art_obj", grp)))


#saving
# purrr::map(grp, function(x) {
#   filename <- paste0("vec_art_objs", x, ".rds")
#   filepath <- here("data", "obj_id_vecs", filename)
# 
#   vec <- t_vec_art_objs[[x]]
# 
#   saveRDS(vec, filepath)
# })



## Step 2: Retain object IDs & associated info that have valid URLs and non-null values for 
  #artist name 
## Develop function
loop_get_artwork_info <- function(fileno){
  # Read in obj ids
  filename <- paste0("vec_art_objs", fileno, ".rds")
  fp <- here("data", "obj_id_vecs", filename)
  
  vec_art_loop <- readRDS(fp)

  # Create and populate list
  t_art_full <- list()
  n <- 1

  for(i in seq(1, 1750, 50)){
    t_art_loop <- purrr::map(vec_art_loop[i:(i+49)], function(obj_id) {
      Sys.sleep(1.5 + runif(1, 0, 1))
      get_artwork_info(obj_id)
    })
    Sys.sleep(30)
    t_art_full <- c(t_art_full, t_art_loop)
    print(paste("Batch", n, "Completed"))
    n <- n + 1
  }

  return(t_art_full)

}

## Execute function
filenum <- 8

# t_obj <- loop_get_artwork_info(fileno=filenum)


## Convert to DF and save
# df_obj <- t_obj %>%
#   compact() %>% #remove NULL entries
#   bind_rows()
# 
# fname_df <- paste0("df_art_info", filenum, ".rds")
# fp_out <- here("data", "art_info_dfs", fname_df)

# saveRDS(df_obj, fp_out)


## Step 3: Read in DFs of object IDs, combine, and save as one RDS
vec_fn_art_info <- list.files(here("data", "art_info_dfs"), full.names=TRUE)

df_art_info_all <- purrr::map_df(vec_fn_art_info, readRDS) %>%
  select(!public)


## Step 4: Combine with the public DF
### Read in public art info DF
fp_art_explore <- list.files(here("data"), "^02_art-exploration", full.names=TRUE) %>% 
  sort(decreasing=TRUE)

df_art_info_public <- readRDS(fp_art_explore)


#### Join by object_id & filter down
#create classification filter
valid_classifications <- c(
  "Paintings",
  "Paintings-Canvas",
  "Paintings-Fresco",
  "Paintings-Frescoes",
  "Paintings-Panels",
  "Paintings-Icons",
  "Miscellaneous-Paintings & Portraits"
)



# Helper function to lowercase common particles (you may already have this)
clean_artist_names <- function(df, name_col = "artist_simple") {
  
  lowercase_particles <- function(name) {
    # Fix apostrophe spacing: "d' Hondecoeter" -> "d'Hondecoeter"
    name <- str_replace_all(name, regex("d'\\s+", ignore_case = TRUE), "d'")
    
    # Define particles to keep lowercase (added "der")
    particles <- c("van", "von", "de", "del", "della", "da", "di", "le", "la", "du", 
                   "des", "dos", "das", "den", "het", "op", "te", "ten", "ter", "der", "d'")
    
    # Split into parts
    parts <- str_split(name, "\\s+")[[1]]
    
    fixed_parts <- sapply(parts, function(part) {
      lower_part <- str_to_lower(part)
      
      # Handle d' prefix separately
      if (str_starts(lower_part, "d'")) {
        # lowercase 'd'', capitalize first letter after apostrophe + rest as-is
        paste0("d'", str_to_upper(substr(part, 3, 3)), substr(part, 4, nchar(part)))
      } else if (lower_part %in% particles) {
        lower_part
      } else {
        # Capitalize first letter, keep rest as is
        paste0(str_to_upper(substr(part, 1, 1)), substr(part, 2, nchar(part)))
      }
    })
    
    paste(fixed_parts, collapse = " ")
  }
  
  df %>%
    mutate(
      artist_clean = !!sym(name_col) %>%
        str_remove("\\s*\\(.*?\\)") %>%
        str_remove_all("(?i)\\s+and workshop|\\s+called.*?$") %>%
        str_remove_all("(?i)^Attributed To\\s*") %>%
        str_remove_all("(?i)\\bthe elder\\b|\\bthe younger\\b|\\bthe third\\b") %>%
        str_squish() %>%
        str_replace_all("\\s*-\\s*", "-") %>%
        stri_trans_general("Latin-ASCII") %>%  # remove accents
        str_to_title() %>%
        str_replace_all("[^\\w\\s\\.\\-']", "") %>%
        str_replace_all("\\bD'", "d'") %>%
        str_replace_all(" Y ", " y ") %>%
        str_replace_all("Iii", "III") %>%
        str_squish()
    ) %>%
    rowwise() %>%
    mutate(
      artist_clean = lowercase_particles(artist_clean)
    ) %>%
    ungroup() %>%
    mutate(
      artist_clean = str_squish(artist_clean),
      artist_clean = str_remove(artist_clean, "\\s[\\p{Han}\\p{Hiragana}\\p{Katakana}]+$"),
      artist_clean = str_remove(artist_clean, "(?i)^Sir\\s+|^Count\\s+"),
      artist_clean = str_trim(artist_clean)
    )
}


artist_names_anon <- c(
  "^Master",
  "^Maestro",
  # "Master [Oo]f [Tt]he Saint Barbara Legend",
  # "Master [Oo]f [Tt]he Saint Catherine Legend",
  # "Master [Oo]f [Tt]he Saint Godelieve Legend",
  # "Master [Oo]f [Tt]he Saint Ursula Legend",
  # "Master [Oo]f [Tt]he Story [Oo]f Joseph",
  # "Master [Oo]f [Tt]he View [Oo]f Saint Gudula",
  # "Master [Oo]f [Tt]he Virgin [Aa]mong Virgins",
  # "Master [Oo]f Varlungo",
  "Monogrammist F[Aa]",
  "Monogrammist F[Ss]",
  "Monogrammist I[Ss]",
  "Monogrammist J[Gg]",
  "Monogrammist L[Aa][Mm]",
  # "Master Morata",
  "Morata Master",
  "Osma Master",
  "Osservanza Master",
  "Netherlandish",
  "Italian, Neapolitan Follower of Giotto",
  "Master G.Z.",
  "Follower of Lippo Memmi",
  "Italian, Lombard \\(probably Pavia\\)",
  "^German$",
  "^Shosai$", #multiple artists
  "Tosa School",
  "Unidentified",
  "Various Scholars",
  "Byzantine or Crusader",
  "Budapest Master",
  "^Workshop of",
  "[Pp]ainter",
  "artist$",
  "artists$",
  "artist\\(s\\)$"
)



  
# df_art_info_all_public <- 
df_art_info_all %>%
  #pull in True value of public field
  left_join(df_art_info_public[,c("object_id", "public")], by="object_id") %>% 
  mutate(public=replace_na(public, FALSE)) %>% 
  #retain paintings & named artists
  filter(classification %in% valid_classifications) %>% 
  filter(!str_detect(artist_simple, "[Pp]ainter|artist$|artists$|artist\\(s\\)$")) %>% 
  filter(!str_detect(artist_simple, paste(artist_names_anon, collapse="|"))) %>%
  #identify NAs
  mutate(across(.cols=c(nationality, bio, date, medium, dimensions, period, classification),
                .fns=~na_if(.x, ""))) %>% 
  #normalize artist names
  clean_artist_names() %>% 
  # filter(str_detect(artist_clean, "^Workshop")) %>% View()
  pull(artist_clean) %>% unique() %>% sort() -> vec_artists


# Load required package
install.packages("stringdist")  # Run this only once
library(stringdist)

# Paste your full list of artist names here
artists <- vec_artists

# Compute a distance matrix (Jaro-Winkler is good for names)
dist_matrix <- stringdistmatrix(artists, artists, method = "jw")

# Convert to data frame and extract potential duplicates
threshold <- 0.1  # lower threshold = more strict matching
duplicates <- which(dist_matrix < threshold & dist_matrix > 0, arr.ind = TRUE)

# Remove mirrored duplicates and self-pairs
duplicates <- duplicates[duplicates[,1] < duplicates[,2], ]

# Output similar pairs
for (i in seq_len(nrow(duplicates))) {
  cat(
    sprintf("[%d] \"%s\"  <-->  \"%s\"\n",
            i,
            artists[duplicates[i, 1]],
            artists[duplicates[i, 2]])
  )
}



#Rembrandt 
        



# 2. Explore Data===================================================================================




# 2. Image Preprocessing with magick================================================================
# Goal: Prepare the images for ML by resizing, normalizing, or augmenting them.
# Image Manipulation Tasks:
# Resize: Scale the images to a standard size so they can be input into a neural network or other ML model. This ensures consistent dimensions across all images.
# Color Adjustments: Convert to grayscale if color isn’t relevant, or apply other color transformations (e.g., histogram equalization) to standardize lighting and contrast.
# Crop/Pad: Adjust the aspect ratio, crop unnecessary parts, or add padding to maintain consistent image sizes.
# Augmentation: If necessary, apply transformations like rotation, flipping, or warping to augment your dataset and introduce variety.
# Convert to Matrix: After preprocessing, convert the images to numerical arrays or matrices that represent the image pixel values, which ML algorithms can use.
# Code Example:
# 
# r
# Copy
# library(magick)
# 
# # Read an image
# img <- image_read("artwork.jpg")
# 
# # Resize the image to 100x100 pixels
# img_resized <- image_resize(img, "100x100")
# 
# # Convert to grayscale
# img_gray <- image_convert(img_resized, colorspace = "gray")
# 
# # Convert to a matrix (pixel intensities)
# img_matrix <- as.integer(image_data(img_gray))
# 
# # Check matrix size
# dim(img_matrix)




# 3. Feature Extraction (Optional)==================================================================
# Goal: Transform raw image data into features that represent key characteristics of the image for the model to learn from.
# Feature Extraction Techniques:
# Pixel-level features: For simple models, the pixel values themselves might be sufficient.
# Histogram of Gradients (HOG): Useful for object recognition.
# Edge Detection: Use edge detection algorithms to extract key visual features.
# Deep Features: If using pre-trained CNNs, you might extract features using a convolutional network (e.g., ResNet or VGG) to obtain high-level abstractions.
# Example: You could extract HOG features from an image:
# 
# r
# Copy
# library(imager)
# img <- load.image("artwork.jpg")
# img_edge <- grayscale(img) %>% cannyEdges()



# 4. Data Splitting=================================================================================
# Goal: Split the image data into training, validation, and test sets.
# Typical Split:
# Training set (usually 70-80% of the data) used to train the model.
# Validation set (10-20%) used for model hyperparameter tuning and performance evaluation.
# Test set (10-20%) used to assess final model performance.



# 5. Model Selection================================================================================
# Goal: Choose an appropriate machine learning model based on your task and data.
# Common Models:
# Convolutional Neural Networks (CNNs): The most common approach for image classification tasks.
# Support Vector Machines (SVM): Sometimes used with extracted features for classification.
# K-Nearest Neighbors (KNN): Can be used for simple image classification problems based on pixel values.
# If working with deep learning, you might consider using keras or tensorflow to build and train neural networks.



# 6. Model Training=================================================================================
# Goal: Train the selected model using the preprocessed image data.
# Steps:
# Feed the image data (or extracted features) into the model.
# Train the model to learn patterns in the data (e.g., using gradient descent for CNNs).
# Monitor performance metrics such as loss and accuracy on the training and validation sets.
# Example (using a simple CNN model with keras):
# 
# r
# Copy
# library(keras)
# 
# # Define a simple CNN model
# model <- keras_model_sequential() %>%
#   layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = 'relu', input_shape = c(100, 100, 1)) %>%
#   layer_max_pooling_2d(pool_size = c(2, 2)) %>%
#   layer_flatten() %>%
#   layer_dense(units = 128, activation = 'relu') %>%
#   layer_dense(units = 10, activation = 'softmax')  # Assuming 10 classes
# 
# # Compile the model
# model %>% compile(
#   loss = 'categorical_crossentropy',
#   optimizer = 'adam',
#   metrics = c('accuracy')
# )
# 
# # Train the model (assuming X_train and y_train are prepared)
# model %>% fit(X_train, y_train, epochs = 10, validation_data = list(X_val, y_val))



# 7. Model Evaluation===============================================================================
# Goal: Evaluate the trained model on the test set to assess generalization performance.
# Evaluation Metrics:
# Accuracy
# Precision, Recall, F1-score (for imbalanced classes)
# Confusion Matrix (for multi-class classification)
# Example:
# 
# r
# Copy
# # Evaluate model performance on the test set
# model %>% evaluate(X_test, y_test)



# 8. Model Tuning and Optimization==================================================================
# Goal: Improve the model’s performance by adjusting hyperparameters.
# Techniques:
# Hyperparameter tuning (e.g., learning rate, number of layers, number of units in each layer).
# Data augmentation (to improve model generalization).
# Regularization techniques (e.g., dropout).



# 9. Final Deployment===============================================================================
# Goal: Deploy the trained model to production, where it can classify or process new images.
# Steps:
# Save the trained model (using save_model_hdf5 or similar).
# Load the model in the production environment and apply it to new images.
# Recap of the Steps:
# Collect Image Data: Gather labeled or unlabeled images.
# Preprocess Images: Resize, normalize, and possibly augment the images using magick.
# Feature Extraction: (Optional) Extract meaningful features from the images.
# Data Splitting: Split the dataset into training, validation, and test sets.
# Select Model: Choose an appropriate ML model, such as a CNN.
# Train the Model: Train the model using the processed images.
# Evaluate the Model: Test model performance on the test set.
# Tune Hyperparameters: Improve performance through hyperparameter optimization.
# Deploy the Model: Deploy the trained model for real-time or batch predictions.
# By following these steps, you can build an end-to-end machine learning pipeline for processing and classifying artwork images using the magick package for image preprocessing.