# Models
# First model
#UI: user uploads image of painting, and app will identify artist 
#model trained on features of images only (predictors) and yield artist or movement (category)
#training data: use public and non-public domain data; choose k clusters with 80/20 breakdown,
  #fit model, apply hyperparameter tuning
#test data: user-imported image


# Load Packages and Functions=======================================================================
pacman::p_load(here, httr, jsonlite, magick)

source(here("fns_objs", "00_fn.R"))



# 1. Image Data Collection==========================================================================
# Goal: Gather a dataset of images that will be used to train the machine learning model.
# Considerations:
# You need a collection of labeled images (if you're doing supervised learning) or unlabeled images 
  #(for unsupervised learning).
# The images should be relevant to your problem (e.g., artwork images, medical images, etc.).
# Example: If you're working with artwork, collect images of different artwork categories, such as 
  #paintings, sketches, or digital art.


## Step 1: Get the object IDs of possible artworks (remember that some don't have valid URLs)
natls <- c("North America", "Europe")

vec_art_objs <- purrr::map(natls, search_paintings) %>%
  unlist() 


## Step 2: Retain object IDs that have valid URLs and non-null values for artist name and artwork
  #(first to see if there's enough art to train a model for both)
## Run function
obj <- vec_art_objs[1]

obj <- validate_obj_id(obj)

valid_obj_id <- purrr:::map_int(vec_art_objs[1:1000], validate_obj_id) %>%
  .[!is.na(.)]


# Get information associated with an object ID
## Define the search URL for the Metropolitan Museum API
obj1 <- vec_art_objs[1]

get_artwork_info(vec_art_objs[20])

vec_art_info <- purrr::map(vec_art_objs, get_artwork_info)



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