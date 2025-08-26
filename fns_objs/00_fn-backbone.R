# Backbone Functions for Artist Exploration Shiny App #


# Utility functions------------------------------
grab_newest_fp <- function(dir, patt) {
  fp <- list.files(dir, 
                   pattern=patt,
                   full.names=TRUE) %>%
  sort(decreasing=TRUE) %>%
  .[1]
  
  return(fp)
}



# 00: Metadata Functions============================================================================
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


## Wrapper function for grabbing artist info
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


## Functions for cleaning metadata------------------------------
### Function to clean artist names (from metadata)
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
      artist_clean = str_trim(artist_clean),
      artist_clean = ifelse(artist_clean == "Nicola di Maestro Antonio",
                            "Nicola di Maestro Antonio d'Ancona",
                            artist_clean)
    )
}


## Functions for processing images------------------------------
### Function to process image file to vector
process_image_to_vector <- function(img_url, size = "100x100") {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".jpg")
  
  # Ensure the temp file is deleted on function exit
  on.exit(unlink(temp_file), add = TRUE)
  
  # Try to download the image
  success <- tryCatch({
    download.file(img_url, temp_file, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (!success) return(NULL)
  
  # Try to read and process the image
  img <- tryCatch({
    suppressWarnings(
      image_read(temp_file) %>%
      image_strip() %>%
      image_resize(size) %>%
      image_extent(size, gravity = "center", color = "white")
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if(is.null(img)) return(NULL)
  
  # Convert to feature vector
  img_array <- image_data(img)
  feature_vector <- as.integer(as.vector(img_array))
  
  return(feature_vector)
}


### Wrapper function for above
batch_image_processing <- function(df, start=1, n=100) {
  # Subset data
  df <- df[start:(start+(n-1)),]
  
  # Process image and store to new DF
  results <- purrr::map_df(df$object_id, function(x){
    img_url <- df %>% filter(object_id==x) %>% pull(image_url)
    feat_vec <- process_image_to_vector(img_url)
    
    if(is.null(feat_vec)) return(NULL)
    
    tibble(object_id=x, feature_vector=list(feat_vec))
  }) 
  
  # Remove NULLs before combining into a DF
  df_results <- purrr::compact(results) %>% bind_rows()
  
  # Save to rds
  fname <- paste0("df_img_feat_vec", start, ".rds")
  fp <- here("data", "img_feat_vec_dfs", fname)
  saveRDS(df_results, fp)
}



# 01 Functions======================================================================================
## QC functions for splitting data------------------------------
### Function to check min and max artworks per artist by dataset
check_artwork_range <- function(df) {
  df1 <- df %>%
    count(artist_clean) %>%
    summarize(min=min(n),
              max=max(n))
  
  return(df1)
}


### Function to check number of artists & artworks per dataset
check_n_art <- function(df) {
  df1 <- df %>%
    summarize(n_artists=n_distinct(artist_clean),
              n_artworks=n())
  
  return(df1)
}


## Feature extraction functions-----------------------------
### Subfunction to calculate mean absolute deviation
mad <- function(x) {
  # Calculate the mean of the data
  data_mean <- mean(x, na.rm = TRUE) # na.rm = TRUE handles missing values
  
  # Calculate the absolute deviations from the mean
  absolute_deviations <- abs(x - data_mean)
  
  # Calculate the mean of the absolute deviations
  mad_value <- mean(absolute_deviations, na.rm = TRUE)
  
  return(mad_value)
}


### Function to extract RGB summary stats/features
extract_rgb_features <- function(vec) {
  # Pixels are flattened by color
  R <- vec[1:10000]
  G <- vec[10001:20000]
  B <- vec[20001:30000]
  
  # Calculate summary stats on each color
  tibble(
    R_mean=mean(R), R_median=median(R), R_sd=sd(R), R_min=min(R), R_max=max(R), 
    R_q1=quantile(R, 0.25), R_q3=quantile(R, 0.75), R_iqr=quantile(R, 0.75)-quantile(R, 0.25),
    R_range=max(R)-min(R), R_skew=skewness(R), R_kurtosis=kurtosis(R), R_mad=mad(R),
    G_mean=mean(G), G_median=median(G), G_sd=sd(G), G_min=min(G), G_max=max(G), 
    G_q1=quantile(G, 0.25), G_q3=quantile(G, 0.75), G_iqr=quantile(G, 0.75)-quantile(G, 0.25),
    G_range=max(G)-min(G), G_skew=skewness(G), G_kurtosis=kurtosis(G), G_mad=mad(G),
    B_mean=mean(B), B_median=median(B), B_sd=sd(B), B_min=min(B), B_max=max(B), 
    B_q1=quantile(B, 0.25), B_q3=quantile(B, 0.75), B_iqr=quantile(B, 0.75)-quantile(B, 0.25),
    B_range=max(B)-min(B), B_skew=skewness(B), B_kurtosis=kurtosis(B), B_mad=mad(B)
  )
}


### Subfunction to count bins
count_bin <- function(x, bin) {
  # Count values per bin
  if(bin==1){
    bin_count <- between(x, 0, 51) %>% sum()
  } else if(bin==2){
    bin_count <- between(x, 52, 102) %>% sum()
  } else if(bin==3){
    bin_count <- between(x, 103, 153) %>% sum()
  } else if(bin==4){
    bin_count <- between(x, 154, 204) %>% sum()
  } else if(bin==5){
    bin_count <- between(x, 205, 255) %>% sum()
  } else{stop("'bin' must be an integer from 1 through 5")
  }
  
  # Return value
  return(bin_count)
}


### Function to extract rgb bin counts
extract_rgb_bins <- function(vec) {
  # Pixels are flattened by color
  R <- vec[1:10000]
  G <- vec[10001:20000]
  B <- vec[20001:30000]
  
  # Calculate color pixel counts by intensity range
  tibble(
    R_bin1=count_bin(R, 1), R_bin2=count_bin(R, 2), R_bin3=count_bin(R, 3),
    R_bin4=count_bin(R, 4), R_bin5=count_bin(R, 5),
    G_bin1=count_bin(G, 1), G_bin2=count_bin(G, 2), G_bin3=count_bin(G, 3),
    G_bin4=count_bin(G, 4), G_bin5=count_bin(G, 5),
    B_bin1=count_bin(B, 1), B_bin2=count_bin(B, 2), B_bin3=count_bin(B, 3),
    B_bin4=count_bin(B, 4), B_bin5=count_bin(B, 5)
  )
  
}

