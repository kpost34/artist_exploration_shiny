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
  
  if(!success) return(NULL)
  
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


### Function to extract RGB summary stats/features (03 too)
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


### Subfunction to count bins (03 too)
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


### Function to extract rgb bin counts (03 too)
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


## Prediction functions-----------------------------
### Function get get the top k predictions when running finalized workflow on test data
get_top_k_preds <- function(df, k = 3) {
  # Assume df columns: 1=object_id, 2=obs, 3+: predicted probabilities
  
  object_ids <- df[[1]]
  obs <- df[[2]]
  prob_df <- df[, -(1:2)]
  
  # Ensure probabilities are numeric
  prob_df <- as.data.frame(lapply(prob_df, as.numeric))
  
  # For each row, get top-k predictions in long format
  top_k_long <- purrr::map_dfr(seq_len(nrow(prob_df)), function(i) {
    prob_vector <- as.numeric(prob_df[i, ])
    names(prob_vector) <- colnames(prob_df)
    top_k <- sort(prob_vector, decreasing = TRUE)[1:k]
    
    tibble(
      object_id = object_ids[i],
      obs = obs[i],
      rank = seq_len(k),
      artist = names(top_k) %>% 
        stringr::str_remove("^\\.pred_") %>%
        stringr::str_replace_all("\\.", " "),
      prob = as.numeric(top_k)
    )
  })
  
  # Pivot to wide format
  top_k_wide <- tidyr::pivot_wider(
    top_k_long,
    id_cols = c(object_id, obs),
    names_from = rank,
    values_from = c(artist, prob),
    names_glue = "{.value}_top{rank}"
  )
  
  # Calculate match indicators
  top_k_wide <- top_k_wide %>%
    rowwise() %>%
    mutate(
      # Logical: does top1 artist match obs?
      top_match = (artist_top1 == obs),
      
      # Logical: does any artist in top-k match obs?
      any_match = any(c_across(starts_with("artist_top")) == obs),
      
      # Integer: which rank matches obs? NA if none
      rank_match = {
        matches <- which(c_across(starts_with("artist_top")) == obs)
        if (length(matches) == 0) NA_integer_ else matches[1]
      }
    ) %>%
    ungroup() %>%
    # Reorder columns: object_id, obs, top_match, any_match, rank_match, then rest
    select(object_id, obs, top_match, any_match, rank_match, everything())
  
  return(top_k_wide)
}


### Function to return probs on single artwork
get_top_k_preds_for_artwork <- function(new_artwork_df, model, k = 3) {
  # Predict probabilities for the single artwork
  prob_df <- predict(model, new_data = new_artwork_df, type = "prob")
  
  # Convert to named numeric vector
  prob_vector <- as.numeric(prob_df[1, ])
  names(prob_vector) <- colnames(prob_df)
  
  # Get top-k predictions as tibble
  top_k <- sort(prob_vector, decreasing = TRUE)[1:k]
  
  tibble(
    artist = names(top_k) %>% 
      str_remove("^\\.pred_") %>% 
      str_replace_all("\\.", " "),
    probability = as.numeric(top_k)
  )
}



# 03 Functions======================================================================================
## Metadata extraction functions-----------------------------
### Function to classify medium
classify_medium <- function(df) {
  # Create mixed medium vector
  medium_mixed <- c("Tempera, oil, and gold on wood", 
                    "Tempera and gold on canvas, transferred from wood",
                    "Oil on panel, transferred to canvas", 
                    "Tempera and oil on wood")
  
  # Categorize medium values into larger groups
  df1 <- df %>%
    mutate(medium_group=case_when(
      medium=="Oil on copper"                                ~ "Oil on metal",
      medium=="Fresco, transferred to canvas"                ~ "Fresco",
      medium %in% medium_mixed                               ~ "Mixed or Other",
      str_detect(medium, "^Oil.*on (paper|parchment)")       ~ "Oil on paper",
      str_detect(medium, "^Oil.*on canvas")                  ~ "Oil on canvas",
      str_detect(medium, "^Oil.*on (wood|oak|beech|linden)") ~ "Oil on wood",
      str_detect(medium, "^Tempera")                         ~ "Tempera on wood",
      str_detect(medium, "^Mixed media")                     ~ "Mixed or Other",
      TRUE                                                   ~ "NEEDS CATEGORY")
    ) 
  
  return(df1)
}


## Dimension-based functions
### Functions to extract width and height
#extract width
grab_width <- function(string, special=FALSE, num=FALSE) {
  if(!special) {
    width <- str_extract(string, "[0-9.]{1,}(?= cm\\))") 
  } else if(special) {
    width <- str_extract(string, "(?<=([Pp]archment|[Pp]ainted [Ss]urface)).+") %>%
      # width <- str_remove(dimensions, ".+ painted surface") %>%
      str_extract("[0-9.]{1,}(?= cm\\))") 
  }
  
  if(num) {
    width <- as.numeric(width)
  }
  return(width)
}

#extract height
grab_height <- function(string, special=FALSE, num=FALSE) {
  if(!special) {
    height <- str_extract(string, "[0-9.]{1,}(?= [x|\u00d7] [0-9.]{1,} cm\\))")
  } else if(special){
    height <- str_extract(string, "(?<=([Pp]archment|[Pp]ainted [Ss]urface)).+") %>%
      str_extract("[0-9.]{1,}(?= [x|\u00d7] [0-9.]{1,} cm\\))") 
  }
  
  if(num) {
    height <- as.numeric(height)
  }
  return(height)
}


## Function to calculate height and width of multi-piece paintings
calc_multi_dims <- function(df) {
  df1 <- df %>%
    select(object_id, dimensions) %>%
    filter(str_detect(dimensions, "Each|wing|Angel|panel|\\(a\\)")) %>%
    mutate(
      each=str_detect(dimensions, "[Ee]ach"),
      dim_each=ifelse(each, 
                      str_extract(dimensions, "(?<=[Ee]ach).+\\)") %>%
                        str_extract("\\(.+?cm\\)"), NA_character_),
      dim_extract=ifelse(str_detect(dimensions, "[Pp]ainted surface"),
                         str_extract_all(dimensions, "(?<=[Pp]ainted surface).+?\\)"),
                         str_extract_all(dimensions, "(?<= in).+?\\)"))
    ) %>% 
    select(!each) %>%
    unnest(cols=dim_extract) %>% 
    mutate(dim_extract=str_extract(dim_extract, "\\(.+?cm\\)")) %>% 
    mutate(dim_extract=paste(dim_extract, dim_each, sep="_")) 
  
  df2 <- df1 %>%
    bind_rows(
      df1 %>%
        select(object_id, dimensions, dim_extract="dim_each") %>%
        filter(!is.na(dim_extract)) %>%
        distinct()
    ) %>%
    select(!dim_each) %>%
    mutate(dim_extract=str_replace(dim_extract, "x(?=[0-9])", "x "),
           height_cm=grab_height(dim_extract, num=TRUE),
           width_cm=grab_width(dim_extract, num=TRUE)) %>%
    group_by(object_id, dimensions) %>%
    summarize(height_cm=max(height_cm),
              width_cm=sum(width_cm)) %>%
    ungroup() %>%
    mutate(
      multi_piece=TRUE,
      overall=str_detect(dimensions, "[Oo]verall"),
      ps=str_detect(dimensions, "[Pp]ainted|[Pp]archment"),
      painted_surface=ps|(!ps & !overall)
    ) %>%
    select(object_id, height_cm, width_cm, painted_surface, multi_piece)
  
  return(df2)
  
}


## Function to calculate height and width of singlei-piece painting
calc_single_dims <- function(df) {
  df1 <- df %>%
    select(object_id, dimensions) %>% 
    #remove multi-piece works
    filter(!str_detect(dimensions, "Each|wing|Angel|panel|\\(a\\)")) %>%
    #note: painted_surface here is just for filtering as it's missing examples with 1 set of dims
    mutate(
      n_sets=str_count(dimensions, " x | \u00d7 ")/2,
      diameter=str_detect(dimensions, "[Dd]iameter"),
      overall=str_detect(dimensions, "[Oo]verall"),
      ps=str_detect(dimensions, "[Pp]ainted|[Pp]archment")
    ) %>%
    mutate(
      dimensions=str_replace_all(dimensions, "(?<=[0-9]{1})cm", " cm"),
      height_cm=case_when(
        (n_sets==0|diameter) & !ps  ~ grab_width(dimensions),
        (n_sets==0|diameter) & ps   ~ grab_width(dimensions, special=TRUE),
        n_sets==1|n_sets==1.5       ~ grab_height(dimensions), #ps or !ps is the same
        n_sets==2 & !ps             ~ grab_height(dimensions),
        n_sets==2 & ps              ~ grab_height(dimensions, special=TRUE),
        TRUE                        ~ "NEEDS CATEGORY"),
      width_cm=case_when(
        (n_sets==0|diameter) & !ps  ~ grab_width(dimensions),
        (n_sets==0|diameter) & ps   ~ grab_width(dimensions, special=TRUE),
        n_sets==1|n_sets==1.5       ~ grab_width(dimensions), #ps or !ps is the same
        n_sets==2 & !ps             ~ grab_width(dimensions),
        n_sets==2 & ps              ~ grab_width(dimensions, special=TRUE),
        TRUE                        ~ "NEEDS CATEGORY")
    ) %>%
    mutate(across(c(height_cm, width_cm), ~as.integer(.x)),
           multi_piece=FALSE,
           painted_surface=ps|(!ps & !overall)) %>%
    select(object_id, height_cm, width_cm, painted_surface, multi_piece)
  
  return(df1)
}


## Function to extract shape
extract_shape <- function(df, df_dim) {
  df %>% 
    select(object_id, dimensions) %>%
    left_join(df_dim, by="object_id") %>%
    mutate(
      diameter=str_detect(dimensions, "[Dd]iameter|[Rr]ound"),
      shape=case_when(
        diameter                                               ~ "Circle",
        str_detect(dimensions, "[Ii]rregular [Oo]val|[Oo]val") ~ "Oval",
        str_detect(dimensions, "[Ii]rregular")                 ~ "Irregular",
        str_detect(dimensions, "[Ss]hape|[Aa]rched")           ~ "Arched",
        #within 5% is considered square
        between(height_cm, 0.95*width_cm, 1.05*width_cm)       ~ "Square",
        between(width_cm, 0.95*height_cm, 1.05*height_cm)      ~ "Square",
        TRUE                                                   ~ "Rectangle")
    ) %>% 
    select(!c(dimensions, diameter))
}
























