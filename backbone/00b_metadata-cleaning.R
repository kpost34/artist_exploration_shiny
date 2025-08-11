# Import individual metadata files, combine, and clean

# Load Packages and Functions=======================================================================
pacman::p_load(here, httr, jsonlite, tidyverse, stringi, janitor, stringdist)

source(here("fns_objs", "00_fn.R"))



# Data Import and Cleaning==========================================================================
## Read in DFs of object IDs, combine, and save as one RDS (all metadata)
vec_fn_art_info <- list.files(here("data", "art_info_dfs"), full.names=TRUE)

df_art_info_all <- purrr::map_df(vec_fn_art_info, readRDS) %>%
  select(!public)


## Read in public art info DF
fp_art_explore <- grab_newest_fp(dir=here("data"), patt="^00_art-exploration")

df_art_info_public <- readRDS(fp_art_explore)


## Combine and clean
### Create objects for cleaning
#classification filter
valid_classifications <- c(
  "Paintings",
  "Paintings-Canvas",
  "Paintings-Fresco",
  "Paintings-Frescoes",
  "Paintings-Panels",
  "Paintings-Icons",
  "Miscellaneous-Paintings & Portraits"
)

#vector of anonymous artists
artist_names_anon <- c(
  "^Master",
  "^Maestro",
  "Monogrammist F[Aa]",
  "Monogrammist F[Ss]",
  "Monogrammist I[Ss]",
  "Monogrammist J[Gg]",
  "Monogrammist L[Aa][Mm]",
  "Morata Master",
  "Osma Master",
  "Osservanza Master",
  "Netherlandish",
  "Italian, Neapolitan Follower of Giotto",
  "Master G.Z.",
  "Follower of Lippo Memmi",
  "Italian, Lombard \\(probably Pavia\\)",
  "^German$",
  "^Shosai$", 
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

european_nationalities <- c(
  "Netherlandish", "Dutch", "French", "Italian", "German", "Swiss", "Flemish",
  "Spanish", "British", "Greek", "Danish", "Norwegian", "Austrian",
  "Dutch, British", "British, Scottish", "Belgian", "Swedish", "Portuguese",
  "British, Welsh", "Irish", "Hungarian", "Italian, active Ancona",
  "British, born The Netherlands"
)

#### Join and wrangle data
df_art_info_final <- df_art_info_all %>%
  #pull in True value of public field
  left_join(df_art_info_public[,c("object_id", "public")], by="object_id") %>% 
  mutate(public=replace_na(public, FALSE)) %>% 
  #retain paintings & named artists
  filter(classification %in% valid_classifications) %>% 
  filter(!str_detect(artist_simple, "[Pp]ainter|artist$|artists$|artist\\(s\\)$")) %>% 
  filter(!str_detect(artist_simple, paste(artist_names_anon, collapse="|"))) %>% 
  #identify NAs
  mutate(across(.cols=c(nationality, bio, date, medium, dimensions, period, classification),
                .fns=~na_if(.x, "")),
  #manually impute nationalities then remove non-European painters
    nationality=case_when(
      artist_simple=="Cecco del Caravaggio (Francesco Buoneri)" ~ "Italian",
      artist_simple=="Tawaraya Sōri"                            ~ "Japanese",
      artist_simple=="Manohar"                                  ~ "Indian",
      TRUE                                                      ~ nationality)) %>% 
  filter(nationality %in% european_nationalities) %>%
  #populate date and bio fields when missing (for display)
  mutate(date = case_when(
    is.na(date) & date_start==date_end ~ as.character(date_start),
    is.na(date) & date_start!=date_end ~ paste0("ca. ", date_start, "-", date_end),
    TRUE                               ~ date),
    bio=ifelse(is.na(bio), nationality, bio)) %>%
  #normalize artist names
  clean_artist_names() %>% 
  #remove period because it's fully missing
  select(!period) 


#---------------------------------------------------------------------------------------------------
# NOTE: code below was one prior to finalizing clean_artist_names() function which incorporates
  #code to combine duplicate artist names (different spellings) into one

## Grab vector of artists
# df_art_info_final %>% pull(artist_clean) %>% unique() %>% sort() -> artists
# 
# 
# ## Compute a distance matrix (Jaro-Winkler is good for names)
# dist_matrix <- stringdistmatrix(artists, artists, method = "jw")
# 
# ## Convert to data frame and extract potential duplicates
# threshold <- 0.1  # lower threshold = more strict matching
# duplicates <- which(dist_matrix < threshold & dist_matrix > 0, arr.ind = TRUE)
# 
# ## Remove mirrored duplicates and self-pairs
# duplicates <- duplicates[duplicates[,1] < duplicates[,2], ]
# 
# ## Output similar pairs
# for (i in seq_len(nrow(duplicates))) {
#   cat(
#     sprintf("[%d] \"%s\"  <-->  \"%s\"\n",
#             i,
#             artists[duplicates[i, 1]],
#             artists[duplicates[i, 2]])
#   )
# }


#Results:
# "Andrea di Cione"  <-->  "Andrea di Lione" #different 
# "Francesco Francia"  <-->  "Francesco Granacci" #different
# "Giovanni Bellini"  <-->  "Giovanni Boldini" #different
# "Gustave Dore"  <-->  "Gustave Moreau" #different
# "Niccolo di Pietro"  <-->  "Niccolo di Pietro Gerini" #different
# "Nicola di Maestro Antonio"  <-->  "Nicola di Maestro Antonio d'Ancona"
  #same; latter is more common; incorporated into cleaning function

#---------------------------------------------------------------------------------------------------

# Save DF===========================================================================================
fn_art_info_final <- paste0("00_art-info-final_", Sys.Date(), ".rds")
fp_art_info <- here("data", fn_art_info_final)
# saveRDS(df_art_info_final, fp_art_info)









