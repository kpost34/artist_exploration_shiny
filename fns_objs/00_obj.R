# Objects for Artist Exploration App #

# Strings
## Classify art
txt_classify1 <- "Click 'Browse', choose a file from your machine, click Open, and the algorithm
        will identify the top X artists. Adjust X via the slider and enable percent confidence
        using the radio button. The results are displayed in the table."

txt_classify2 <- "Remember that this algorithm is trained on North American
              and European painters only."


## Artist exploration
txt_explore <- "Choose an artist either directly or by filtering with the movement and nationality
                selectors. Once selected, a photograph, bio, and selection of their artwork
                will be displayed."


## Game
txt_game1 <- "Select your difficulty. Click begin and the first image will display. Choose the artist
             that created the painting either by selecting the appropriate response or typing it in.
             Click enter, and the app will evaluate your answer. Repeat for the remaining four
             images. Once completed, the machine will reveal its answers for you to compare."

txt_game2 <- "Difficulty level affects the response type, number of options, and the accuracy
              of the machine learning model."


# Artist Info
#NOTE: will replace with dynamic version
## DF for selectors (tab 2)
df_artist_info <- tribble(
  ~movement, ~nationality, ~artist, 
  "Impressionism", "Dutch", "Van Gogh, Vincent",
  "Impressionism", "French", "Monet, Claude",
  "Cubism", "Spanish", "Picasso, Pablo",
  "Surrealism", "Spanish", "Picasso, Pablo",
  "Realism", "French", "Manet, Edouard",
  "Impressionism", "French", "Manet, Edouard",
  "Cubism", "Spanish", "Dalí, Salvador",
  "Surrealism", "Spanish", "Dalí, Salvador"
) %>%
  mutate(artist_simple=convert_artist_name(artist))


## Bios
### Individual
bio_dali <- "Dali bio"


bio_vangogh <- "Van gogh bio"


bio_monet <- "Monet bio"


bio_manet <- "Manet bio"


bio_picasso <- "Picasso bio"


### Compile into DF
df_artist_bios <- tibble(
  artist=c("Dalí, Salvador", "Van Gogh, Vincent", "Monet, Claude", "Manet, Edouard", "Picasso, Pablo"),
  bio=c(bio_dali, bio_vangogh, bio_monet, bio_manet, bio_picasso)
) %>%
  mutate(artist_simple=convert_artist_name(artist))




### Join artist info and bios with art info for complete df
#### Read in art info (for public domain art)
fp_art_explore <- here("data", "02_art-exploration.rds")
df_art_info <- readRDS(fp_art_explore)


#### Execute joins
df_art_info_full <- df_artist_info %>%
  left_join(df_artist_bios) %>%
  left_join(df_art_info)

df_art_info_full


## Choices for game (will change later)
vec_artists <- c("Van Gogh", "Monet", "Dalí", "Picasso", "Manet")






  