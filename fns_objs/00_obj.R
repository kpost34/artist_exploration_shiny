# Objects for Artist Exploration App #

# Instructions======================================================================================
## Classify art
txt_classify1 <- "Click 'Browse', choose a file from your machine, click Open, and the algorithm
        will identify the top X artists. Adjust X via the slider and enable percent confidence
        using the radio button. The results are displayed in the table."

txt_classify2 <- "Remember that this algorithm is trained on North American
              and European painters only."


## Artist exploration
txt_explore <- "Choose an artist either directly or by filtering with the movement and nationality
                selectors. Once selected, a bio and selection of their artwork
                will be displayed."


## Game
txt_game1 <- "Select your difficulty. Click begin and the first image will display. Choose the artist
             that created the painting either by selecting the appropriate response or typing it in.
             Click enter, and the app will evaluate your answer. Repeat for the remaining four
             images. Once completed, the machine will reveal its answers for you to compare."

txt_game2 <- "Difficulty level affects the response type, number of options, and the accuracy
              of the machine learning model."



# Artist Info=======================================================================================
## DF for selectors (artist info)
df_artist_info <- tribble(
  ~movement, ~nationality, ~artist, 
  "Realism", "French", "Manet, Edouard",
  "Impressionism", "French", "Manet, Edouard",
  "Impressionism", "Dutch", "van Gogh, Vincent",
  "Impressionism", "French", "Degas, Edgar",
  "Baroque", "Dutch", "van Rijn, Rembrandt",
  "Post-impressionism", "French", "Gauguin, Paul",
  "Symbolism", "French", "Gauguin, Paul",
  "Impressionism", "French", "Cézanne, Paul",
  "Post-impressionism", "French", "Cézanne, Paul",
  "Spanish Rennaissance", "Greek", "Theotokopoulos, Domenikos"
  # "Rococo", "Italian", "Tiepolo, Giovanni Battista",

) %>%
  mutate(artist_simple=convert_artist_name(artist)) 


## Bios
### Individual
bio_manet <- "Édouard Manet (1832–1883) was a pioneering French painter who played a crucial 
role in the transition from Realism to Impressionism. Known for his bold brushwork and modern 
subjects, Manet challenged traditional art norms with works like *Olympia* and *Luncheon on the 
Grass*. His innovative use of light and composition influenced a generation of artists. Though 
often controversial in his time, Manet maintained connections with avant-garde figures like 
Monet and Degas. His art bridged academic traditions and modern life, earning him a lasting 
legacy as a key figure in 19th-century art and a forerunner of modernist painting."


bio_vangogh <- "Vincent van Gogh (1853–1890) was a Dutch Post-Impressionist painter whose bold 
colors and emotional honesty revolutionized modern art. Despite struggling with mental illness and 
poverty, he produced over 2,000 artworks, including iconic pieces like *Starry Night*, *Sunflowers*, 
and *The Bedroom*. Van Gogh's expressive brushwork and unique vision were largely unrecognized 
during his lifetime—he sold only one painting—but he later became one of the most influential 
figures in Western art. His intense, passionate style captured both the beauty and turmoil of 
the human experience, leaving a lasting legacy that continues to inspire artists and audiences 
worldwide."


bio_degas <- "Edgar Degas (1834–1917) was a French artist best known for his masterful depictions 
of ballet dancers, capturing movement, grace, and candid moments behind the scenes. Though 
often associated with the Impressionists, Degas preferred to be called a realist, focusing on 
composition, line, and form. He worked in various media, including painting, sculpture, and 
pastel, and drew inspiration from classical art and contemporary life. His innovative use of 
perspective and cropping was influenced by photography and Japanese prints. Degas’s work reveals 
both elegance and psychological depth, securing his reputation as a key figure in 19th-century
French art."


bio_rembrandt <- "Rembrandt van Rijn (1606–1669) was a Dutch master painter and etcher, renowned 
for his profound use of light, shadow, and human emotion. A central figure of the Dutch Golden Age, 
he created iconic works such as *The Night Watch*, *The Anatomy Lesson of Dr. Nicolaes Tulp*, 
and a deeply introspective series of self-portraits. Rembrandt’s art explored biblical scenes, 
portraits, and everyday life with unmatched psychological depth and technical skill. Though he 
faced personal tragedy and financial hardship, his legacy endures as one of history’s greatest 
artists, influencing generations with his innovative storytelling, realism, and sensitivity to 
the human condition."


bio_gauguin <- "Paul Gauguin (1848–1903) was a French Post-Impressionist artist known for his 
bold colors, simplified forms, and depictions of exotic subjects. Disenchanted with Western 
society, he sought inspiration in Tahiti and the Marquesas Islands, where he created some of 
his most famous works, such as *Where Do We Come From? What Are We? Where Are We Going?*. 
Gauguin’s art broke from realism, emphasizing symbolism and emotion over natural representation. 
Though underappreciated in his lifetime, his innovative style profoundly influenced modern art, 
including movements like Fauvism and Expressionism. Gauguin remains a complex, controversial 
figure whose work challenged artistic and cultural boundaries."


bio_cezanne <- "Paul Cézanne (1839–1906) was a French Post-Impressionist painter whose work 
laid the foundation for modern art. Known for his structured brushstrokes and exploration of 
form and color, Cézanne sought to depict the underlying geometry of nature, often focusing on 
still lifes, landscapes, and bathers. His paintings, such as *Mont Sainte-Victoire* and 
*The Basket of Apples*, bridged Impressionism and Cubism, influencing artists like Picasso and 
Matisse. Though initially criticized, Cézanne gained recognition later in life for his innovative 
approach. His dedication to capturing visual perception made him a pivotal figure in the shift 
from 19th-century art to 20th-century modernism."


bio_theo <- "Doménikos Theotokópoulos (1541–1614), known as El Greco, was a Greek-born painter, 
sculptor, and architect who became a key figure of the Spanish Renaissance. Trained in the 
Byzantine tradition, he later absorbed Venetian and Roman influences before settling in Toledo, 
Spain. El Greco developed a highly distinctive style marked by elongated figures, dramatic 
lighting, and spiritual intensity. His masterpieces, such as *The Burial of the Count of Orgaz* 
and *View of Toledo*, reflect a blend of mysticism and Mannerist innovation. Though misunderstood 
in his time, El Greco's visionary art profoundly influenced modern artists, including Picasso and 
the Expressionists."


# bio_tiepolo <- "Giovanni Battista Tiepolo (1696–1770) was a Venetian painter and printmaker 
# celebrated for his grand, luminous frescoes and masterful use of color and composition. A 
# leading figure of the Rococo era, Tiepolo became renowned for his theatrical, uplifting scenes 
# filled with graceful figures, dynamic movement, and dramatic light. His major works include 
# ceiling frescoes in the Würzburg Residence in Germany and the Palazzo Labia in Venice. Tiepolo's 
# art blended classical themes with imaginative flair, embodying the elegance and exuberance of 
# 18th-century European art. His influence extended across Europe, and his legacy endures as one 
# of Italy’s greatest decorative painters."


### Compile into DF
df_artist_bios <- tibble(
  artist=c("Manet, Edouard", "van Gogh, Vincent", "Degas, Edgar", "van Rijn, Rembrandt", 
           "Gauguin, Paul", "Cézanne, Paul", "Theotokopoulos, Domenikos"),
  bio=c(bio_manet, bio_vangogh, bio_degas, bio_rembrandt, bio_gauguin, bio_cezanne,
        bio_theo)
) %>%
  mutate(artist_simple=convert_artist_name(artist))



### Join artist info and bios with art info for complete df
#### Read in art info (for public domain art)
fp_art_explore <- list.files(here("data"), "^00_art-exploration", full.names=TRUE) %>% 
  sort(decreasing=TRUE)

df_art_info_public <- readRDS(fp_art_explore) %>%
  select(!c(bio, period)) %>%
  filter(nchar(date) > 0, #must have date/year info
         classification=="Paintings") %>% 
  mutate(artist_simple=ifelse(
    str_detect(artist_simple, "\\("),
    str_remove_all(artist_simple, ".+\\(|\\).*$"),
    artist_simple
  )) 


#### Execute joins
cols_art_info <- c("object_id", "title", "artist", "artist_simple", "classification", "date",
                   "date_start", "date_end", "medium", "dimensions", "nationality",
                   "movement", "bio", "image_url", "public")

df_art_info_public_full <- df_artist_info_public %>%
  left_join(df_artist_bios) %>%
  left_join(df_art_info) %>%
  arrange(movement, nationality, artist) %>%
  select(all_of(cols_art_info))
  

df_art_info_public_full









  