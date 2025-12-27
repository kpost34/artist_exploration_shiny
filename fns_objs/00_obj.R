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



# 02: Art Exploration===============================================================================
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
  "Spanish Rennaissance", "Greek", "Theotokopoulos, Domenikos",
  "Northern Renaissance", "Netherlandish", "David, Gerard",
  "Northern Renaissance", "Netherlandish", "Memling, Hans", 
  "Baroque", "Dutch", "Vermeer, Johannes", 
  "Northern Renaissance", "Netherlandish",  "Christus, Petrus", 
  "Rococo", "Italian", "Falca, Pietro",
  "Romanticism", "French", "Delacroix, Eugène", 
  "Post-impressionism", "French", "Seurat, Georges", 
  "Baroque", "Flemish", "Rubens, Peter Paul", 
  "Barbizon School", "French", "Rousseau, Théodore"
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


bio_david <- "Gerard David (c.1460–1523) was an Early Netherlandish painter active in Bruges, 
known for his serene compositions and luminous color. Working during the Northern Renaissance, 
David combined meticulous detail with a calm, harmonious sense of space, often focusing on religious 
and devotional subjects. Notable works such as *The Virgin Among the Virgins* and *The Judgement of 
Cambyses* demonstrate his refined technique and emotional restraint. Influenced by Jan van Eyck and 
Hans Memling, David helped advance a more unified and naturalistic style in Netherlandish painting."


bio_memling <- "Hans Memling (c.1430–1494) was a German-born Early Netherlandish painter who worked 
primarily in Bruges. Renowned for his delicate realism, balanced compositions, and luminous color, 
Memling specialized in religious altarpieces and intimate portraits. Works such as *The Last 
Judgment* and *The St. Ursula Shrine* blend the precision of Jan van Eyck with a softer, more 
graceful sensibility. Highly successful in his lifetime, Memling exemplified the devotional clarity 
and refined naturalism of late 15th-century Northern Renaissance art."


bio_vermeer <- "Johannes Vermeer (1632–1675) was a Dutch Baroque painter celebrated for his 
masterful use of light, color, and quiet domestic scenes. Working in Delft during the Dutch Golden 
Age, Vermeer specialized in intimate interiors depicting everyday life, often featuring a single 
figure absorbed in a moment of stillness. Paintings such as *Girl with a Pearl Earring* and *The 
Milkmaid* reveal his subtle handling of perspective and luminous surfaces. Though little known 
during his lifetime, Vermeer is now regarded as one of the greatest painters of the 17th century 
for his poetic realism and technical refinement."


bio_christus <- "Petrus Christus (c.1410–1475/76) was an Early Netherlandish painter active in 
Bruges, known for bringing greater spatial coherence and realism to Northern Renaissance art. 
Influenced by Jan van Eyck, Christus employed precise detail, controlled perspective, and clear 
light to create calm, structured compositions. His works, including *A Goldsmith in His Shop* and 
*Portrait of a Carthusian*, demonstrate an innovative use of linear perspective and psychological 
presence. As a key transitional figure, Petrus Christus helped move Netherlandish painting toward 
a more rational and unified representation of space."


bio_falca <- "Pietro Falca (1696–1770), better known as Giovanni Battista Tiepolo, was a Venetian 
Rococo painter and fresco artist renowned for his luminous color, dramatic compositions, and airy 
illusionism. Active across Italy, Germany, and Spain, Tiepolo specialized in grand decorative 
schemes for palaces and churches. Major works such as *The Banquet of Cleopatra* and the frescoes 
of the Würzburg Residenz showcase his dynamic figures and theatrical use of light. Celebrated in 
his lifetime, Tiepolo is regarded as the greatest fresco painter of the 18th century."


bio_delacroix <- "Eugène Delacroix (1798–1863) was a leading French Romantic painter known for his 
expressive brushwork, rich color, and dramatic subject matter. Rejecting strict neoclassical 
restraint, Delacroix emphasized emotion, movement, and atmosphere in his historical, literary, and 
exotic scenes. Works such as *Liberty Leading the People* and *The Death of Sardanapalus* exemplify 
his dynamic compositions and bold palette. Deeply influential on later artists, including the 
Impressionists, Delacroix helped redefine painting as a vehicle for emotional and imaginative 
expression in 19th-century art."


bio_seurat <- "Georges Seurat (1859–1891) was a French Post-Impressionist painter and theorist, 
best known for developing the technique of pointillism, in which small dots of color create 
luminous, cohesive images. Working during the late 19th century, Seurat focused on landscapes, 
urban scenes, and leisure activities, combining scientific color theory with precise composition. 
Notable works such as *A Sunday Afternoon on the Island of La Grande Jatte* and *Bathers at 
Asnières* showcase his innovative approach. Seurat’s methodical, analytical style laid the 
foundation for modern Neo-Impressionism and influenced subsequent generations of artists exploring 
color and perception."


bio_paulrubens <- "Peter Paul Rubens (1577–1640) was a Flemish Baroque painter renowned for his 
dynamic compositions, vibrant color, and dramatic energy. Active primarily in Antwerp, Rubens 
created religious altarpieces, mythological scenes, and grand portraits characterized by movement, 
sensuality, and grandeur. Works such as *The Descent from the Cross* and *The Garden of Love* 
exemplify his masterful use of color, light, and form. Highly influential across Europe, Rubens 
combined Italian Renaissance techniques with Northern European detail, establishing himself as one 
of the most important painters of the 17th century."


bio_rousseau <- "Théodore Rousseau (1812–1867) was a French painter and leading figure of the 
Barbizon School, known for his naturalistic landscapes and atmospheric depictions of the French 
countryside. Rejecting academic convention, Rousseau focused on the truthful observation of light, 
texture, and season, capturing forests, rivers, and rural life with poetic realism. Notable works 
such as *The Forest of Fontainebleau* and *Sunset in the Forest* demonstrate his deep sensitivity to 
nature. Rousseau’s dedication to painting directly from the landscape helped lay the groundwork for 
later Impressionists and the evolution of modern landscape painting."


### Compile into DF
df_artist_bios <- tibble(
  artist=c("Manet, Edouard", "van Gogh, Vincent", "Degas, Edgar", "van Rijn, Rembrandt", 
           "Gauguin, Paul", "Cézanne, Paul", "Theotokopoulos, Domenikos", "David, Gerard",
           "Memling, Hans", "Vermeer, Johannes", "Christus, Petrus", "Falca, Pietro",
           "Delacroix, Eugène", "Seurat, Georges", "Rubens, Peter Paul", "Rousseau, Théodore"),
  bio=c(bio_manet, bio_vangogh, bio_degas, bio_rembrandt, bio_gauguin, bio_cezanne,
        bio_theo, bio_david, bio_memling, bio_vermeer, bio_christus, bio_falca, bio_delacroix,
        bio_seurat, bio_paulrubens, bio_rousseau)
) %>%
  mutate(artist_simple=convert_artist_name(artist))



### Join artist info and bios with art info for complete df
#### Read in art info (for public domain art)
fp_art_explore <- grab_newest_fp(dir=here("data"), patt="^00_art-exploration")

df_artist_info0 <- readRDS(fp_art_explore) 
df_artist_info_public <- df_artist_info0 %>%
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
  left_join(df_artist_info) %>%
  arrange(movement, nationality, artist) %>%
  select(all_of(cols_art_info)) %>%
  #filter for available images
  mutate(image_available = map_lgl(image_url, image_ok)) %>%
  filter(image_available) %>%
  select(!image_available)



# 03: Game==========================================================================================
## Read in clean, feature-extracted app data for game and model & pull in image_url
fp_game <- grab_newest_fp(dir=here("data"),
                          patt="^03_app-feat_")

df_game0 <- readRDS(fp_game)

df_game <- df_artist_info0 %>%
  select(object_id, image_url) %>%
  inner_join(df_game0) %>%
  mutate(image_available = map_lgl(image_url, image_ok)) %>%
  filter(image_available) %>%
  select(!image_available)


## Read in full, raw app data for modal table
### Create vectors for subsetting and naming fields in app
vec_modal <- c("object_id", "nationality", "title", "creation_dates", "medium", "dims_clean")
vec_modal_easy <- vec_modal[vec_modal!="object_id"]
vec_modal_normal <- vec_modal[!vec_modal %in% c("object_id", "nationality")]

labs_modal <- c("object_id", "Artist's Nationality", "Title of Artwork", "Creation Date(s)",
                "Medium", "Dimensions")
labs_modal_easy <- labs_modal[labs_modal!="object_id"]
labs_modal_normal <- labs_modal[!labs_modal %in% c("object_id", "Artist's Nationality")]


### Wrangle data for cleaner display in app
df_modal <- df_artist_info_public %>%
  filter(object_id %in% df_game0$object_id) %>%
  mutate(creation_dates=ifelse(date_start==date_end,
                               date_start,
                               paste(date_start, date_end, sep="-")),
         dims_clean=str_remove_all(dimensions, "\\s*\\([^\\)]*?cm\\)")) %>%
  select(all_of(vec_modal)) %>%
  set_names(labs_modal)





  