# Art Exploration Module

# UI================================================================================================
exploreUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Explore Artwork from Well-Known European and North American Painters"),
    br(),
    
    fluidRow(
      column(5,
      ## Instructions
      accordion(
        accordion_panel(
          "Instructions",
          p(txt_explore)
        ),
        open=FALSE,
        id="explore_art_acc")
      ),
      column(7)
    ),
    br(),
    br(),
    
    ## Choose art movement, nationality, and artist
    fluidRow(
      column(4,
        selectInput(ns("sel_movement"), "Select movement", 
                    choices=c("Select one"="", unique(sort(df_artist_info$movement))),
                    selected=""),
      ),
      column(4,
        selectInput(ns("sel_nationality"), "Select nationality",
                    choices=c("Select one"="", unique(sort(df_artist_info$nationality))),
                    selected="")
      ),
      column(4,
        selectInput(ns("sel_artist"), "Select artist", 
                    choices=c("Select one"="", unique(sort(df_artist_info$artist))),
                    selected="")
      )
    ),
    
    ## Delineation
    br(),
    hr(),
    br(),
    
    ## Outputs
    fluidRow(
        h3(strong(textOutput(ns("out_txt_artist"))))
    ),
    fluidRow(
      column(3,
        imageOutput(ns("out_img_artist"))
      ),
      column(1),
      column(3,
        h4(textOutput(ns("out_txt_bio_title"))),
        p(textOutput(ns("out_txt_bio_body")))
      ),
      column(4,
        slickROutput(ns("carousel_sample"))
        # imageOutput(ns("out_img_artwork"))
      )
    )
  )

  
}



# Server============================================================================================
exploreServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
  ## Artist Name
  ### Create reactive
    
    
    
  ### Render output
  output$out_txt_artist <- renderText({
    input$sel_artist %>%
      str_split_1(pattern=", ") %>%
      rev() %>%
      paste(collapse=" ")
  })
    
    
    
  ## Image rendering/display
  # output$img_artist <- renderImage({
  #   req(input$file_img)
  #   if(is.null(input$file_img)) {
  #     return(NULL)
  #   }
  # 
  #   file_path <- input$file_img$datapath
  # 
  #   list(src = file_path,
  #        alt = "Uploaded Image")
  # }, deleteFile = FALSE)
  
    
  ## Artist bio
  output$txt_bio <- renderText({
    "Artist bio goes here"
  })
  
  
  ## Artworks
  output$carousel_sample <- renderSlickR({
    # Provide image URLs or file paths
    imgs <- list(
      img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Shaki_waterfall.jpg/640px-Shaki_waterfall.jpg", height = "300px"),
      img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a1/Nepal_Mount_Everest.jpg/640px-Nepal_Mount_Everest.jpg", height = "300px"),
      img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0f/Eiffel_Tower_in_Paris.jpg/640px-Eiffel_Tower_in_Paris.jpg", height = "300px")
    )
    slickR(imgs)
  })
    
    
  })
}




