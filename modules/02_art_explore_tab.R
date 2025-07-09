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
      h3(strong(textOutput(ns("out_txt_artist")))),
    ),
    fluidRow(
      column(4,
        p(textOutput(ns("out_txt_bio"))),
        tags$p(textOutput(ns("out_txt_citation")),
          style = "font-size: 0.8em; color: gray; font-style: italic;")
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
  

  ## Update dropdown menus
  ### Create reactive of DF
  df_art <- reactive({
    df_art_info_full
  })
  # df_artist <- reactive({
  #   df_artist_info
  #   #note: later replace with connection to API
  # })


  observeEvent(input$sel_movement, {
    req(input$sel_movement)
    df_filt <- filter(df_art(), movement==input$sel_movement)
      
    vec_natl_update <- unique(df_filt$nationality)

    vec_artist1_update <- unique(df_filt$artist)

    updateSelectInput(session,
                      'sel_movement',
                      selected=input$sel_movement
    )

    updateSelectInput(session,
                      'sel_nationality',
                      choices=c("Select one"="", vec_natl_update)
    )

    updateSelectInput(session,
                      'sel_artist',
                      choices=c("Select one"="", vec_artist1_update)
    )
  })

  observeEvent(input$sel_nationality, {
    req(input$sel_nationality)
    df_filt <- filter(df_art(), nationality==input$sel_nationality)

    vec_artist2_update <- unique(df_filt$artist)


    updateSelectInput(session,
                      'sel_nationality',
                      selected=input$nationality
    )

    updateSelectInput(session,
                      'sel_artist',
                      choices=c("Select one"="", vec_artist2_update)
    )
  })

  
  ## Artist Name
  ### Render output
  output$out_txt_artist <- renderText({
    format_name(input$sel_artist)
  })
  
  
  ## Artist bio
  ### Create reactive
  txt_bio <- reactive({
    req(input$sel_artist)
    df_art() %>%
      filter(artist==input$sel_artist) %>%
      pull(bio) %>%
      unique()
  })
  
  
  ### Render output
  output$out_txt_bio <- renderText({
    txt_bio()
  })
  
  output$out_txt_citation <- renderText({
    req(input$sel_artist)
    "Bio generated with the assistance of ChatGPT (OpenAI, 2025)."
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
  
  
  ## Artworks
  output$carousel_sample <- renderSlickR({
    req(input$sel_artist)
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




