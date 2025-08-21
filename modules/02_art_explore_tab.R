# Art Exploration Module

# UI================================================================================================
exploreUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Explore Artwork from Well-Known European Painters"),
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
      column(6, 
        #artist name, bio, and citation
        h3(strong(textOutput(ns("out_txt_artist")))),
        p(uiOutput(ns("out_txt_bio"))),
        p(uiOutput(ns("out_txt_citation")))
      ),
      column(1),
      column(5,
        #carousel of artwork with caption below
        slickROutput(ns("carousel_artwork"))
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
      df_art_info_public_full
    })
  
  
    ### Update dropdowns
    #movement
    observeEvent(input$sel_movement, {
      req(input$sel_movement != "")
      # req(input$sel_movement)
      df_filt <- filter(df_art(), movement==input$sel_movement)
        
      vec_natl_update <- unique(df_filt$nationality)
  
      vec_artist1_update <- unique(df_filt$artist)
  
      updateSelectInput(session,
                        'sel_nationality',
                        choices=c("Select one"="", vec_natl_update)
      )
  
      updateSelectInput(session,
                        'sel_artist',
                        choices=c("Select one"="", vec_artist1_update)
      )
    })
  
    #nationality
    observeEvent(input$sel_nationality, {
      req(input$sel_nationality != "")
      # req(input$sel_nationality)
      df_filt <- filter(df_art(), 
                        nationality==input$sel_nationality) %>%
        {if(input$sel_movement != "") filter(., movement==input$sel_movement) else .}
  
      vec_artist2_update <- unique(df_filt$artist)
  
      updateSelectInput(session,
                        'sel_artist',
                        choices=c("Select one"="", vec_artist2_update)
      )
    })
    
    
    ## Create selected artist reactive DF
    df_art_sel <- reactive({
      req(input$sel_artist != "")
      
      df_art() %>%
        filter(artist==input$sel_artist) 
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
      
      df_art_sel() %>%
        filter(artist==input$sel_artist) %>%
        pull(bio) %>%
        unique()
    })
    
    
    ### Render output
    #bio
    output$out_txt_bio <- renderUI({
      markdown(txt_bio())
    })
    
    #citation
    output$out_txt_citation <- renderUI({
      req(input$sel_artist)
      em("Bio generated with the assistance of ChatGPT (OpenAI, 2025).")
    })
    
      
    ## Image rendering/display
    ### Create reactive of urls
    df_art_sel_info <- reactive({
      req(input$sel_artist)
      
      mvmt <- df_art_sel()$movement[1]
      
      df_art_sel() %>%
        filter(movement==mvmt) %>%
        slice_sample(n=3) %>%
        select(artist_simple, title, date, image_url) 
    })
    
    
    ## Artworks
    output$carousel_artwork <- renderSlickR({
      #create vectors for building carousel
      imgs <- df_art_sel_info() %>% pull(image_url)
      
      artist <- df_art_sel_info() %>% pull(artist_simple)
      
      titles <- df_art_sel_info() %>% pull(title)
      
      dates <- df_art_sel_info() %>% 
        pull(date) %>%
        paste0("(", ., "),")
      
      closing <- "The Metropolitan Museum of Art. Open Access. Image courtesy of The Met."
        
      #combine vectors to generate caption vector
      captions <- paste(" by", 
                       artist,
                       dates,
                       closing)
      
      #build slides for carousel
      slides <- purrr::pmap(list(imgs, titles, captions), function(x, y, z) {
        tags$div(
          style = "text-align: center;",
          tags$img(
            src = x,
            style = "max-width: 100%; height: 300px; display: block; margin: auto;"
          ),
          tags$p(
            tags$em(y),
            z, 
            style = "margin-top: 10px; font-size: 14px; color: #444;")
        )
      })
       
      #generate carousel
      slickR(slides)
  
    })
  })
}




