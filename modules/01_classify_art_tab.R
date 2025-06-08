# Classify Art Module

# UI================================================================================================
classifyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Classify User-uploaded Artwork"),
    fluidRow(
      column(5,
        br(),
        ## Instructions
        accordion(
          accordion_panel(
            "Instructions",
            p(txt_classify1,
            p(em(txt_classify2))
            )
          ),
          open=FALSE,
          id="classify_art_acc")
      ),
      column(1),
      column(6,
        ## Image file input
        fileInput(ns("file_img"), "Select an image file")
      )
    ),
    br(),
    hr(),
    fluidRow(
      ## Image upload preview
      column(1),
      column(4,
        br(),
        h4(strong("Sample Artwork")),
        imageOutput(ns("out_img_artwork"),
                    height="300px", 
                    width="300px")
      ),
      column(1),
    
      ## Choose identification options
      column(6,
        br(),
        #top n artists
        sliderInput(ns("sld_n"), "Choose top x artists", 
                    min=1, value=3, max=5, step=1),
        br(),
        br(),
        
        #percent breakdown
        radioButtons(ns("rad_pct"), "Include percent confidence?",
                     choices=c("Yes", "No"), selected="No"
        )
      )
    ),
    br(),
    ## Tabular output
    fluidRow(
      DTOutput(ns("tab_artist"))
    )
  )
}



# Server============================================================================================
classifyServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## Image rendering/display
    output$out_img_artwork <- renderImage({
      req(input$file_img)
      if(is.null(input$file_img)) {
        return(NULL)
      }

      file_path <- input$file_img$datapath

      list(src = file_path,
           height=300, 
           width=400,
           alt = "Uploaded Image")
    }, deleteFile = FALSE)
  
    
    ## Run algorithm
    #placeholder for now
    df <- reactive({
      req(input$file_img)
      tibble(
        rank=1:5,
        artist=c("Van Gogh, Vincent", "Monet, Claude", "Picasso, Pablo", "Manet, Édouard", "Dalí, Salvador"),
        nationality=c("Dutch", "French", "Spanish", "French", "Spanish"),
        pct_confidence=c(90, 5, 2, 2, 1)
      )
    })
    
    
    ## Filter output
    df_filt <- reactive({
      req(df())
      df() %>%
        filter(rank <= input$sld_n) %>%
        {if(input$rad_pct=="No") select(., !pct_confidence) 
          else if(input$rad_pct=="Yes") .}
    })
    
    
    
    ## Run algorithm & generate output
    output$tab_artist <- renderDT({
      req(df_filt())
      DT::datatable(df_filt(),
                    rownames=FALSE,
                    option=list(dom="t"))
    })
    
  })
}




