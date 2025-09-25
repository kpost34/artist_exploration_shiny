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
        uiOutput(ns("ui_txt_artwork")),
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
        radioButtons(ns("rad_prob"), "Include probabilities?",
                     choices=c("Yes", "No"), selected="Yes"
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
classifyServer <- function(id, mod) {
  moduleServer(id, function(input, output, session) {
    
    ## Image rendering/display & title
    #image
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
    
    #title
    output$ui_txt_artwork <- renderUI({
      req(input$file_img)
      h4(
        strong("Upoaded Artwork")
      )
    })
  
    
    ## Run algorithm
    ### Process image and extract features
    df_feat <- reactive({
      req(input$file_img)
      input$file_img$datapath %>%
        process_loaded_image() %>%
        extract_final_rgb_feat()
      # tibble(
      #   rank=1:5,
      #   artist=c("Van Gogh, Vincent", "Monet, Claude", "Picasso, Pablo", "Manet, Édouard", "Dalí, Salvador"),
      #   nationality=c("Dutch", "French", "Spanish", "French", "Spanish"),
      #   pct_confidence=c(90, 5, 2, 2, 1)
      # )
    })
    
    
    ### Make predictions
    df_pred <- reactive({
      req(df_feat())
      df_feat() %>%
        get_top_k_preds_for_artwork(model=mod, k=input$sld_n)
    })
    
    
    ## Filter output
    df_pred_filt <- reactive({
      req(df_pred())
      df_pred() %>%
        # filter(rank <= input$sld_n) %>%
        {if(input$rad_prob=="No") select(., !probability) 
          else if(input$rad_prob=="Yes") .}
    })
    
    
    
    ## Run algorithm & generate output
    output$tab_artist <- renderDT({
      req(df_pred_filt())
      DT::datatable(df_pred_filt(),
                    rownames=FALSE,
                    option=list(dom="t"))
    })
    
  })
}




