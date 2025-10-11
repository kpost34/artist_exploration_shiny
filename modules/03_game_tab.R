# Game Module

# UI================================================================================================
gameUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Can you Beat the Computer?"),
    
    fluidRow(
      column(5, 
        br(),
             
        ## Instructions
        accordion(
          accordion_panel(
            "Instructions",
            p(txt_game1,
            p(em(txt_game2))
            )
          ),
          open=FALSE,
          id="art_game_acc")
      ),
      column(1),
      column(3, 
        ## Difficulty slider & submit button
        sliderTextInput(ns("sldT_diff"), "Select your difficulty",
                        choices=c("easy", "normal", "hard"),
                        selected="normal")
        #easy = 3 choices, normal = 4 choices, hard = 5 choices
      ),
      column(1),
      column(2, 
        br(),
        actionButton(ns("btn_diff"), "Submit")
      )
    ),
            
    ## Delineation 
    br(),
    hr(),
    
    ## Artwork, choices, and answer response
    fluidRow(
      column(1),
      1:5 %>%
        purrr::map(build_q_a_block, id=id),
      column(1)
    ),
    
    br(),
    
    ## Submit answers
    fluidRow(
      column(5),
      column(2,
        uiOutput(ns("ui_btn_submit_art")),
        br(),
        textOutput(ns("txt_n_correct"))
      ),
      column(5)
    )
  )
}



# Server============================================================================================
gameServer <- function(id, mod, current_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Reactive DFs
    ### Hold shrinking DF of game artwork
    rv_remaining <- reactiveVal(df_game) #seed with initial DF
    
    
    ### Extract sample & retain remaining records
    df_sample <- eventReactive(input$btn_diff, {
      # req(current_tab()=="Game")
      
      df <- rv_remaining()
      req(nrow(df) >= 5)

      #get sample
      sample <- df %>%
        left_join(df_modal, by="object_id") %>%
        slice_sample(n=5)

      #reduce pool of object_ids
      rv_remaining(df %>% filter(!object_id %in% sample$object_id))

      sample
    })
    
    
    ### Create reactive of correct answers with metadata
    df_artist_meta <- reactive({
      df <- df_modal %>%
        filter(object_id %in% df_sample$object_id)
      
      df
    })
    
    
    ### Create vector of wrong answers
    vec_wrong_artists <- reactive({
      df_game %>%
        anti_join(df_sample(), by="artist_clean") %>%
        pull(artist_clean) %>%
        unique()
    })
    
    
    ## Display blocks of UI
    ### Pre-process images
    input_ids <- paste0("input_answer_art", 1:5)
    # vec_correct_answers <- c("Monet", "Dali", "Monet", "Manet")
    vec_answers_msg <- paste0("txt_answer_msg_art", 1:5)
    
    
    ### Render images
    observeEvent(df_sample(), {
      
      urls <- df_sample() %>%
        pull(image_url)
      
      purrr::walk(1:5, function(x) {
        local({
          nm_image <- paste0("out_img_art", x)
          url <- urls[x]
          
          output[[nm_image]] <- renderUI({
            tags$img(
              src = url,
              style = "height: 200px; width: 100%; object-fit: contain; display: block; margin: 10px auto;",
              # style = "margin: 10px; max-width: 100%; height: auto; max-height: 200px",
              alt = paste("Artwork", x)
            )
          })
        })
      })
    })
    
    
    ### Modals
    #### Display buttons to show modals
    observeEvent(input$btn_diff, {
      1:5 %>%
        purrr::map(function(x) {
          #create outputs and inputs
          nm_input_btn <- paste0("btn_modal_art", x)
          nm_output_btn <- paste0("ui_btn_modal_art", x)
          
          #build ui
          output[[nm_output_btn]] <- renderUI({
            actionButton(ns(nm_input_btn),
                         label="See information")
          })
        })
    })
    
    
    #### Render tables for modals
    observeEvent(df_sample(), {
      purrr::map(1:5, function(x) {
        nm_dt_meta <- paste0("dt_art_meta", x)
        df_meta <- extract_rotate_meta(df=df_sample(),
                                       row=x,
                                       fields=vec_modal_easy)

        output[[nm_dt_meta]] <- DT::renderDataTable({
          DT::datatable(df_meta)
        })
      })
    })
    
    
    #### Display modals
    purrr::map(1:5, function(x) {
      observeEvent(input[[paste0("btn_modal_art", x)]], {
        nm_modal_table <- paste0("dt_art_meta", x)
        
        showModal(modalDialog(
          title=paste("Modal for button", x),
          DT::dataTableOutput(ns(nm_modal_table)),
          # p(paste("You clicked button number", x, ". This is the modal for it.")),
          footer=modalButton("Close"),
          easyClose=TRUE
        ))
      })
    })
    
    
    ### Conditionally display answer (choices) UI
    observeEvent(input$btn_diff, {
      purrr::map(1:5, function(x) {
        #create outputs and inputs
        nm_input_answer <- paste0("input_answer_art", x)
        nm_output_answer <- paste0("ui_answer_art", x)
      
        #grab right answer
        answer_right <- df_sample()$artist_clean[x]
          
        
        #create choices per question
        if(input$sldT_diff=="easy"){
          n <- 2
        } else if(input$sldT_diff=="normal"){
          n <- 3
        } else if(input$sldT_diff=="hard"){
          n <- 4
        }
        
        vec_wrong <- sample(vec_wrong_artists(), n)
        
        vec_choices <- sample(
          c(answer_right, vec_wrong),
          n+1
        )

        #radio button input
        output[[nm_output_answer]] <- renderUI({
          radioButtons(ns(nm_input_answer), 
                       "Choose artist",
                       choices=vec_choices,
                       selected=character(0)
          )
        })
      })
    })
    
    
    ### Conditionally display answer submit button
    #### Create reactive of answers being selected
    all_selected <- reactive({
      #combine answers and replace default (NULL) with empty string
      map_chr(input_ids, ~ input[[.x]] %||% "")
    })
    
    
    ### Display submit button when all radio buttons are selected/text inputs have any characters
    observeEvent(all_selected(), {
      selected_answers <- all_selected()

      #show button only all inputs have a selection or character
      if(all(nzchar(selected_answers))) {
        output$ui_btn_submit_art <- renderUI({
          actionButton(ns("btn_submit_art"), "Submit answers")
        })
      } else {
        output$ui_btn_submit_art <- renderUI(NULL)  #hide if not all selected
      }
    })
    
    
    ## Evaluate whether answers are correct
    ### Display number correct below submit button
    #### Create reactive of each answer assessment
    vec_answers_assess <- reactive({
      req(input$btn_submit_art)
      all_selected()==df_sample()$artist_clean
    })
    
    
    #### Render output
    observeEvent(input$btn_submit_art, {
      req(vec_answers_assess())
      
      purrr::map(1:5, function(x) {
        output[[vec_answers_msg[x]]] <- renderUI({
          if(vec_answers_assess()[x]) {
            HTML("<span style='color: green;'>Correct!</span>")
          } else{
            HTML("<span style='color: red;'>Wrong</span>")
          }
        })
          # output[[vec_answers_msg[x]]] <- renderText({
          #   vec_text_msg[x]
          # })
      })
    })
    
    
    ### Display correct/incorrect below each response/artwork
    #### Create reactive
    n_correct <- reactive({
      req(vec_answers_assess())
      sum(vec_answers_assess())
    })
    
    
    #### Render output
    output$txt_n_correct <- renderText({
      req(n_correct())
      obj <- if(n_correct()==1){
        "painting"
      } else if(n_correct()!=1){
        "paintings"
      }
      paste("You identified", n_correct(), obj, "correctly")
    })
    
    
  })
}




