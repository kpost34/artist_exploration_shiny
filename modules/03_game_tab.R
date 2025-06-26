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
        #easy = 3 choices, normal = 5 choices, hard = text entry
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
gameServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    input_ids <- paste0("input_answer_art", 1:5)
    vec_correct_answers <- c(rep("Van Gogh", 3), "Monet", "Dali")
    vec_answers_msg <- paste0("txt_answer_msg_art", 1:5)
  
    ## Conditionally display answer (choices) UI
    observeEvent(input$btn_diff, {
        1:5 %>%
          purrr::map(function(x) {
            #create outputs and inputs
            nm_input_answer <- paste0("input_answer_art", x)
            nm_output_answer <- paste0("ui_answer_art", x)
          
            #easy and normal cases
            if(input$sldT_diff %in% c("easy", "normal")) {
              
              vec_artist_choices <- if(input$sldT_diff=="easy") {
                vec_artists[1:3]
              } else if(input$sldT_diff=="normal"){
                vec_artists
              }
              #radio button input
              output[[nm_output_answer]] <- renderUI({
                radioButtons(ns(nm_input_answer), 
                             "Choose artist",
                             choices=vec_artist_choices,
                             selected=character(0)
                )
              })
              #hard case
              } else if(input$sldT_diff=="hard"){
              
                #text input
                output[[nm_output_answer]] <- renderUI({
                  textInput(ns(nm_input_answer),
                            "Enter artist's name")
                })
              }
          })
    })
    
    
    
    ## Conditionally display answer submit button
    ### Create reactive of answers being selected
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
    vec_answers_determination <- reactive({
      req(input$btn_submit_art)
      all_selected()==vec_correct_answers
    })
    
    
    #### Render output
    observeEvent(input$btn_submit_art, {
      vec_text_msg <- map_chr(vec_answers_determination(),
                              ~ifelse(.x, "Correct!", "Wrong")
      )
      
      1:5 %>% 
        map(function(x) {
          output[[vec_answers_msg[x]]] <- renderText({
            vec_text_msg[x]
          })
      })
    })
    
    
    ### Display correct/incorrect below each response/artwork
    #### Create reactive
    n_correct <- reactive({
      req(vec_answers_determination())
      sum(vec_answers_determination())
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




