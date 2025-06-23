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
        uiOutput(ns("ui_btn_submit_art"))
      ),
      column(5)
    )
  )
}



# Server============================================================================================
gameServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    ## Conditionally display answer (choices) UI
    observeEvent(input$btn_diff, {
        1:5 %>%
          purrr::map(function(x) {
            #create outputs and inputs
            nm_output_answer <- paste0("ui_answer_art", x)
          
            #easy and normal cases
            if(input$sldT_diff %in% c("easy", "normal")) {
              nm_input_rad <- paste0("rad_answer_art", x)
              
              vec_artist_choices <- if(input$sldT_diff=="easy") {
                vec_artists
              } else if(input$sldT_diff=="normal"){
                vec_artists[1:3]
              }
              #radio button input
              output[[nm_output_answer]] <- renderUI({
                radioButtons(nm_input_rad, 
                             "Choose artist",
                             choices=vec_artist_choices,
                             selected=character(0)
                )
              })
              #hard case
              } else if(input$sldT_diff=="hard"){
                nm_input_txt <- paste0("txt_answer_art", x)
              
                #text input
                output[[nm_output_answer]] <- renderUI({
                  textInput(nm_input_txt,
                            "Enter artist's name")
                })
              }
          })
    })
    
    
    
    ## Conditionally display answer submit button
    observeEvent(input$ui_answer_art1, {
      output$ui_btn_submit_art <- renderUI({
        actionButton("btn_submit_art", "Submit answers")
      })
    })
    
    
    ## Evaluate whether answers are correct
    ### Display number correct below submit button
    
    
    ### Display correct/incorrect below each response/artwork
    
  })
}




