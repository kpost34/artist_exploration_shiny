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
    
    ## First row of artwork
    # fluidRow(
    #   1:3 %>%
    #     purrr::map(build_q_a_block)
    # ),
    fluidRow(
      build_q_a_block(id, n=1)
    ),
    
    
    # fluidRow(
    #   column(4,
    #     imageOutput(ns("out_img_art1")),
    #     uiOutput(ns("ui_answer_art1")),
    #     textOutput(ns("txt_answer_msg_art1"))
    #   ),
    #   column(4,
    #     imageOutput(ns("out_img_art2")),
    #     uiOutput(ns("ui_answer_art2"))
    #   ),
    #   column(4,
    #     imageOutput(ns("out_img_art3"))
    #   )
    # ),
    
    ## Second row of artwork + submit button
    fluidRow(
      column(4,
        imageOutput(ns("out_img_art4"))
      ),
      column(4,
        imageOutput(ns("out_img_art5"))
      ),
      column(2),
      column(2,
        #this shouldn't display until all answers are made
        # uiOutput(ns("ui_btn_answers"), "Submit")
        actionButton(ns("btn_answers"), "Submit")
      )
    )
  )
  
}



# Server============================================================================================
gameServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    ## Conditionally display response UI
    
    
    ## Conditionally display answer submit button
    # observeEvent({input$} , {
    #   output$ui_btn_answers <- renderUI({
    # 
    #   })
    # })
    
    
  })
}




