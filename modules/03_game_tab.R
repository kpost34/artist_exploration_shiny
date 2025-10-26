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
      column(1, 
        br(),
        actionButton(ns("btn_start_game"), "New Game")
      ),
      column(1,
        br(),
        uiOutput(ns("ui_btn_next_round"))
      )
    ),
            
    ## Delineation 
    hr(),
    
    ## Artwork, choices, and answer response
    fluidRow(
      purrr::map(1:5, build_q_a_block, id=id),
      column(2,
        DTOutput(ns("tab_game_score"))
      )
    ),
    
    br(),
    
    ## Submit answers
    fluidRow(
      column(4,
        textOutput(ns("txt_n_correct"))
      ),
      column(2,
        uiOutput(ns("ui_btn_submit_art"))
      ),
      column(6,
        textOutput(ns("txt_n_mod_correct"))
      )
    )
  )
}



# Server============================================================================================
gameServer <- function(id, mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Reactive DFs-------------------
    ### Hold shrinking DF of game artwork
    rv_remaining <- reactiveVal(df_game) #seed with initial DF
    
    
    ### Extract sample & retain remaining records
    df_sample <- eventReactive(input$btn_start_game, {
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
    
    
    ### Create vector of wrong answers
    vec_wrong_artists <- reactive({
      df_game %>%
        anti_join(df_sample(), by="artist_clean") %>%
        pull(artist_clean) %>%
        unique()
    })
    
    
    ## Images-------------------
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
    
    
    ## Modals-------------------
    ### Display buttons to show modals
    observeEvent(input$btn_start_game, {
      #no modals on hard mode
      req(input$sldT_diff %in% c("easy", "normal"))
      
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
    
    
    ### Render tables for modals
    observeEvent(df_sample(), {
      #no modals on hard mode
      req(input$sldT_diff %in% c("easy", "normal"))
      
      purrr::map(1:5, function(x) {
        nm_dt_meta <- paste0("dt_art_meta", x)
        
        #dynamically choose labels (attribute names)
        labs_modal_sel <- if(input$sldT_diff=="easy"){
          labs_modal_easy
        } else if(input$sldT_diff=="normal"){
          labs_modal_normal
        }
        
        #turn sample date into singular, transposed DFs
        df_meta <- extract_rotate_meta(df=df_sample(),
                                       row=x,
                                       fields=labs_modal_sel)
        
        #render clean DTs of attribute
        output[[nm_dt_meta]] <- DT::renderDT({
          DT::datatable(df_meta,
                        rownames=FALSE,
                        options=list(dom="t",
                                     ordering=FALSE)
          )
        })
      })
    })
    
    
    ### Display modals
    purrr::map(1:5, function(x) {
      observeEvent(input[[paste0("btn_modal_art", x)]], {
        #no modals on hard mode
        req(input$sldT_diff %in% c("easy", "normal"))
        
        nm_modal_table <- paste0("dt_art_meta", x)
        
        showModal(modalDialog(
          title=paste("Artwork", x),
          DT:DTOutput(ns(nm_modal_table)),
          footer=modalButton("Close"),
          easyClose=TRUE
        ))
      })
    })
    
    
    ## Answer choices and assessment-------------------
    ### Conditionally display answer (choices) UI
    observeEvent(input$btn_start_game, {
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
    
    
    ### Choice selection
    #### User: Create reactive vector of choices selected
    user_selected <- reactive({
      input_ids <- paste0("input_answer_art", 1:5)
      #combine answers and replace default (NULL) with empty string
      map_chr(input_ids, ~ input[[.x]] %||% "")
    })
    
    
    #### Model: Create reactive vector of choice selected
    model_selected <- reactive({
      predict(mod, df_sample()) %>%
        .[[1]] %>%
        as.character()
    })
    
    
    ### Display submit button when all radio buttons are selected/text inputs have any characters
    observeEvent(user_selected(), {
      selected_answers <- user_selected()

      #show button only all inputs have a selection or character
      if(all(nzchar(selected_answers))) {
        output$ui_btn_submit_art <- renderUI({
          actionButton(ns("btn_submit_art"), "Confirm selections")
        })
      } else {
        output$ui_btn_submit_art <- renderUI(NULL)  #hide if not all selected
      }
    })
    
    
    ### Hide submit button once clicked
    observeEvent(input$btn_submit_art, {
      output$ui_btn_submit_art <- renderUI(NULL)
      output$ui_btn_next_round <- renderUI({
        actionButton(ns("btn_next_round"), "Next Round")
      })
    })
    
    
    ### Evaluate whether individual answers are correct
    #### User: Create reactive of each answer assessment
    vec_user_answers_assess <- reactive({
      req(input$btn_submit_art)
      user_selected()==df_sample()$artist_clean
    })
    
    
    #### Model: Create reactive of each answer assessment
    vec_model_answers_assess <- reactive({
      req(input$btn_submit_art)
      model_selected()==df_sample()$artist_clean
    })
    
    
    #### Render output
    observeEvent(input$btn_submit_art, {
      req(vec_user_answers_assess(),
          vec_model_answers_assess()
      )
      
      vec_answers_msg <- paste0("txt_answer_msg_art", 1:5)
      vec_mod_answers <- paste0("txt_mod_answer_art", 1:5)
      vec_mod_answers_msg <- paste0("txt_mod_answer_msg_art", 1:5)
      vec_correct_answers <- paste0("txt_correct_answer", 1:5)
      
      intro_model <- c("Model selections:", rep("<br>", 4))
      
      intro_answer <- c("Correct answers:", rep("<br>", 4))
      
      
      purrr::map(1:5, function(x) {
        #mod answers
        output[[vec_mod_answers[x]]] <- renderUI({
          
          HTML(intro_model[x],
          # HTML(paste0("<p>", intro_model[x], "</p>"),
               paste0("<p>", model_selected()[x], "</p>"))
        })
        
        #correct answers
        output[[vec_correct_answers[x]]] <- renderUI({
          answer <- df_sample()$artist_clean[x]
          
          HTML(intro_answer[x],
          # HTML(paste0("<p>", intro_answer[x], "</p>"),
               paste0("<p><strong>", answer, "</strong></p>"))
        })
        
        #user answer right/wrong
        # output[[vec_answers_msg[x]]] <- renderUI({
        #   if(vec_user_answers_assess()[x]) {
        #     HTML("<span style='color: green;'>Correct!</span>")
        #   } else{
        #     HTML("<span style='color: red;'>Wrong</span>")
        #   }
        # })
        
        #mod answer right/wrong
        # output[[vec_mod_answers_msg[x]]] <- renderUI({
        #   if(vec_model_answers_assess()[x]) {
        #     HTML("<span style='color: green;'>Correct!</span>")
        #   } else{
        #     HTML("<span style='color: red;'>Wrong</span>")
        #   }
        # })
      })
    })
    
    
    ### Count numbers of correct selections 
    #### Create recent round reactives
    n_correct <- reactive({
      # req(vec_user_answers_assess())
      sum(vec_user_answers_assess())
    })
    
    
    n_mod_correct <- reactive({
      # req(vec_model_answers_assess())
      sum(vec_model_answers_assess())
    })
    
    
    #### Create total reactives
    #seed with 0s
    rv_user_score <- reactiveVal(0)
    rv_mod_score <- reactiveVal(0)
    
    #get totals
    observeEvent(input$btn_submit_art, {
      # req(n_correct(), n_mod_correct())
      #user
      current_val <- rv_user_score()
      rv_user_score(current_val + n_correct())
      
      #model
      current_mod_val <- rv_mod_score()
      rv_mod_score(n_mod_correct() + current_mod_val)
    })
    
    
    
    #### Render outputs
    ##### Text
    output$txt_n_correct <- renderText({
      req(n_correct())
      obj <- if(n_correct()==1){
        "painting"
      } else if(n_correct()!=1){
        "paintings"
      }
      paste("You identified", n_correct(), obj, "correctly")
    })
  
    output$txt_n_mod_correct <- renderText({
      req(n_mod_correct())
      obj <- if(n_mod_correct()==1){
        "painting"
      } else if(n_mod_correct()!=1){
        "paintings"
      }
      paste("The model identified", n_mod_correct(), obj, "correctly")
    })
    
    
    ##### Tabular
    output$tab_game_score <- renderDT({
      req(n_correct(), n_mod_correct())
      
      df_score <- tibble(
        player=c("user", "model"),
        round=c(n_correct(), n_mod_correct()),
        total=c(rv_user_score(), rv_mod_score())
      )
      
      DT::datatable(df_score,
                    rownames=FALSE,
                    options=list(dom="t",
                                 ordering=FALSE)
      )
    })
    
  })
}




