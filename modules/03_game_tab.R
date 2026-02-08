# Game Module

# UI================================================================================================
gameUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Can you Beat the Computer?"),
    
    fluidRow(
      column(4, 
        br(),
             
        ## Instructions
        accordion(
          accordion_panel(
            "Instructions",
            p(txt_game1),
            p(txt_game2),
            p(txt_game3)
          ),
          open=FALSE,
          id="art_game_acc")
      ),
      column(2, 
        ## Difficulty slider
        uiOutput(ns("ui_txt_diff")),
        sliderTextInput(ns("sldT_diff"), "Difficulty",
                        choices=c("easy", "normal", "hard"),
                        selected="normal")
      ),
      column(1),
      column(2,
        ## Game length slider
        uiOutput(ns("ui_txt_pts")),
        sliderInput(ns("sld_pts"), "Points to Win", 
                    min=3, max=9, value=6)
      ),
      column(1),
      column(1, 
        br(),
        actionButton(ns("btn_round"), "Start Game", class="btn-success")
      ),
      column(1,
        br(),
        uiOutput(ns("ui_btn_reset_game"))
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
        hidden(
          actionButton(ns("btn_submit_art"), "Confirm selections", class="btn-success")
        )
      ),
      column(6,
        textOutput(ns("txt_n_mod_correct"))
      )
    ),
    tags$hr(),
    tags$footer(
      style = "text-align: center; color: #777; font-size: 0.9em; padding: 10px;",
      "Artwork provided by The Metropolitan Museum of Art. Open Access. Images courtesy of The Met."
    )
  )
}



# Server============================================================================================
gameServer <- function(id, mod) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    ## Create static values-------------------
    vec_answers_msg <- paste0("txt_answer_msg_art", 1:5)
    vec_mod_answers <- paste0("txt_mod_answer_art", 1:5)
    vec_mod_answers_msg <- paste0("txt_mod_answer_msg_art", 1:5)
    vec_correct_answers <- paste0("txt_correct_answer", 1:5)
    
    intro_model <- c("Model selections:", rep("<br>", 4))
    intro_answer <- c("Correct answers:", rep("<br>", 4))
    
    
    ## Seed reactive values-------------------
    #seed with initial DF
    rv_remaining <- reactiveVal(df_game)
    
    #seed with 0s and submitted set to FALSE
    rv <- reactiveValues(
      user_score=0,
      mod_score=0,
      submitted=FALSE
    )
    
    
    ## Reset Game Button-------------------
    observeEvent(input$btn_reset_game, {
      #difficulty text disappears & slider returns 
      shinyjs::hide("ui_txt_diff")
      shinyjs::show("sldT_diff")
      
      #points to win disappears & slider returns
      shinyjs::hide("ui_txt_pts")
      shinyjs::show("sld_pts")
      
      #show new game button
      shinyjs::show("btn_round")
      
      #resets dataframe
      rv_remaining(df_game)  
      
      #reset scores & submitted
      rv$user_score <- 0
      rv$mod_score <- 0
      rv$submitted <- FALSE
      
      purrr::map(1:5, function(x) {
      
        #reset image
        nm_image <- paste0("out_img_art", x)
        output[[nm_image]] <- renderUI({NULL})
      
        #reset modal buttons
        nm_output_btn <- paste0("ui_btn_modal_art", x)
        output[[nm_output_btn]] <- renderUI({NULL})
    
        #reset choices
        nm_output_answer <- paste0("ui_answer_art", x)
        output[[nm_output_answer]] <- renderUI({NULL})
        
        #reset round scores
        output[[vec_mod_answers[x]]] <- renderUI({ NULL })
        output[[vec_correct_answers[x]]] <- renderUI({ NULL })
      })
      
      #update btn_round name
      updateActionButton(session, "btn_round", "Start Game")
    }, ignoreInit=TRUE
    )

    
    ## New Round Button-------------------
    ### Extract sample & retain remaining records
    df_sample <- eventReactive(input$btn_round, {
      
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
              style = "height: 200px; width: auto; max-width: 100%; display: block; margin-left: auto; margin-right: auto",
              alt = paste("Artwork", x)
            )
          })
        })
      })
    })
    
    
    ## Modals-------------------
    ### Display buttons to show modals
    observeEvent(input$btn_round, {
      
      #hide confirm button
      shinyjs::hide("btn_submit_art")
      
      1:5 %>%
        purrr::map(function(x) {
          #create names of outputs and inputs
          nm_input_btn <- paste0("btn_modal_art", x)
          nm_output_btn <- paste0("ui_btn_modal_art", x)
          
          #build ui
          output[[nm_output_btn]] <- renderUI({
            req(input$sldT_diff %in% c("easy", "normal"))
            
            div(style = "text-align: center;",
                actionButton(ns(nm_input_btn),
                             label = "See information",
                             class = "btn-info")
            )
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
          DT::DTOutput(ns(nm_modal_table)),
          footer=modalButton("Close"),
          easyClose=TRUE
        ))
      })
    })
    
    
    ## Answer choices and assessment-------------------
    ### Conditionally display answer (choices) UI
    observeEvent(input$btn_round, {
      #hide start game/next round button temporarily
      shinyjs::hide("btn_round")
      
      #reset submission state at start of round
      rv$submitted <- FALSE
      
      #clear displayed answers & scores
      purrr::walk(1:5, function(x) {
        output[[vec_mod_answers[x]]] <- renderUI({ NULL })
        output[[vec_correct_answers[x]]] <- renderUI({ NULL })
      })
      
      #show difficulty selected & hide slider
      output$ui_txt_diff <- renderUI({
        HTML(
          paste0("Difficulty: ", 
                 "<strong>", str_to_sentence(input$sldT_diff), "</strong>")
        )
      })
      shinyjs::show("ui_txt_diff")
      shinyjs::hide("sldT_diff")
      
      #show points to win selected & hide slider
      output$ui_txt_pts <- renderUI({
        HTML(
          paste0("Points to win: ", 
                 "<strong>", input$sld_pts, "</strong>")
        )
      })
      shinyjs::show("ui_txt_pts")
      shinyjs::hide("sld_pts")
      
      #display reset button
      output$ui_btn_reset_game <- renderUI({
        actionButton(ns("btn_reset_game"), "Reset game", class="btn-danger")
      })
      
      #hide submit button
      shinyjs::hide("btn_submit_art")
      
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
    
    
    #### Model: Create reactive vector of choices selected
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
        shinyjs::show("btn_submit_art")
      } else {
        shinyjs::hide("btn_submit_art")
      }
    })
    
    
    ### Submit button clicked
    #### Update scores, show next round button, hide submit button, and update btn_round label
    observeEvent(input$btn_submit_art,{
      rv$submitted <- TRUE 

      #update scores
      rv$user_score <- rv$user_score + n_correct()
      rv$mod_score <- rv$mod_score + n_mod_correct()
      
      #show button
      shinyjs::show("btn_round")
      
      #hide button
      shinyjs::hide("btn_submit_art")
      
      #update button name
      updateActionButton(session, "btn_round", "Next Round")
    }, ignoreInit=TRUE)
    
    
    #### Evaluate whether individual answers are correct
    #user: Create reactive of each answer assessment
    vec_user_answers_assess <- reactive({
      if (!rv$submitted) return(rep(FALSE, 5))
      user_selected()==df_sample()$artist_clean
    })
  
    #model: Create reactive of each answer assessment
    vec_model_answers_assess <- reactive({
      if (!rv$submitted) return(rep(FALSE, 5))
      model_selected()==df_sample()$artist_clean
    })
    
    
    #### Render individual selections & answers
    observeEvent(input$btn_submit_art, {
      req(rv$submitted)

      purrr::map(1:5, function(x) {
        #mod answers
        output[[vec_mod_answers[x]]] <- renderUI({
  
          HTML(intro_model[x],
               paste0("<p>", model_selected()[x], "</p>"))
        })
  
        #correct answers
        output[[vec_correct_answers[x]]] <- renderUI({
          answer <- df_sample()$artist_clean[x]
  
          HTML(intro_answer[x],
               paste0("<p><strong>", answer, "</strong></p>"))
        })
      })
    })
    
    
    ## Count and sum correct answers-------------------
    ### Count numbers of correct selections 
    #user
    n_correct <- reactive({
      input$btn_submit_art
      sum(vec_user_answers_assess())
    })
    
    #model
    n_mod_correct <- reactive({
      input$btn_submit_art
      sum(vec_model_answers_assess())
    })
    
  
    ### Render outputs
    #### Text
    output$txt_n_correct <- renderText({
      req(rv$submitted)
      obj <- if(n_correct()==1) "painting" else "paintings"
      paste("You identified", n_correct(), obj, "correctly")
    })
  
    output$txt_n_mod_correct <- renderText({
      req(rv$submitted)
      obj <- if(n_mod_correct()==1) "painting" else "paintings"
      paste("The model identified", n_mod_correct(), obj, "correctly")
    })
    
    
    #### Tabular
    output$tab_game_score <- renderDT({
      #submitted state is TRUE
      req(rv$submitted)
      
      tibble(
        player=c("user", "model"),
        round=c(n_correct(), n_mod_correct()),
        total=c(rv$user_score, rv$mod_score)
      ) %>%
        DT::datatable(
          rownames=FALSE,
          options=list(dom="t", ordering=FALSE)
        )
    })
    
    
    ## Game ending-------------------
    observeEvent(input$btn_submit_art, {
      req(rv$user_score >= input$sld_pts|rv$mod_score >= input$sld_pts)
      
      #create result msg object
      msg_result <- if(rv$user_score==rv$mod_score) {
        "You tied the model!"
      } else if(rv$user_score > rv$mod_score){
        "You beat the model!"
      } else if(rv$mod_score > rv$user_score){
        "The model beat you!"
      }
      
      #create score msg objects
      msg_user_score <- paste("You:", rv$user_score)
      msg_mod_score <- paste("Model: ", rv$mod_score)
      
      #show modal and contents
      showModal(
        modalDialog(
          title=h4("Game Over"),
          p(strong(msg_result)),
          p("Final Score: "),
          p(msg_user_score),
          p(msg_mod_score),
          footer=actionButton(ns("btn_close_score_modal"), "Close", class="btn-primary"),
          easyClose=FALSE
          )
      )
      
      #hide next round button
      shinyjs::hide("btn_round")
    })
    
    
    ## Post-game ending-------------------
    observeEvent(input$btn_close_score_modal, {
      removeModal()
      
      #difficulty text disappears & slider returns 
      shinyjs::hide("ui_txt_diff")
      shinyjs::show("sldT_diff")
      
      #points to win disappears & slider returns
      shinyjs::hide("ui_txt_pts")
      shinyjs::show("sld_pts")
      
      #update button name
      updateActionButton(session, "btn_round", "New Game")
      
      #hide reset button & show new game button
      shinyjs::hide("btn_reset_game")
      shinyjs::show("btn_round")
      
      
      #reset scores 
      rv$user_score <- 0
      rv$mod_score <- 0
      rv$submitted <- FALSE
      
    })
    
  })
}




