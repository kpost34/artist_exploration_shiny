# Artist Exploration Shiny App
## App code

# Load Packages, Source Scripts, & Models===========================================================
## Load packages
pacman:: p_load(shiny, shinyjs, bslib, here, tidyverse, janitor, DT, tidymodels, httr, shinyWidgets,
                slickR, magick, e1071)


## Load functions and objects
here("fns_objs") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)


## Load modules
here("modules") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)


## Load models
### Model 1
fp_mod1 <- grab_newest_fp(here("models"), patt="01_")
mod1 <- readRDS(fp_mod1)


### Model 3
fp_mod3 <- grab_newest_fp(here("models"), patt="03_")
mod3 <- readRDS(fp_mod3)



# Create and Run App================================================================================
artistExplorationApp <- function() {
  ## UI
  ui <- navbarPage(
    #insert theme
    theme=bslib::bs_theme(version=5),
    #thicker horizontal line & global page margins
    tags$head(
      tags$style(HTML("
        hr {
          border: 2px solid black;  /* Darker and thicker line */
          margin-top: 20px;
          margin-bottom: 20px;
        }
        
        .page-wrapper {
          margin-left: 5%;
          margin-right: 5%;
        }
      "))
    ),
    useShinyjs(),
    title="Artist Exploration Shiny App", 
    id="modNav",
  
    ### Classify Art Tab
    tabPanel(
      span(icon("microscope"), strong("Classifier")), 
      div(class="page-wrapper",
        classifyUI("classify_mod"))
    ),
    
    ### Art Exploration Tab
    tabPanel(
      span(icon("compass"), strong("Exploration")),
      div(class="page-wrapper",
        exploreUI("explore_mod"))
    ),
    
    ### Game Tab
    tabPanel(
      span(icon("gamepad"), strong("Game")),
      div(class="page-wrapper",
        gameUI("game_mod"))
    ),
    
    ### About section
    tabPanel(
      span(icon("circle-info"), "About"),
      div(class="page-wrapper",
        aboutUI("about_mod"))
    )
  )

  
  ## Server
  server <- function(input, output, session) {
    
    ### Classify Art Tab
    classifyServer("classify_mod", mod=mod1)
    
    ### Art Exploration Tab
    exploreServer("explore_mod")
    
    ### Game Tab
    gameServer("game_mod", mod=mod3)
    # gameServer("game_mod", mod=mod3, current_tab=reactive(input$modNav))
    
    ### About
    #placeholder
    
  }

shinyApp(ui, server)

}

artistExplorationApp()



