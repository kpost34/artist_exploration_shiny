# Artist Exploration Shiny App
## App code

# Load Packages & Source Scripts====================================================================
## Load packages
pacman:: p_load(shiny, shinyjs, bslib, here, tidyverse, janitor, DT, tidymodels, shinyWidgets,
                slickR)


## Load functions and objects
here("fns_objs") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)


## Load modules
here("modules") %>%
  list.files(full.names=TRUE) %>%
  purrr::map(source)



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
    tabPanel("Classify Art", 
      div(class="page-wrapper",
        classifyUI("classify_mod"))
    ),
    
    ### Art Exploration Tab
    tabPanel("Explore Arts",
      div(class="page-wrapper",
        exploreUI("explore_mod"))
    ),
    
    ### Game Tab
    tabPanel("Compete Against the Model",
      div(class="page-wrapper",
        gameUI("game_mod"))
    ),
    
    ### App Info
    tabPanel("App Info",
      div(class="page-wrapper",
        appInfoUI("app_info_mod"))
    ),
    
    ### Developer Info Tab
    tabPanel("Developer Info",
      div(class="page-wrapper",
        devInfoUI("dev_info_mod"))
    )
  )

  
  ## Server
  server <- function(input, output, session) {
    
    ### Classify Art Tab
    classifyServer("classify_mod")
    
    ### Art Exploration Tab
    exploreServer("explore_mod")
    
    ### Game Tab
    gameServer("game_mod")
    
    ### App Info
    #placeholder
    
    ### Developer Info
    #placeholder
    
    
  }

shinyApp(ui, server)

}

artistExplorationApp()



