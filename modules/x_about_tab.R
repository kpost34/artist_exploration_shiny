# About Section Module

# UI================================================================================================
aboutUI <- function(id) {
  tagList(
    h2("About the App"),
    accordion(
      accordion_panel(
        "App",
        p("This interactive application leverages the ",
          strong("Metropolitan Museum of Art's public API"),
          "to explore the world of European painters through data science and play. The app is
          divided into three components:"
        ),
        tags$ul(
          tags$li(
            strong("Artist Classifier:"),
            "A ML-driven tool that analyzes uploaded artwork to predict the creator, providing a ",
            "ranked list of likely artists and their associated probabilities."
          ),
          tags$li(
            strong("Artist Exploration:"),
            "A searchable gallery for discovering biographical details and masterpieces. Users ",
            "can browse the collection freely or filter by ",
            strong("nationality"),
            "and ",
            HTML(paste0(strong("artistic movement"), "."))
          ),
          tags$li(
            strong("The Identification Game:"),
            "A competitive mode where you go head-to-head with the algorithm. Test your art ",
            "knowledge across three difficulty levels to see if you can outscore the model."
          )
        )
      ),
      accordion_panel(
        "Models",
        "Here is text for important notes"
      ),
      accordion_panel(
        "Developer",
        strong("Keith Post"),
        br(),
        p("If you would like to see the code for this Shiny app, please visit the",
          tags$a(href="https://github.com/kpost34/artist_exploration_shiny", 
                 "Github repo"),
          "for this project."
        ),
        p("Important links:"),
        tags$ul(
          tags$li(tags$a(href="https://github.com/kpost34","GitHub Profile")),
          tags$li(tags$a(href="https://www.linkedin.com/in/keith-post","LinkedIn"))
        )
      ),
      open=FALSE,
      id="app_info_acc"
    )
  )
}

