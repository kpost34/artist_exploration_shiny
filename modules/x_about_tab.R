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
        h4("Technical Project Constraints"),
        p(
          strong("Note on Accuracy:"),
          "These models were developed under strict hardware limitations (8GB RAM,",
          "limited disk space). To maximize the user experience, I prioritized a",
          strong("diverse artist pool"),
          "over a high-volume image training set, choosing to showcase the algorithm's ability", 
          "to handle 35–40 different styles rather than optimizing for high accuracy on a smaller,",
          "more narrow dataset."
        ),
        hr(),
        h3("Model 1: The Visual Specialist (Image Data Only; Classifier)"),
        p("This model attempts to identify artists using only raw numerical representations of",
          "their work, focusing on color distributions and intensity."),
        br(),
        strong("Data Processing & Engineering"),
        tags$ul(
          tags$li(
            strong("Image Standardization:"),
            "Artworks were downsampled to 100 x 100 grayscale vectors to maintain a low",
            "memory footprint."
          ),
          tags$li(
            strong("Feature Extraction:"),
            "Derived", 
            strong("RGB Statistics"),
            "(mean, SD, skewness, kurtosis) and",
            strong("Color Bins"),
            "(frequency counts of pixel intensities across 5 levels)."
          ),
          tags$li(
            strong("Data Refinement:"),
            "Focused on a subset of 35 artists with 10+ works each",
            HTML(paste0("(", em("N"))),
            "= 570); removed near-zero variance and highly correlated features",
            HTML(paste0("(", em("r"))),
            "> 0.9)."
          )
        ),
        br(),
        strong("Model Performance & Tuning"),
        tags$ul(
          tags$li(
            strong("Algorithms Tested:"),
            "Random Forest (RF), XGBoost (XGB), and Multinomial Logistic Regression (MLR)."
          ),
          tags$li(
            strong("Winning Model: Random Forest (Ranger)"),
            tags$ul(
              tags$li(
                strong("Test Accuracy:"), 
                "~25%"
              ),
              tags$li(
                strong("Insight:"),
                "Even with limited pixel data and low sample sizes per artist, the model performs",
                "significantly better than random guessing (~2.8%)."
              )
            )
          )
        ),
        hr(),
        h3("Model 2: The Art Historian (Multimodal Data; Game)"),
        p(
          "This model simulates a holistic approach by combining visual cues with 'curatorial'",
          "metadata." 
        ),
        br(),
        strong("Data Processing & Engineering"),
        tags$ul(
          tags$li(
            strong("Visual Features:"),
            "Retained the RGB stats and bins from the previous workflow."
          ),
          tags$li(
            strong("Metadata Engineering:"),
            tags$ul(
              tags$li(
                strong("NLP:"),
                "Applied",
                strong("TF-IDF"),
                "to artwork titles to identify thematic keywords."
              ),
              tags$li(
                strong("Temporal/Physical:"),
                "Encoded creation length, century, dimensions, shape, and medium (e.g., gold,",
                "parchment)."
              )
            )
          ),
          tags$li(
            strong("Scope:"),
            "Optimized for 39 artists represented in both the 'public' (display-ready) and",
            HTML(paste0("non-public datasets (", em("N"), "= 501)."))
          )
        ),
        br(),
        strong("Model Performance & Tuning"),
        tags$ul(
          tags$li(
            strong("Winning Model: Random Forest (Ranger)"),
            tags$ul(
              tags$li(
                strong("Accuracy: 46-48%")
              ),
              tags$li(
                strong("Comparison:"),
                "By adding context, like the medium used or the era of creation, the model's",
                "predictive power nearly",
                strong("doubled"),
                "compared to using image data alone."
              )
            )
          )
        )
      ),
      accordion_panel(
        "Developer",
        strong("Keith Post"),
        br(),
        p("If you would like to see the code for this Shiny app, please visit the",
          tags$a(href="https://github.com/kpost34/artist_exploration_shiny", 
                 "Github repo"),
          "associated with this project."
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

