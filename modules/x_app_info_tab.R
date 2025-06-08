# App Ifo Module

# UI================================================================================================
appInfoUI <- function(id) {
  tagList(
    h2("Application info"),
    accordion(
      accordion_panel(
        "App Overview",
        "Here is text for app overview."
        ),
      accordion_panel(
        "Important Notes",
        "Here is text for important notes"
      ),
      open=FALSE,
      id="app_info_acc")
  )
}

