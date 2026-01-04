#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    bslib::page_navbar(
      bslib::nav_panel("Visualize Player Stats",icon=bsicons::bs_icon("graph-up"),mod_nba_viz_ui("nba_viz_1")),
      bslib::nav_panel("Player Point Prediction",icon=bsicons::bs_icon("person-standing"),mod_nba_pred_ui("nba_pred_1"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nbaApp"
    ),
    waiter::use_waiter(),
    shinyjs::useShinyjs()
  )
}
