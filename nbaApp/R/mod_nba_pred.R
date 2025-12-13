#' nba_pred UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nba_pred_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar=bslib::sidebar(
        shiny::selectInput(ns("team"),label="Select Team",choices=c(NULL)),
        shiny::selectInput(ns("player"),label="Select Player for Prediction",choices=c(NULL)),
        shiny::dateRangeInput(ns("dates"),label="Select Date Range for Model Training",min=NULL,max=NULL),
        shiny::actionButton(ns("train"),label="Train Model")
      ),
      plotly::plotlyOutput(ns("mod_out"))
    )
  )
}

#' nba_pred Server Functions
#'
#' @noRd
mod_nba_pred_server <- function(id,out){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactive
    df_rctv <- shiny::reactiveVal(NULL)

    # Observe data
    shiny::observeEvent(out(),{
      df <- out()
      df_2026 <- df |> dplyr::filter(yearSeason==2026)
      shiny::updateSelectInput(session=session,"team",choices=c(unique(df_2026$nameTeam)))
      df_rctv(df_2026)
    })

    shiny::observeEvent(input$team,{
      if(input$team!=""){
        dfPlayer <- df_rctv()
        dfPlayer <- dfPlayer |> dplyr::filter(nameTeam==input$team)
        shiny::updateSelectInput(session=session,"player",choices=c(unique(dfPlayer$namePlayer)))
      }
    })

    shiny::observeEvent(input$player,{
      if(input$player!="" & input$team!=""){
        df <- out()
        df <- df |> dplyr::filter(namePlayer==input$player & nameTeam==input$team)
        shiny::updateDateRangeInput(session=session,"dates",min=min(df$dateGame),max=max(df$dateGame),start=min(df$dateGame),end=max(df$dateGame))
      }
    })

    shiny::observeEvent(input$train,{
      browser()
      df_pred <- out()
      df_pred <- df_pred |> dplyr::filter(nameTeam==input$team & namePlayer==input$player & dateGame <= input$dates[2] & dateGame >= input$dates[1])
      y <- df_pred$pts
      X <- df_pred[c("numberGameTeamSeason","isB2B","locationGame","countDaysRestTeam","slugOpponent","isWin","fgm","fga","minutes","ftm","fta")]
      X$isB2B <- as.numeric(as.factor(X$isB2B))
      X$locationGame <- as.numeric(as.factor(X$locationGame))
      X$slugOpponent <- as.numeric(as.factor(X$slugOpponent))
      X$isWin <- as.numeric(as.factor(X$isWin))

      # Fit lasso with cross-validation
      cv_fit <- glmnet::cv.glmnet(X, y, alpha = 1)


  })

  })
}

## To be copied in the UI
# mod_nba_pred_ui("nba_pred_1")

## To be copied in the server
# mod_nba_pred_server("nba_pred_1")
