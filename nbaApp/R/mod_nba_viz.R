#' nba_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nba_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      sidebar=bslib::sidebar(
        shiny::selectizeInput(ns("years_input"),label="Select Season(s)",choices=c(1975:2026),selected=2026,multiple=TRUE),
        shiny::selectInput(ns("team_input"),label="Select Team",choices=c(NULL)),
        shiny::selectInput(ns("player_input"),label="Select Player",choices=c(NULL)),
        shiny::actionButton(ns("go"),label="Visualize Data")
      ),
      bslib::layout_columns(
        col_widths = c(4,4,4),
        plotly::plotlyOutput(ns("plot_fg")),
        plotly::plotlyOutput(ns("plot_ft")),
        plotly::plotlyOutput(ns("plot_p"))
      )
    )
  )
}

#' nba_viz Server Functions
#'
#' @noRd
mod_nba_viz_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Load all possible data and set up reactive
    df <- readRDS("nba_stats_12062025.rds")
    df_rctv <- shiny::reactiveVal(NULL)

    # Update teams based on years
    shiny::observeEvent(input$years_input,{
      dfSub <- df |> dplyr::filter(df$yearSeason %in% c(input$years_input))
      df_rctv(dfSub)
      shiny::updateSelectInput(session=session,"team_input",choices=c(unique(dfSub$nameTeam)))
    })

    # Update players based on teams
    shiny::observeEvent(input$team_input,{
      dfSub <- df_rctv()
      dfSub <- dfSub |> dplyr::filter(nameTeam==input$team_input)
      shiny::updateSelectInput(session=session,"player_input",choices=c(unique(dfSub$namePlayer)))
    })

    shiny::observeEvent(input$go,{
      dfSub <- df_rctv()
      dfSub <- dfSub |> dplyr::filter(namePlayer==input$player_input)

      # % PG
      p1 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=pctFG,text=(paste("Date of Game:",dateGame,"\nFG Attempt:",fga, "\nFG Made:",fgm,"\n% FG:",pctFG,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("% FG")+
        ggplot2::ggtitle(paste(input$player_input,"\n% FG"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p1 <- plotly::ggplotly(p1,tooltip = "text")

      # % FT
      p2 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=pctFT,text=(paste("Date of Game:",dateGame,"\nFT Attempt:",fta, "\nFT Made:",ftm,"\n% FT:",pctFT,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("% FT")+
        ggplot2::ggtitle(paste(input$player_input,"\n% FT"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p2 <- plotly::ggplotly(p2,tooltip = "text")

      # PTS
      p3 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=pts,text=(paste("Date of Game:",dateGame,"\nPoints:",pts,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("Points")+
        ggplot2::ggtitle(paste(input$player_input,"\nPoints"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p3 <- plotly::ggplotly(p3,tooltip = "text")

      # Render
      output$plot_fg <- plotly::renderPlotly({p1})
      output$plot_ft <- plotly::renderPlotly({p2})
      output$plot_p <- plotly::renderPlotly({p3})
    })

  })
}

## To be copied in the UI
# mod_nba_viz_ui("nba_viz_1")

## To be copied in the server
# mod_nba_viz_server("nba_viz_1")
