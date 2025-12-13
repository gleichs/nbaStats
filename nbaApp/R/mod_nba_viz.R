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
      waiter::use_waiter(),
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
      ),
      bslib::layout_columns(
        col_widths = c(4,4,4),
        plotly::plotlyOutput(ns("plot_ast")),
        plotly::plotlyOutput(ns("plot_treb")),
        plotly::plotlyOutput(ns("plot_blk"))
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

    waiter::waiter_show(
      html = tagList(
        waiter::spin_fading_circles(),
        h3("Loading NBA App...")
      ),
      color = "#222222"
    )
    # Load all possible data and set up reactive
    df <- readRDS("nba_stats_12062025.rds")
    df_pass <- shiny::reactiveVal(df)
    df_rctv <- shiny::reactiveVal(NULL)
    waiter::waiter_hide()

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
      dfSub$pctFG <- dfSub$pctFG*100
      dfSub$pctFT <- dfSub$pctFT*100
      p1 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=pctFG,text=(paste("Date of Game:",dateGame,"\nFG Attempt:",fga, "\nFG Made:",fgm,"\n% FG:",pctFG,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(data=dfSub[!is.na(dfSub$pctFG),],ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("% FG")+
        ggplot2::ggtitle(paste(input$player_input,"\n% FG"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"),breaks=c("L","W"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p1 <- plotly::ggplotly(p1,tooltip = "text")

      # % FT
      p2 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=pctFT,text=(paste("Date of Game:",dateGame,"\nFT Attempt:",fta, "\nFT Made:",ftm,"\n% FT:",pctFT,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(data=dfSub[!is.na(dfSub$pctFT),],ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("% FT")+
        ggplot2::ggtitle(paste(input$player_input,"\n% FT"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"),breaks=c("L","W"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p2 <- plotly::ggplotly(p2,tooltip = "text")

      # PTS
      p3 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=pts,text=(paste("Date of Game:",dateGame,"\nPoints:",pts,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(data=dfSub[!is.na(dfSub$pts),],ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("Points")+
        ggplot2::ggtitle(paste(input$player_input,"\nPoints"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"),breaks=c("L","W"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p3 <- plotly::ggplotly(p3,tooltip = "text")

      # Ast
      p4 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=ast,text=(paste("Date of Game:",dateGame,"\nNumber of Assists:",ast,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(data=dfSub[!is.na(dfSub$ast),],ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("Assists")+
        ggplot2::ggtitle(paste(input$player_input,"\nNumber of Assists"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"),breaks=c("L","W"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p4 <- plotly::ggplotly(p4,tooltip = "text")


      # Total Rebounds
      p5 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=treb,text=(paste("Date of Game:",dateGame,"\nOffensive Rebounds:",oreb,"\nDefensive Rebounds:",dreb,"\nTotal Rebounds:",treb,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(data=dfSub[!is.na(dfSub$treb),],ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("Total Rebounds")+
        ggplot2::ggtitle(paste(input$player_input,"\nTotal Rebounds"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"),breaks=c("L","W"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p5 <- plotly::ggplotly(p5,tooltip = "text")

      # Blocks
      p6 <- ggplot2::ggplot(dfSub,ggplot2::aes(x=dateGame,y=blk,text=(paste("Date of Game:",dateGame,"\nNumber of Blocks:",blk,"\nOutcome of Game:",outcomeGame,"\nMatchup:",slugMatchup))))+
        ggplot2::geom_line(data=dfSub[!is.na(dfSub$blk),],ggplot2::aes(group=1))+
        ggplot2::geom_point(ggplot2::aes(color=outcomeGame),size=3)+
        ggplot2::theme_bw()+
        ggplot2::xlab("Date of Game")+
        ggplot2::ylab("Number of Blocks")+
        ggplot2::ggtitle(paste(input$player_input,"\nNumber of Blocks"))+
        ggplot2::scale_color_manual(name="Outcome of Game",values=c("red3","dodgerblue"),breaks=c("L","W"))+
        ggplot2::scale_x_date(date_labels = "%m-%d-%y")

      p6 <- plotly::ggplotly(p6,tooltip = "text")

      # Render
      output$plot_fg <- plotly::renderPlotly({p1})
      output$plot_ft <- plotly::renderPlotly({p2})
      output$plot_p <- plotly::renderPlotly({p3})
      output$plot_ast <- plotly::renderPlotly({p4})
      output$plot_treb <- plotly::renderPlotly({p5})
      output$plot_blk <- plotly::renderPlotly({p6})
    })
    return(df_pass)
  })
}

## To be copied in the UI
# mod_nba_viz_ui("nba_viz_1")

## To be copied in the server
# mod_nba_viz_server("nba_viz_1")
