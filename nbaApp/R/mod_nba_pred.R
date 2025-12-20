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
        shiny::selectInput(ns("team"),label="Select Current Team",choices=c(NULL)),
        shiny::selectInput(ns("player"),label="Select Player for Prediction",choices=c(NULL)),
        shiny::dateRangeInput(ns("dates"),label="Select Date Range for Model Training",min=NULL,max=NULL),
        shiny::selectInput(ns("mod_type"),label="Select Model Type",choices=c("Lasso Regression","Random Forest")),
        shiny::actionButton(ns("train"),label="Train Model")
      ),
      bslib::card(
        bslib::card_header("Player Model Evaluation"),
        bslib::layout_columns(
          col_widths = c(3,3,6),
          bslib::value_box(
            title="RMSE",
            id="rmse_box",
            value = textOutput(ns("rmse_out"))
          ),
          bslib::value_box(
            id="mae_box",
            title="MAE",
            value=shiny::textOutput(ns("mae_out"))
          ),
          plotly::plotlyOutput(ns("mod_out"))
        )
      ),
      bslib::card(
        bslib::card_header("Player Model Prediction (Next Game)"),
        bslib::layout_columns(
        col_widths = c(4,4,4),
        shiny::dateInput(ns("next_game"),"Date of Next Game"),
        shiny::selectInput(ns("next_opponent"),"Opposing Team",choices=c(NULL)),
        shiny::actionButton(ns("pred_go"),"Generate Pts Prediction")
      ),
      plotly::plotlyOutput(ns("mod_pred"))
      )
    )
  )
}

#' nba_pred Server Functions
#'
#' @noRd
mod_nba_pred_server <- function(id,out){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Reactives
    df_2026 <- shiny::reactiveVal(NULL)
    df_player <- shiny::reactiveVal(NULL)

    # Observe initial data and update 2026 team names
    shiny::observeEvent(out(),{
      df <- out()
      df_2026 <- df |> dplyr::filter(yearSeason==2026)
      shiny::updateSelectInput(session=session,"team",choices=c(unique(df_2026$nameTeam)))
      df_2026(df_2026)
    })

    # When the team is updated, list played on that team
    shiny::observeEvent(input$team,{
      if(input$team!=""){
        dfTeam <- df_2026()
        dfTeam <- dfTeam |> dplyr::filter(nameTeam==input$team)
        shiny::updateSelectInput(session=session,"player",choices=c(unique(dfTeam$namePlayer)))
      }
    })

    # When the player is selected, update the date range
    shiny::observeEvent(input$player,{
      if(input$player!="" & input$team!=""){
        df <- out()
        dfPlayer <- df |> dplyr::filter(namePlayer==input$player)
        shiny::updateDateRangeInput(session=session,"dates",min=min(dfPlayer$dateGame),max=max(dfPlayer$dateGame),start=min(dfPlayer$dateGame),end=max(dfPlayer$dateGame)-30)
      }
    })

    # When train is executed
    shiny::observeEvent(input$train,{

      # Select predictor variables
      df_pred <- out()
      df_pred <- subset(df_pred,namePlayer==input$player)
      df_pred <- df_pred[c("yearSeason","nameTeam","isB2B","locationGame","slugOpponent","numberGamePlayerSeason","countDaysRestPlayer","pts","dateGame")]

      # Convert factors to dummy numeric
      df_pred$nameTeam <- as.numeric(as.factor(df_pred$nameTeam))
      df_pred$isB2B <- as.numeric(as.factor(df_pred$isB2B))
      df_pred$isB2B <- ifelse(df_pred$isB2B==1,0,1)
      df_pred$locationGame <- as.numeric(as.factor(df_pred$locationGame))
      df_pred$locationGame <- ifelse(df_pred$locationGame==1,0,1)
      df_pred$slugOpponent <- as.numeric(as.factor(df_pred$slugOpponent))

      # Train/test split
      df_train <- df_pred |> dplyr::filter(dateGame <= input$dates[2] & dateGame >= input$dates[1])
      df_test <- df_pred |> dplyr::filter(dateGame > input$dates[2])

      # Prep train data
      y <- df_train$pts
      X <- df_train
      X$pts <- NULL
      X$dateGame <- NULL

      # Fit lasso regression or RF regression
      if(input$mod_type=="Lasso Regression"){
        X <- as.matrix(X)
        mod_out <- glmnet::cv.glmnet(X, y, alpha = 1)
      }
      else if(input$mod_type=="Random Forest"){
        mod_out <- randomForest::randomForest(X,y)
      }

      # Generate predictions using training data
      preds_train <- predict(mod_out, newx = X, s = "lambda.min")
      # If test data are available...
      if(nrow(df_test)>0){

        # Prep test data
        X_test <- df_test
        X_test$pts <- NULL
        X_test$dateGame <- NULL
        if(input$mod_type=="Lasso Regression"){
          X_test <- as.matrix(X_test)
        }

        # Generate test data predictions
        preds_test <- predict(mod_out, X_test, s = "lambda.min")

        # Wrangle predictions
        preds_test <- as.data.frame(preds_test)
        preds_test$dataset <- "Test"
        preds_train <- as.data.frame(preds_train)
        preds_train$dataset <- "Train"
        colnames(preds_test) <- colnames(preds_train)
        preds <- rbind(preds_train,preds_test)
        colnames(preds) <- c("pred","dataset")
        df_all <- rbind(df_train,df_test)
        preds <- cbind(preds,df_all)
        # RMSE/MAE calculations
        mae <- mean(abs(df_test$pts - preds_test[[1]]))
        rmse <- sqrt(mean((df_test$pts - preds_test[[1]])^2))
      }
      else{
        shiny::showModal(
          shiny::modalDialog(
            "No data to use for model testing. RMSE and MAE values will be calculated using training data. Error metrics should be interpreted with caution."
          )
        )
        preds <- as.data.frame(preds_train)
        preds$dataset <- "Train"
        colnames(preds) <- c("pred","dataset")
        preds <- cbind(preds,df_train)
        # RMSE/MAE calculations
        mae <- mean(abs(preds$pts - preds$pred))
        rmse <- sqrt(mean((preds$pts - preds$pred)^2))
      }

      # Plot predictions
      preds_out <- ggplot2::ggplot(preds,ggplot2::aes(x=pred,y=pts,shape=dataset,color=dataset,text=paste("Predicted Points: ",round(pred,3),"\nPoints Made: ",round(pts,3),"\nDataset: ",dataset,sep="")))+
        ggplot2::geom_point(size=2)+
        ggplot2::geom_abline(intercept=0,slope=1,linetype="dashed")+
        ggplot2::theme_bw(base_size=14)+
        ggplot2::xlab("Predicted Points")+
        ggplot2::ylab("Points Made")+
        ggplot2::scale_color_manual(name="Dataset",values=c("red3","dodgerblue"),breaks=c("Test","Train"))+
        ggplot2::scale_shape_manual(name="Dataset",values=c(17,16),breaks=c("Test","Train"))+
        ggplot2::ggtitle(paste(input$player))

      # Render eval plot
      output$mod_out <- plotly::renderPlotly({
        plotly::ggplotly(preds_out,tooltip = "text")
      })

      # Render value box RMSE/MAE info
      output$mae_out <- shiny::renderText({round(mae,3)})
      output$rmse_out <- shiny::renderText({round(rmse,3)})

      # Update prediction selections
      df_2026 <- df_2026()
      df_2026 <- df_2026 |> dplyr::filter(nameTeam!=input$team)
      shiny::updateSelectInput(session=session,"next_opponent",choices=c(unique(df_2026$nameTeam)))
    })
  })
}

## To be copied in the UI
# mod_nba_pred_ui("nba_pred_1")

## To be copied in the server
# mod_nba_pred_server("nba_pred_1")
