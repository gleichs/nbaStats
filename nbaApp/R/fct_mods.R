#' mods
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
mods_train <- function(df_train,model) {

  if(model=="Neural Network"){
    # Scale continuous vars
    namez <- colnames(df_train)
    df_train$numberGamePlayerSeason <- as.numeric(scale(df_train$numberGamePlayerSeason))
    df_train$countDaysRestPlayer <- as.numeric(scale(df_train$countDaysRestPlayer))
    df_train$yearSeason <- as.numeric(scale(df_train$yearSeason))

    # Save scaling params for pts for later
    y_scaled <- scale(df_train$pts)
    y_center <- attr(y_scaled, "scaled:center")
    y_scale  <- attr(y_scaled, "scaled:scale")
    df_train$pts <- as.numeric(y_scaled)
    colnames(df_train) <- namez
  }

  # Prep train data
  y <- df_train$pts
  X <- df_train
  X$pts <- NULL
  X$dateGame <- NULL

  # Fit lasso regression
  if(model=="Lasso Regression"){
    X <- as.matrix(X)
    mod_out <- glmnet::cv.glmnet(X, y, alpha = 1)
    mod_out_check <- as.matrix(coef(mod_out))
    if(sum(mod_out_check!=0) <=1){
      shiny::showModal(
        shiny::modalDialog(
          "All variables were dropped from the algorithm. Try training another model to obtain better results."
        )
      )
    }
    # Generate predictions using training data
    preds_train <- predict(mod_out, newx = X, s = "lambda.min")
  }

  # Fit RF
  else if(model=="Random Forest"){
    mod_out <- randomForest::randomForest(X,y)
    # Generate predictions using training data
    preds_train <- predict(mod_out, newx = X, s = "lambda.min")
  }

  # Fit NN
  else if(model=="Neural Network"){
    X$pts <- y
    colnames(X) <- make.names(colnames(X))
    mod_out <- neuralnet::neuralnet(
      pts ~ .,
      data = X,
      hidden = c(5, 3),
      linear.output = TRUE,
      stepmax = 1e6,
      rep = 3          # try multiple initial random starts
    )

    if("result.matrix" %in% names(mod_out)){
      # Try prediction again with explicit column removal
      predictor_cols <- setdiff(names(X), "pts")
      X_predictors <- X[, predictor_cols]
      preds_train <- neuralnet::compute(mod_out, X_predictors)$net.result
      preds_train <- preds_train * y_scale + y_center
    }
    else{
      shiny::showModal(
        shiny::modalDialog(
          "Neural network did not converge. Try a different training set or model type."
        )
      )
      preds_train <- NULL
    }
  }
  return(list(mod_out,preds_train))
}

mods_test <- function(df_test,df_train,preds_train,mod_out,model){

  df_train_og <- df_train
  df_test_og <- df_test

  if(model=="Neural Network"){
    # Scale continuous vars
    namez <- colnames(df_test)
    # Number games
    numberGamePlayerSeason_scaled <- scale(df_train$numberGamePlayerSeason)
    numberGamePlayerSeason_center <- attr(numberGamePlayerSeason_scaled, "scaled:center")
    numberGamePlayerSeason_scale  <- attr(numberGamePlayerSeason_scaled, "scaled:scale")
    # Number days rest
    countDaysRestPlayer_scaled <- scale(df_train$countDaysRestPlayer)
    countDaysRestPlayer_center <- attr(countDaysRestPlayer_scaled, "scaled:center")
    countDaysRestPlayer_scale  <- attr(countDaysRestPlayer_scaled, "scaled:scale")
    # Season
    yearSeason_scaled <- scale(df_train$yearSeason)
    yearSeason_center <- attr(yearSeason_scaled, "scaled:center")
    yearSeason_scale  <- attr(yearSeason_scaled, "scaled:scale")
    # Points
    y_scaled <- scale(df_train$pts)
    y_center <- attr(y_scaled, "scaled:center")
    y_scale  <- attr(y_scaled, "scaled:scale")
    # Scale test vars
    df_test$numberGamePlayerSeason <- (df_test$numberGamePlayerSeason - numberGamePlayerSeason_center)/numberGamePlayerSeason_scale
    df_test$countDaysRestPlayer <- (df_test$countDaysRestPlayer - countDaysRestPlayer_center)/countDaysRestPlayer_scale
    df_test$yearSeason <- (df_test$yearSeason - yearSeason_center)/yearSeason_scale
    df_test$pts <- (df_test$pts - y_center)/y_scale
  }

  # Prep test data
  X_test <- df_test
  X_test$pts <- NULL
  X_test$dateGame <- NULL

  # Lasso reg
  if(model=="Lasso Regression"){
    X_test <- as.matrix(X_test)
    # Generate test data predictions
    preds_test <- predict(mod_out, X_test, s = "lambda.min")
  }
  else if(model=="Random Forest"){
    # Generate test data predictions
    preds_test <- predict(mod_out, X_test, s = "lambda.min")
  }
  else if(model=="Neural Network"){
    preds_test <- neuralnet::compute(mod_out, X_test)$net.result
    preds_test <- preds_test * y_scale + y_center
  }

  # Wrangle predictions
  preds_test <- as.data.frame(preds_test)
  preds_test$dataset <- "Test"
  preds_train <- as.data.frame(preds_train)
  preds_train$dataset <- "Train"
  colnames(preds_test) <- colnames(preds_train)
  preds <- rbind(preds_train,preds_test)
  colnames(preds) <- c("pred","dataset")
  df_all <- rbind(df_train_og,df_test_og)
  preds <- cbind(preds,df_all)
  return(preds)
}
