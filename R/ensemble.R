#' Make ensemble prediction by averaging probabilities
#'
#' @param model_list List of models (glm, randomForest, C5.0, rpart)
#' @param df_list List of data frames (or single data frame)
#' @param test_index Indices to predict
#' @param positive Positive class label
#' @param target_treatment Transformation flag (unused in your current code)
#' @return Numeric vector of ensemble probabilities
#' @export
make_ensemble_predict <- function (model_list, df_list,
                                   test_index, positive,
                                   target_treatment = "none") {

  prediction_list <- vector("list", length(model_list))

  if (inherits(df_list, "data.frame")) {
    df_list <- rep(list(df_list), length(model_list))
  }

  for (i in seq_along(model_list)) {
    model <- model_list[[i]]
    df    <- df_list[[i]]

    if (inherits(model, "glm")) {
      prediction_list[[i]] <- predict(model_list[[i]],
                                      df[test_index,], type = "response")
    } else if (inherits(model, "randomForest") ||
               inherits(model, "C5.0") ||
               inherits(model, "rpart")) {
      prediction_list[[i]] <- predict(model, df[test_index,],
                                      type = "prob")[, as.character(positive)]
    } else {
      stop("Only support logistic regression, randomForest, C5.0, rpart")
    }
  }

  ensemble_predictions <- Reduce("+", prediction_list) / length(model_list)
  ensemble_predictions <- as.numeric(ensemble_predictions)

  if (tolower(target_treatment) != "none") {
    # reverse_num is referenced in your original code;
    # if you use it, you can define it in another helper file.
    ensemble_predictions <- reverse_num(ensemble_predictions)
  }

  return(ensemble_predictions)
}

#' Find best threshold to maximize F1
#'
#' @param predict_prob Numeric vector of probabilities
#' @param df Data frame
#' @param test_index Indices used
#' @param target_col Target column name
#' @param positive Positive label
#' @param negative Negative label
#' @return Best threshold (numeric)
#' @export
find_best_threshold <- function (predict_prob, df, test_index,
                                 target_col, positive, negative) {

  real_value <- as.numeric(as.character(df[[target_col]][test_index]))
  best_threshold <- -Inf
  largest_F1     <- -Inf

  for (i in seq(0.01, 0.99, by = 0.01)) {

    prediction_result <- ifelse(predict_prob >= i, positive, negative)

    TP <- sum(prediction_result == positive & real_value == positive)
    FP <- sum(prediction_result == positive & real_value == negative)
    FN <- sum(prediction_result == negative & real_value == positive)
    precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
    recall    <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
    F1        <- ifelse(precision + recall == 0, NA,
                        2 * precision * recall / (precision + recall))

    if (!is.na(F1) && F1 > largest_F1){
      largest_F1     <- F1
      best_threshold <- i
    }
  }

  cat("The best threshold is", best_threshold,
      "and it gives largest F1", round(largest_F1,3),".")
  return(best_threshold)
}

#' Compute F1-based weight for each ensemble model
#'
#' @param model_list List of models
#' @param df_list List of data frames (or single data frame)
#' @param test_index Test indices
#' @param best_threshold Threshold used to binarize predictions
#' @param target_col Target column
#' @param positive Positive label
#' @param negative Negative label
#' @return List of weights (sum to 1)
#' @export
ensemble_weight_F1 <- function(model_list, df_list,
                               test_index, best_threshold,
                               target_col, positive, negative) {

  F1_list <- list()
  prediction_list <- vector("list", length(model_list))

  if (inherits(df_list, "data.frame")) {
    df_list <- rep(list(df_list), length(model_list))
  }

  for (i in 1:length(prediction_list)) {
    model <- model_list[[i]]
    df    <- df_list[[i]]

    if (inherits(model, "glm")) {
      prediction_list[[i]] <- predict(model_list[[i]],
                                      df[test_index,], type = "response")
    } else if (inherits(model, "randomForest") ||
               inherits(model, "C5.0") ||
               inherits(model, "rpart")) {
      prediction_list[[i]] <- predict(model, df[test_index,],
                                      type = "prob")[, as.character(positive)]
    } else {
      stop("Only support logistic regression, randomForest, C5.0, rpart")
    }

    prediction <- ifelse(prediction_list[[i]] >= best_threshold,
                         positive, negative)
    real_value <- as.numeric(as.character(df[[target_col]][test_index]))

    TP <- sum(prediction == positive & real_value == positive)
    FP <- sum(prediction == positive & real_value == negative)
    FN <- sum(prediction == negative & real_value == positive)
    precision <- TP / (TP + FP)
    recall    <- TP / (TP + FN)
    F1        <- 2 * (precision * recall) / (precision + recall)

    F1_list[[i]] <- F1
    # 这里把 "glm lm" 那串 ugly class 名字去掉，只打印类型：
    type_label <- if (inherits(model, "glm")) {
      "Logistic Regression"
    } else if (inherits(model, "randomForest")) {
      "Random Forest"
    } else if (inherits(model, "C5.0")) {
      "C5.0"
    } else if (inherits(model, "rpart")) {
      "Decision Tree"
    } else {
      class(model)[1]
    }

    cat("The model", i, "(", type_label, ")'s F1 is",
        round(F1_list[[i]], 3), "\n")
  }

  F1_vector  <- unlist(F1_list)
  F1_weights <- F1_vector / sum(F1_vector)
  weight_list <- as.list(F1_weights)

  cat("Each of their weight are",
      round(as.numeric(weight_list),3),".")

  return(weight_list)
}

#' Weighted ensemble prediction
#'
#' @param model_list List of models
#' @param df_list List of data frames or single data frame
#' @param index Indices to predict
#' @param weight_list List of weights
#' @param positive Positive label
#' @param target_treatment Transformation flag
#' @return Numeric vector of weighted ensemble probabilities
#' @export
emsemble_result_with_weight <- function(model_list, df_list,
                                        index, weight_list,
                                        positive, target_treatment = "none") {

  if (inherits(df_list, "data.frame")) {
    df_list <- rep(list(df_list), length(model_list))
  }

  if (length(model_list) != length(weight_list)) {
    stop("Model List and Weight List must be equal number")
  } else if (length(model_list) != length(df_list)) {
    stop("Model List and Data Frame List must be equal number")
  }

  prediction_list <- vector("list", length(model_list))

  for (i in seq_along(model_list)) {
    model <- model_list[[i]]
    df    <- df_list[[i]]

    if (inherits(model, "glm")) {
      prediction_list[[i]] <- predict(model, df[index,], type = "response")
      prediction_list[[i]] <- prediction_list[[i]] * weight_list[[i]]
    } else if (inherits(model, "randomForest") ||
               inherits(model, "C5.0") ||
               inherits(model, "rpart")) {
      prediction_list[[i]] <- predict(model, df[index,],
                                      type = "prob")[, as.character(positive)]
      prediction_list[[i]] <- prediction_list[[i]] * weight_list[[i]]
    } else {
      stop("Only support logistic regression, randomForest, C5.0, rpart")
    }
  }

  ensemble_predictions <- Reduce("+", prediction_list)

  if (tolower(target_treatment) != "none") {
    ensemble_predictions <- reverse_num(ensemble_predictions, target_treatment)
  }

  return(ensemble_predictions)
}
