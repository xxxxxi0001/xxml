#' Logistic regression with backward elimination based on p-values
#'
#' @param df Data frame
#' @param train_index Numeric vector or list of vectors
#' @param target_col Target column name
#' @param positive Positive class label
#' @param positive_weight Weight for positive class
#' @param negative_weight Weight for negative class
#' @return glm object or list of glm objects
#' @export
backward_p_lr <- function (df, train_index, target_col,
                           positive, positive_weight, negative_weight) {

  if (is.list(train_index)) {
    logistic_model <- list()
    for (i in 1:length(train_index)) {

      index <- unlist(train_index[[i]])

      logistic_model[[i]] <- glm(
        as.formula(paste(target_col, "~ .")),
        data    = df[index,],
        family  = binomial,
        weights = ifelse(df[[target_col]][index] == positive,
                         positive_weight, negative_weight)
      )

      coeffecient <- summary(logistic_model[[i]])$coefficients
      p_values    <- coeffecient[-1, "Pr(>|z|)"]

      if (max(p_values) > 0.05) {
        repeat {
          significant_feature <- names(p_values[p_values < 0.05])
          string_features <- as.formula(
            paste(target_col, "~", paste(significant_feature, collapse = "+"))
          )
          logistic_model[[i]] <- glm(
            string_features,
            data    = df[index,],
            family  = binomial,
            weights = ifelse(df[[target_col]][index] == positive,
                             positive_weight, negative_weight)
          )

          coeffecient <- summary(logistic_model[[i]])$coefficients
          p_values    <- coeffecient[-1,"Pr(>|z|)"]

          if (max(p_values) < 0.05) break

          if (all(is.na(p_values))) {
            cat("All p-values are NA, this model is not working")
            break
          }
        }
      }
    }
  }

  if (is.numeric(train_index)) {
    index <- train_index

    logistic_model <- glm(
      as.formula(paste(target_col, "~ .")),
      data    = df[index,],
      family  = binomial,
      weights = ifelse(df[[target_col]][index] == positive,
                       positive_weight, negative_weight)
    )

    coeffecient <- summary(logistic_model)$coefficients
    p_values    <- coeffecient[-1,"Pr(>|z|)"]

    if (max(p_values) > 0.05) {
      repeat {
        significant_feature <- names(p_values[p_values < 0.05])
        string_features <- as.formula(
          paste(target_col, "~", paste(significant_feature, collapse = "+"))
        )
        logistic_model <- glm(
          string_features,
          data    = df[index,],
          family  = binomial,
          weights = ifelse(df[[target_col]][index] == positive,
                           positive_weight, negative_weight)
        )

        coeffecient <- summary(logistic_model)$coefficients
        p_values    <- coeffecient[-1,"Pr(>|z|)"]

        if (max(p_values) < 0.05) break

        if (all(is.na(p_values))) {
          cat("All p-values are NA, this model is not working")
          break
        }
      }
    }
  }
  return(logistic_model)
}

#' Check model performance: accuracy, TPR, TNR, F1
#'
#' @param predict_prob Numeric vector or list of predictions
#' @param threshold Threshold to classify positive
#' @param positive Positive label
#' @param negative Negative label
#' @param df Data frame
#' @param test_index Indices used for evaluation
#' @param target_col Target column
#' @return List with accuracy, tpr, tnr, F1
#' @export
check_model_performance <- function (predict_prob, threshold,
                                     positive, negative,
                                     df, test_index, target_col) {

  if (is.list(predict_prob)){
    predict_prob <- unlist(predict_prob)
  }

  prediction <- ifelse(predict_prob >= threshold, positive, negative)
  true_value <- as.numeric(as.character(df[[target_col]][test_index]))

  TN <- sum(prediction == negative & true_value == negative)
  TP <- sum(prediction == positive & true_value == positive)
  FP <- sum(prediction == positive & true_value == negative)
  FN <- sum(prediction == negative & true_value == positive)

  precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
  recall    <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
  F1        <- ifelse(precision + recall == 0, 0,
                      round(2 * precision * recall / (precision + recall), 2))

  accuracy <- round(((TP + TN) / (TP + TN + FP + FN)) * 100, 2)
  tpr      <- round((TP / (TP + FN)) * 100, 2)
  tnr      <- round((TN / (TN + FP)) * 100, 2)

  cat("The accuracy is", accuracy,
      "%. The True Positive Rate is ", tpr,
      "%. And the True Negative Rate is ", tnr,
      "% and the F1 score is", F1,".\n")

  return(list(
    accuracy = accuracy,
    tpr      = tpr,
    tnr      = tnr,
    F1       = F1
  ))
}

#' Cross validation wrapper for LR / RF / C5.0 / rpart
#'
#' @param df Data frame
#' @param index_list List of folds (indices)
#' @param positive Positive label
#' @param negative Negative label
#' @param positive_weight Positive class weight
#' @param negative_weight Negative class weight
#' @param target_col Target column
#' @param model_type "lr", "rf", "c5", or "rpart"
#' @param seed_num Seed
#' @param threshold Classification threshold
#' @return Printed fold-wise and overall performance
#' @export
cross_validation <- function(df, index_list,
                             positive, negative,
                             positive_weight, negative_weight,
                             target_col, model_type,
                             seed_num = 888, threshold = 0.5){

  set.seed(seed_num)

  df[[target_col]] <- factor(df[[target_col]],
                             levels = c(negative, positive))

  F1_total       <- 0
  accuracy_total <- 0
  tpr_total      <- 0
  tnr_total      <- 0

  for (i in 1:length(index_list)) {

    test_index  <- index_list[[i]]
    train_index <- unlist(index_list[-i])

    if (tolower(model_type) %in% c("logistic regression","lr")) {

      model <- backward_p_lr(df, train_index, target_col,
                             positive, positive_weight, negative_weight)
      predict_v <- predict(model, df[test_index,], type = "response")

    } else if (tolower(model_type) %in% c("random forest","rf")) {

      rf_model <- randomForest::randomForest(
        as.formula(paste(target_col, "~ .")),
        data       = df[train_index,],
        ntree      = 500,
        mtry       = 2,
        importance = TRUE
      )
      predict_v <- predict(rf_model, df[test_index,], type = "prob")[, as.character(positive)]

    } else if (tolower(model_type) %in% c("c5.0","c5","c50")) {

      feature <- setdiff(names(df), target_col)
      c5_m <- C50::C5.0(
        x      = df[train_index, feature],
        y      = df[train_index, target_col],
        trials = 10
      )
      predict_v <- predict(c5_m, df[test_index,], type = "prob")[, as.character(positive)]

    } else if (tolower(model_type) %in% c("rpart","decision tree")) {

      tree_m <- rpart::rpart(
        as.formula(paste(target_col, "~ .")),
        data   = df[train_index,],
        method = "class"
      )
      predict_v <- predict(tree_m, df[test_index,], type = "prob")[, as.character(positive)]

    } else {
      stop("Only support logistic regression, random forest, c5.0, decision tree")
    }

    cat("Now is runnung fold", i, "\n")
    result   <- check_model_performance(predict_v, threshold,
                                        positive, negative, df, test_index, target_col)
    accuracy <- result$accuracy
    tpr      <- result$tpr
    tnr      <- result$tnr
    F1       <- result$F1

    F1_total       <- F1_total + F1
    accuracy_total <- accuracy_total + accuracy
    tpr_total      <- tpr_total + tpr
    tnr_total      <- tnr_total + tnr
  }

  F1_overall       <- round(F1_total / length(index_list), 2)
  accuracy_overall <- round(accuracy_total / length(index_list), 2)
  tpr_overall      <- round(tpr_total / length(index_list), 2)
  tnr_overall      <- round(tnr_total / length(index_list), 2)

  cat("Overall the cross validation of this model's performance's accuracy is ",
      accuracy_overall,"%, true positive rate is", tpr_overall,
      "%, true negative rate is", tnr_overall,
      "% and F1 is", F1_overall,".\n")
}
