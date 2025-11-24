#' Create a Stacked Model for Multiple Linear Regression
#'
#' Builds a stack model on top of base ensemble models, using their predictions
#' as features and the true target as response.
#'
#' @param df Data frame containing the target.
#' @param target_col Name of the target column.
#' @param test_index Integer vector of indices used to train the stack model.
#' @param model_list List of base regression models.
#'
#' @return A fitted \code{lm} stack model.
#' @export
create_stack_model_mlr <- function(df, target_col, test_index, model_list) {

  stack_df <- data.frame(true_value = df[[target_col]][test_index])

  for (i in 1:length(model_list)) {
    stack_df[[paste0("model", i)]] <- stats::predict(model_list[[i]], df[test_index, ])
  }

  stack_model <- stats::lm(true_value ~ ., data = stack_df)
  return(stack_model)
}

#' Test a Stacked MLR Model and Compute RMSE
#'
#' Uses a stack model and base models to predict on a validation set and
#' computes RMSE between predictions and true values.
#'
#' @param stack_model Fitted stack model (from \code{create_stack_model_mlr}).
#' @param model_list List of base regression models.
#' @param df Data frame used for prediction.
#' @param validation_index Integer vector of row indices used as validation set.
#' @param target_col Name of the target column.
#' @param target_treatment Transformation used on target, if any.
#'
#' @return A list with \code{prediction_value} and \code{true_value}.
#' @export
stack_test_mlr <- function(stack_model, model_list, df, validation_index, target_col, target_treatment) {

  stack_df_val <- data.frame(matrix(nrow = length(validation_index), ncol = 0))

  for (i in 1:length(model_list)) {
    stack_df_val[[paste0("model", i)]] <- stats::predict(model_list[[i]], df[validation_index, ])
  }

  prediction_value <- stats::predict(stack_model, newdata = stack_df_val)
  true_value       <- df[[target_col]][validation_index]

  if (tolower(target_treatment) != "none") {
    prediction_value <- reverse_num(prediction_value, target_treatment)
    true_value       <- reverse_num(true_value, target_treatment)
  }

  rmse <- sqrt(mean((true_value - prediction_value)^2))
  cat("The rmse for stack model of these", length(model_list), "models is", rmse, ".")

  return(list(
    prediction_value = prediction_value,
    true_value       = true_value
  ))
}
