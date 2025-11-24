#' Initialize distance matrix for kNN imputation
#'
#' @param df Data frame
#' @return Euclidean distance matrix for numeric columns without NA
#' @export
initialize_distance_find_best_k <- function(df){

  numerical_columns <- names(df)[sapply(df, is.numeric)]
  excluded_columns  <- names(df)[colSums(is.na(df)) > 0]
  target_columns    <- setdiff(numerical_columns, excluded_columns)

  df_matrix   <- as.matrix(df[, target_columns])
  df_matrix   <- scale(df_matrix)
  df_distance <- as.matrix(dist(df_matrix, method = "euclidean"))

  return(df_distance)
}

#' Get indices of non-NA values in a feature
#'
#' @param target_feature Numeric vector
#' @return Integer vector of indices without NA
#' @export
initialize_not_na_index <- function(target_feature){
  not_na_index <- which(!is.na(target_feature))
  return(not_na_index)
}

#' Sample indices to tune k for kNN imputation
#'
#' @param seed_num Seed for reproducibility
#' @param test_proportion Proportion of non-NA indices to sample
#' @param not_na_index Indices of non-NA values
#' @return Sampled indices to tune k
#' @export
initialize_test_k_index <- function(seed_num, test_proportion, not_na_index) {

  set.seed(seed_num)
  test_k_index <- sample(not_na_index,
                         size = floor(test_proportion * length(not_na_index)))
  return(test_k_index)
}

#' Find best k for kNN imputation based on RMSE
#'
#' @param max_k Maximum k to search
#' @param test_k_index Indices used for tuning
#' @param df_distance Distance matrix
#' @param not_na_index Non-NA indices
#' @param target_feature Numeric feature
#' @return Best k (integer)
#' @export
find_best_k <- function(max_k, test_k_index, df_distance,
                        not_na_index, target_feature){

  smallest_RMSE <- Inf
  smallest_k    <- Inf
  df_temp       <- df_distance

  for (k in 1:max_k) {

    se_total <- 0

    for (j in test_k_index){

      df_temp[j, j] <- Inf

      total_distance <- df_temp[j, not_na_index]
      ordered_dist   <- order(total_distance)
      original_index <- not_na_index[ordered_dist][1:k]

      test_value   <- round(mean(target_feature[original_index]))
      actual_value <- target_feature[j]

      se_total <- se_total + (actual_value - test_value)^2
    }

    RMSE <- sqrt(se_total / length(test_k_index))

    if (RMSE < smallest_RMSE){
      smallest_RMSE <- RMSE
      smallest_k    <- k
    }
  }
  cat("The best k is", smallest_k,"which give smallest error of",
      smallest_RMSE,"\n")

  rm(df_temp, total_distance)
  gc()

  return(smallest_k)
}

#' Perform kNN imputation for a single feature
#'
#' @param df Data frame
#' @param smallest_k Chosen k
#' @param target_feature Numeric vector with NA
#' @param df_distance Distance matrix
#' @return Imputed numeric vector
#' @export
kNN_Imputation <- function(df, smallest_k, target_feature, df_distance){

  k       <- smallest_k
  na_cols <- which(is.na(target_feature))
  na_rows <- which(is.na(target_feature))
  df_temp <- df_distance

  for (i in na_rows) {

    df_temp[i, i]      <- Inf
    df_temp[i, na_cols] <- Inf

    closest_k <- order(df_temp[i, ])[1:k]

    target_feature[i] <- round(mean(target_feature[closest_k]))
  }
  return(target_feature)
}

#' Automation kNN imputation for all columns with NA
#'
#' @param df Data frame
#' @param test_por Portion of Data Set that will be used to test best k
#' @param max_k Maximum number of k test for best k
#' @param ignore_col Character vector of columns to ignore
#' @return Data frame with selected columns imputed
#' @export
automation_knn_imputation <- function(df, test_por=0.1, max_k=20, ignore_col = NULL) {

  na_col_name <- names(df)[colSums(is.na(df)) > 0]

  if (!is.null(ignore_col)) {
    na_col_name <- setdiff(na_col_name, ignore_col)
  }

  for (i in na_col_name) {
    target_feature <- df[[i]]
    df_distance    <- initialize_distance_find_best_k(df)
    not_na_index   <- initialize_not_na_index(target_feature)
    test_k_index   <- initialize_test_k_index(888, test_por, not_na_index)
    cat("Now is processing column", i, "\n")
    smallest_k     <- find_best_k(max_k, test_k_index, df_distance,
                                  not_na_index, target_feature)
    df[[i]]        <- kNN_Imputation(df, smallest_k, target_feature,
                                     df_distance)
    cat("Feature", i, "is successfully imputed. \n")

    rm(df_distance)
    gc()
  }
  return(df)
}
