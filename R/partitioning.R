#' Split data into train, test, and validation with stratified sampling
#'
#' @param df Data frame
#' @param seed_num Seed
#' @param target_col Target column name
#' @param train_portion Train proportion
#' @param test_portion Test proportion (of full data)
#' @param positive Positive class label
#' @param negative Negative class label
#' @return List with train_index, test_index, validation_index
#' @export
three_set_partition <- function(df, seed_num, target_col,
                                train_portion, test_portion,
                                positive, negative){

  set.seed(seed_num)

  donated_index    <- which(df[[target_col]] == positive)
  not_donated_index <- which(df[[target_col]] == negative)

  n_donated     <- length(donated_index)
  n_n_donated   <- length(not_donated_index)

  donated_train   <- sample(donated_index, size = floor(train_portion * n_donated))
  n_donated_train <- sample(not_donated_index, size = floor(train_portion * n_n_donated))
  train_index_encoded <- c(donated_train, n_donated_train)
  train_index_encoded <- sample(train_index_encoded)

  rest_dona_i   <- setdiff(donated_index, donated_train)
  rest_n_dona_i <- setdiff(not_donated_index, n_donated_train)

  nr_dona   <- length(rest_dona_i)
  nr_n_dona <- length(rest_n_dona_i)

  test_num       <- test_portion / (1 - train_portion)
  donated_test   <- sample(rest_dona_i,   size = floor(test_num * nr_dona))
  n_donated_test <- sample(rest_n_dona_i, size = floor(test_num * nr_n_dona))
  test_index_encoded <- c(donated_test, n_donated_test)
  test_index_encoded <- sample(test_index_encoded)

  donated_val   <- setdiff(rest_dona_i,   donated_test)
  n_donated_val <- setdiff(rest_n_dona_i, n_donated_test)
  validation_index_encoded <- c(donated_val, n_donated_val)
  validation_index_encoded <- sample(validation_index_encoded)

  return(list(
    train_index      = train_index_encoded,
    test_index       = test_index_encoded,
    validation_index = validation_index_encoded
  ))
}

#' Create ensemble training subsets with stratified sampling & bootstrapping
#'
#' @param df Data frame
#' @param seed_num Seed
#' @param train_index Overall train indices
#' @param sub_num Number of subsets
#' @param sub_portion Proportion of each class to sample
#' @param target_col Target column
#' @param positive Positive class label
#' @param negative Negative class label
#' @return List of index vectors
#' @export
ensemble_train_partition <- function(df, seed_num, train_index,
                                     sub_num, sub_portion,
                                     target_col, positive, negative) {

  set.seed(seed_num)
  partitions <- vector("list", sub_num)

  train_label <- df[[target_col]][train_index]

  positive_index <- train_index[train_label == positive]
  negative_index <- train_index[train_label == negative]

  for (i in 1:sub_num){
    nt_p <- length(positive_index)
    nt_n <- length(negative_index)
    subset_train_index_p <- sample(positive_index,
                                   size = floor(sub_portion * nt_p),
                                   replace = TRUE)
    subset_train_index_n <- sample(negative_index,
                                   size = floor(sub_portion * nt_n),
                                   replace = TRUE)
    partitions[[i]] <- sample(c(subset_train_index_p, subset_train_index_n))
  }

  return(partitions)
}

#' Check class balance for indices or list of index sets
#'
#' @param df Data frame
#' @param index_list Numeric vector or list of numeric vectors
#' @param target_col Target column
#' @param positive Positive class label
#' @param negative Negative class label
#' @return Printed messages
#' @export
check_class_imbalance <- function(df, index_list, target_col, positive, negative) {

  if (is.list(index_list)) {
    for (i in 1:length(index_list)) {
      index <- unlist(index_list[[i]])
      t <- round(mean(df[[target_col]][index] == positive) * 100, 2)
      f <- round(mean(df[[target_col]][index] == negative) * 100, 2)
      cat("In partition", i, "of", deparse(substitute(index_list)),
          "There are", t, "% postive people in training data set and",
          f,"% negative people in training data set.","\n")
    }
  }

  if (is.numeric(index_list)) {
    index <- unlist(index_list)
    t <- round(mean(df[[target_col]][index] == positive) * 100, 2)
    f <- round(mean(df[[target_col]][index] == negative) * 100, 2)
    cat("In", deparse(substitute(index_list)), "There are", t,
        "% positive people in training data set and", f,
        "% negative people in training data set.","\n")
  }
}

#' Create k-fold stratified CV index list from train indices
#'
#' @param k Number of folds
#' @param df Data frame
#' @param target_col Target column
#' @param train_index Train indices
#' @param positive Positive class
#' @param negative Negative class
#' @return List of length k with indices
#' @export
k_stratified_cv <- function(k, df, target_col, train_index,
                            positive, negative){

  train_label <- df[[target_col]][train_index]

  positive_label <- which(train_label == positive)
  negative_label <- which(train_label == negative)

  positive_index <- train_index[positive_label]
  negative_index <- train_index[negative_label]

  length_sub_p <- floor(length(positive_index) / k)
  length_sub_n <- floor(length(negative_index) / k)

  index_list <- vector("list", k)

  for (i in 1:k) {

    if (i == k){
      index_list[[i]] <- c(positive_index, negative_index)
      break
    }

    p_sub <- positive_index[1:length_sub_p]
    n_sub <- negative_index[1:length_sub_n]

    index_list[[i]] <- sample(c(p_sub, n_sub))

    positive_index <- positive_index[-(1:length_sub_p)]
    negative_index <- negative_index[-(1:length_sub_n)]
  }
  return(index_list)
}
