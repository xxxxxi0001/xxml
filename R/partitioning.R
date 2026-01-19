#' Split data into train, test, and validation with stratified sampling
#'
#' @param df Data frame that used for partition
#' @param target_col Specific column that you don't want to fall in class imbalance (usually column used for prediction)
#' @param train_portion Portion of data select for training
#' @param test_portion Portion of data select for testing (rest for validation)
#' @param positive Target column's positive value
#' @param negative Target column's negative value
#' @return List with train_index, test_index, validation_index
#' @export
three_set_partition<-function(df, target_col, train_portion, test_portion,positive,negative){

  # Get positive/negative index
  p_index<-which(df[[target_col]]==positive)
  n_index<-which(df[[target_col]]==negative)

  # initialization for partition
  n_p<-length(p_index)
  n_n<-length(n_index)

  # Randomly selected designated train index
  p_train<-sample(p_index,size=floor(train_portion*n_p))
  n_train<-sample(n_index,size=floor(train_portion*n_n))
  train_index<-c(p_train,n_train)
  # shuffle
  train_index<-sample(train_index)

  # Get rest index
  rest_p_i<-setdiff(p_index,p_train)
  rest_n_i<-setdiff(n_index,n_train)
  
  # For rest
  nr_p<-length(rest_p_i)
  nr_n<-length(rest_n_i)

  # choose designated portion for testing
  test_num<-test_portion/(1-train_portion)
  p_test<-sample(rest_p_i,size=floor(test_num*nr_p))
  n_test<-sample(rest_n_i,size=floor(test_num*nr_n))
  test_index<-c(p_test,n_test)
  # shuffle
  test_index<-sample(test_index)

  # rest is validation
  p_val<-setdiff(rest_p_i,p_test)
  n_val<-setdiff(rest_n_i,n_test)
  validation_index<-c(p_val,n_val)
  # shuffle
  validation_index<-sample(validation_index)
  
  # Return train, test, validation index as a list
  return(list(
    train_index=train_index,
    test_index=test_index,
    validation_index=validation_index
  ))
}

#' Create ensemble training subsets with stratified sampling & bootstrapping
#'
#' @param df Data frame
#' @param train_index Overall train indices
#' @param sub_num Number of subsets
#' @param sub_portion Proportion of each class to sample
#' @param target_col Target column
#' @param positive Positive class label
#' @param negative Negative class label
#' @return A list contain sub_num number of training index
#' @export
ensemble_train_partition<-function(df,train_index,sub_num,sub_portion,target_col,positive,negative) {
  
  # Initialize a list to store partition index
  partitions<-vector("list",sub_num)
  
  # Get train_index
  train_label<-df[[target_col]][train_index]
  
  # Get positive negative's index
  positive_index<-train_index[train_label==positive]
  negative_index<-train_index[train_label==negative]
  
  # Randomly choose designated number (sub_num) of subset from positive/negative
  # index with designated portion (sub_portion) of number
  # therefore it won't fall in class imbalance
  for (i in 1:sub_num){
    nt_p<-length(positive_index)
    nt_n<-length(negative_index)
    subset_train_index_p<-sample(positive_index,size=floor(sub_portion*nt_p),replace=TRUE)
    subset_train_index_n<-sample(negative_index,size=floor(sub_portion*nt_n),replace=TRUE)
    # clollect selected index in partition list with shuffle so positive/negative 
    # won't cluster together
    partitions[[i]]<-sample(c(subset_train_index_p,subset_train_index_n))
  }
  
  # Return a list of partition index
  return(partitions)
}

#' Split Data into Train, Test, and Validation Sets (No Target Stratification)
#'
#' Randomly splits a data frame into train, test, and validation sets using
#' specified proportions, without stratifying by any target.
#'
#' @param df A data frame.
#' @param train_portion Proportion of rows used for training.
#' @param test_portion Proportion of rows used for testing (of total rows;
#'   the remaining go to validation).
#'
#' @return A list contain Training Index, Test Index & Validation Index (no stratification)
#' @export
three_set_partition_no_target<-function(df, train_portion, test_portion){

  # Initialize total size
  n<-nrow(df)
  
  # Initialize train size
  train_size<-floor(n*train_portion)
  
  # Get Train Index
  train_index<-sample(1:n,size=train_size)
  
  # Get Rest Index
  rest_index<-setdiff(1:n, train_index)
  
  # From rest, get test_index
  nr<-length(rest_index)
  test_num<-test_portion/(1-train_portion)
  test_size<-floor(nr*test_num)
  test_index<-sample(rest_index,size=test_size)
  
  # Lest is validation
  validation_index<-setdiff(setdiff(1:n,train_index),test_index)
  
  # Return train, test, validation index as a list
  return(list(
    train_index=train_index,
    test_index=test_index,
    validation_index=validation_index
  ))
}
