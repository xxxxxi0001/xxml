#' Check model performance: accuracy, TPR, TNR, F1
#'
#' @param predict_prob The prediction result (in probability/response)
#' @param threshold Above what number is positive
#' @param positive Target's positive value 
#' @param negative Target's negative value 
#' @param df The data frame you use to make prediction
#' @param test_index The index you select for test
#' @param target_col Your target name 
#' @return A list of result include accuracy, tpr, tnr, F1 value and output result
#' @export
check_model_performance<-function (predict_prob, threshold, positive, negative, df, test_index, target_col) {
  
  # Make sure its not a list but a series of numerical value that can 
  # be used for comparison
  if (is.list(predict_prob)){
    predict_prob<-unlist(predict_prob)
  }
  
  # First get prediciton & real value
  prediction<-ifelse(predict_prob>=threshold,positive,negative)
  true_value<-as.numeric(as.character(df[[target_col]][test_index]))
  
  # Calculate their tn, tp, fp, fn
  TN<-sum(prediction==negative&true_value==negative)  
  TP<-sum(prediction==positive&true_value==positive)
  FP<-sum(prediction==positive&true_value==negative)
  FN<-sum(prediction==negative&true_value==positive)
  
  # Calculate their precision & recall for F1
  # If denominator is zero, call zero as result to avoid NA
  precision<-ifelse(TP+FP==0, 0, TP/(TP+FP))
  recall<-ifelse(TP+FN==0, 0, TP/(TP+FN))
  F1<-ifelse(precision+recall==0, 0, round(2*precision*recall/(precision+recall),2))
  
  # Calculate accuracy, tpr, tnr
  accuracy<-round(((TP+TN)/(TP+TN+FP+FN))*100,2)
  tpr<-round((TP/(TP+FN))*100,2)
  tnr<-round((TN/(TN+FP))*100,2)
  
  # Print out result
  cat("The accuracy is", accuracy, "%. The True Positive Rate is ", tpr, "%. And the True Negative Rate is ", tnr, "% and the F1 score is", F1,".\n")
  
  return(list(
    accuracy=accuracy,
    tpr=tpr,
    tnr=tnr,
    F1=F1))
}

#' Check class imbalance based on target
#'
#' @param df The data frame those index fall into
#' @param index_list The list of index you want to check if fall into class imbalance
#' @param target_col The column that used to check class imbalance 
#' @param positive Positive value of target_col
#' @param negative Negative value of target_col
#' @return No Return, but will output result
#' @export
check_class_imbalance<-function(df,index_list,target_col,positive,negative) {
  
  # If index list is a list of index list
  if (is.list(index_list)) {
    for (i in 1:length(index_list)) {
      
      # Loop over each list & change each list into serial of 
      # numerical value & calculate portion of positive/negative value, return result
      index<-unlist(index_list[[i]])
      t<-round(mean(df[[target_col]][index]==positive)*100,2)
      f<-round(mean(df[[target_col]][index]==negative)*100,2)
      cat("In partition",i, "of", deparse(substitute(index_list)),"There are", t, "% postive people in training data set and",f,"% negative people in training data set.","\n")
    }
  }
  
  # If index list is only one list of number
  if (is.numeric(index_list)) {
    
    # calculate portion of positive/negative value, return result
    index<-unlist(index_list)
    t<-round(mean(df[[target_col]][index]==positive)*100,2)
    f<-round(mean(df[[target_col]][index]==negative)*100,2)
    cat("In",deparse(substitute(index_list)),"There are", t, "% positive people in training data set and",f,"% negative people in training data set.","\n")
  }
}

#' This Function Support Logistic Regression, RandomForest, C5.0 & rpart
#'
#' @param model_list List of logistic regression model
#' @param df_list The list of data frame that aligned with model_list
#' @param test_index The list of index for ensemble train
#' @param positive Positive value of target_col
#' @param target_treatment If target feature is transformed in feature transformation (default="none")
#' @return No Return, but will output result
#' @export
make_ensemble_predict_categorical<-function (model_list,df_list,test_index,positive,target_treatment="none") {
  
  # Initialization
  prediction_list<-vector("list",length(model_list))
  
  # If the list of model only has one set of data frame
  # make replication so its a list of data frame
  if (inherits(df_list,"data.frame")) {
    df_list <- rep(list(df_list), length(model_list))
  }
  
  # send warning if model list & data frame list is not same length
  if (length(model_list) != length(df_list)) {
    stop("Model List and Data Frame List must be equal number")
  }
  
  # For each model in ensemble model, make prediction
  for (i in seq_along(model_list)) {
    model<-model_list[[i]]
    df<-df_list[[i]]
    if (inherits(model, "glm")) {
      prediction_list[[i]]<-predict(model_list[[i]], df[test_index,], type="response")
    }
    else if (inherits(model, "randomForest") || inherits(model, "C5.0") || inherits(model,"rpart")) {
      prediction_list[[i]]<-predict(model, df[test_index,], type="prob")[, as.character(positive)]
    }
    else {
      stop("Only support logistic regression, randomForest, C5.0, rpart")
    }
  }
  
  #  & calculate mean
  ensemble_predictions<-Reduce("+",prediction_list)/length(model_list)
  
  # make sure its number not list to avoid error
  ensemble_predictions <- as.numeric(ensemble_predictions)
  
  # check if transformation occured or not
  if (tolower(target_treatment)!="none") {
    ensemble_predictions<-reverse_num(ensemble_predictions)
  }
  
  return(ensemble_predictions)
}

#' Reminder: target must be numerical value
#'
#' @param predict_prob The prediction result (in probability/response)
#' @param df The data frame you use to make prediction
#' @param test_index The index you select for test
#' @param target_col Your target name
#' @param positive Target positive value
#' @param negative Target negative value
#' @return Best Threshold with optimal F1 value 
#' @export
find_best_threshold<-function (predict_prob, df,test_index, target_col, positive, negative) {
  
  # Get true value
  real_value<-as.numeric(as.character(df[[target_col]][test_index]))
  # Initialize Threshold & F1 for later use
  best_threshold<--Inf
  largest_F1<--Inf
  
  # Loop over all possible threshold
  for (i in seq(0.01,0.99,by=0.01)) {
    
    # Make prediction based on threshold i
    prediction_result<-ifelse(predict_prob >= i,positive,negative)
    
    # Calculate threshold i's F1
    TP<-sum(prediction_result==positive&real_value==positive)
    FP<-sum(prediction_result==positive&real_value==negative)
    FN<-sum(prediction_result==negative&real_value==positive)
    precision<-ifelse(TP+FP==0, 0, TP/(TP+FP))
    recall<-ifelse(TP+FN==0, 0, TP/(TP+FN))
    F1<-ifelse(precision+recall==0, 0, 2*precision*recall/(precision+recall))
    
    # If F1 larger than any previous F1
    if (!is.na(F1) && F1>largest_F1){
      
      # update F1
      largest_F1<-F1
      
      # update threshold
      best_threshold<-i
    }
  }
  
  # Output results
  cat("The best threshold is", best_threshold, "and it gives largest F1",round(largest_F1,3),".")
  return(best_threshold)
}

#' Get Each Ensemble Model Weight with F1
#'
#' @param model_list The list of model, support logistic regression, C5.0, rpart, random forest
#' @param df_list The list of data frame align with model_list
#' @param test_index The index of 25% testing
#' @param best_threshold The best threshold you get after run function "find_best_threshold"
#' @param target_col The target column that need to make prediction
#' @param positive Target positive value
#' @param negative Target negative value
#' @return Each Ensemble Model's Weight
#' @export
ensemble_weight_F1<-function(model_list, df_list,test_index, best_threshold, target_col, positive, negative) {
  
  # Initialize a list to store F1 value
  F1_list<-list()
  
  # Initialization
  prediction_list<-vector("list",length(model_list))
  
  # If the list of model only has one set of data frame
  # make replication so its a list of data frame
  if (inherits(df_list,"data.frame")) {
    df_list <- rep(list(df_list), length(model_list))
  }
  
  # send warning if model list & data frame list is not same length
  if (length(model_list) != length(df_list)) {
    stop("Model List and Data Frame List must be equal number")
  }
  
  # Loop over all ensemble model
  for (i in 1:length(prediction_list)) {
    model<-model_list[[i]]
    df<-df_list[[i]]
    
    # if is logistic regression, make prediction with type response
    # and get its model type for output result
    if (inherits(model, "glm")) {
      prediction_list[[i]]<-predict(model_list[[i]], df[test_index,], type="response")
      model_type<-"Logistic Regression"
    }
    
    # if belongs to tree prediction, use probability make prediction and
    else if (inherits(model, "randomForest") || inherits(model, "C5.0") || inherits(model,"rpart")) {
      prediction_list[[i]]<-predict(model, df[test_index,], type="prob")[, as.character(positive)]
      
      # get their model type for output result
      if (inherits(model,"randomForest")) {
        model_type<-"Random Forest"
      }
      else if (inherits(model,"C5.0")) {
        model_type<-"C5.0"
      }
      else if (inherits(model,"rpart")) {
        model_type<-"Decision Tree"
      }
    }
    
    # if other model type appeared, halt function and send suggestion
    else {
      stop("Only support logistic regression, randomForest, C5.0, rpart")
    }
  
  # get prediction value with best threshold & its true value
  prediction<-ifelse(prediction_list[[i]] >= best_threshold,positive,negative)
  real_value<-as.numeric(as.character(df[[target_col]][test_index]))
    
  # Calculate F1 with true value and predicted value and get it into list
  TP<-sum(prediction==positive&real_value==positive)
  FP<-sum(prediction==positive&real_value==negative)
  FN<-sum(prediction==negative&real_value==positive)
  precision<-TP/(TP+FP)
  recall<-TP/(TP+FN)
  F1<-2*(precision*recall)/(precision+recall)
  F1_list[[i]]<-F1
  
  # out put result
  cat("The model",model_type, i,"'s F1 is",round(F1_list[[i]],3),"\n")
  }
  
  # Calculate weight and get it into list
  F1_vector<-unlist(F1_list)
  F1_weights<-F1_vector/sum(F1_vector)
  weight_list<-as.list(F1_weights)
  
  # output weight result
  cat("Each of their weight are",round(as.numeric(weight_list),3),".")
  
  return(weight_list)
}

#' Support Logistic Regression, random forest, C5.0, rpart
#'
#' @param model_list The list of model, support logistic regression, C5.0, rpart, random forest
#' @param df_list The list of data frame align with model_list
#' @param index The index you wanna try with this model (usually test & val index)
#' @param weight_list The weight you get for each ensemble
#' @param positive Target's positive value
#' @param target_treatment If target feature is transformed in feature transformation (default="none")
#' @return List of prediction (in probability) made with ensemble Logistic Regression model
#' @export
ensemble_result_with_weight<-function(model_list,df_list,index,weight_list,positive,target_treatment="none") {
  
  # If the list of model only has one set of data frame
  # make replication so its a list of data frame
  if (inherits(df_list,"data.frame")) {
    df_list <- rep(list(df_list), length(model_list))
  }
  
  # Check if model, weight and data frame are in same length, sending warning if not
  if (length(model_list) != length(weight_list)) {
    stop("Model List and Weight List must be equal number")
  }
  else if (length(model_list) != length(df_list)) {
    stop("Model List and Data Frame List must be equal number")
  }
  
  # Initialization
  prediction_list<-vector("list",length(model_list))
  
  # For each model in ensemble model, make prediction
  # if is logistic regression, make prediction with response
  # if is tree prediction, make prediction with probability
  # if other model appeared, halt function & send suggestion
  for (i in seq_along(model_list)) {
    model<-model_list[[i]]
    df<-df_list[[i]]
    if (inherits(model, "glm")) {
      prediction_list[[i]]<-predict(model, df[index,], type="response")
      prediction_list[[i]]<-prediction_list[[i]]*weight_list[[i]]
    }
    else if (inherits(model, "randomForest") || inherits(model, "C5.0") || inherits(model,"rpart")) {
      prediction_list[[i]]<-predict(model, df[index,], type="prob")[, as.character(positive)]
      prediction_list[[i]]<-prediction_list[[i]]*weight_list[[i]]
    }
    else {
      stop("Only support logistic regression, randomForest, C5.0, rpart")
    }
  }
  
  # Add up as final prediction value
  ensemble_predictions<-Reduce("+",prediction_list)
  
  # Calculate ensemble model's mean as final prediciton value
  if (tolower(target_treatment)!="none") {
    ensemble_predictions<-reverse_num(ensemble_predictions,target_treatment)
  }
  
  return(ensemble_predictions)
}
