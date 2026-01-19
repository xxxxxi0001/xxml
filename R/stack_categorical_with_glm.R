#' Helper Function that Generate Data Frame Suitable for Stack Model Construction
#'
#' @param model_list List of Model Generated Before, support glm, RF, C5, rpart
#' @param df_list List of data frame that aligned with model list
#' @param index The index you wanna use to train this model, could be test or validation
#' @param positive Target's positive value
#'
#' @return A fitted stack data frame
#' @export
generate_stack_df<-function(model_list, df_list, index, positive) {
  
  # Initialization
  stack_df<-data.frame(matrix(nrow = length(index), ncol = 0))
  
  # Initialization
  prediction_list<-list()
  
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
  for (i in 1:length(model_list)) {
    model<-model_list[[i]]
    df<-df_list[[i]]
    
    # if is logistic regression, make prediction with type response
    if (inherits(model, "glm")) {
      prediction_list[[i]]<-predict(model, df[index,], type="response")
    }
    
    # if belongs to tree prediction, use probability make prediction
    else if (inherits(model, "randomForest") || inherits(model, "C5.0") || inherits(model,"rpart")) {
      prediction_list[[i]]<-predict(model, df[index,], type="prob")[, as.character(positive)]
    }
    
    # if other model type appeared, halt function and send suggestion
    else {
      stop("Only support logistic regression, randomForest, C5.0, rpart")
    }
    # paste prediction value into prediction list sequentially
    stack_df[[paste0("model",i)]]<-prediction_list[[i]]
  }
  return(stack_df)
}

#' Purpose is use a list of model to train a stack model 
#'
#' Support logistic regression, random forest, C5.0 & rpart
#' 
#' @param model_list List of Model Generated Before, support glm, RF, C5, rpart
#' @param df_list List of data frame that aligned with model list
#' @param test_index The index you use prepare to tune model
#' @param positive Target's positive value
#' @param target_col The feature you wanna make prediction
#'
#' @return A stack model made with a list of model
#' @export
stack_model_lr<-function(model_list,df_list,test_index,positive,target_col){
  
  # Generate data frame suitable for stack
  stack_df<-generate_stack_df(model_list,df_list,test_index, positive)
  # Get real value
  true_value<-as.numeric(as.character(df_list[[1]][[target_col]][test_index]))
  
  # Use real value & prediciton value build new logistic regression model
  stack_model<-glm(true_value~., data=stack_df, family=binomial)
  
  # as a stack model
  return (stack_model)
}

#' This function's purpose is to use the stack model you build earlier to make prediction
#' 
#' @param stack_model The stack_model you build earlier
#' @param model_list A list of model, support logistic regression, random forest, C5.0 & rpart
#' @param df_list A list of data frame that aligned with model list
#' @param index The index you wanna test this model, could be test & validation
#' @param positive The positive value of your target
#' @param negative The negative value of your target
#' @param target_col The feature you wanna make prediction
#' @param threshold I set to reasonable 0.5, but you can change this after threshold tune
#' 
#' @return Output the overall performance of stack model
#' @export 
stack_model_predict_lr<-function(stack_model,model_list,df_list,index,positive,negative,target_col,threshold=0.5){
  
  # Generate a data frame that suit stack model's purpose
  stack_df<-generate_stack_df(model_list,df_list,index, positive)
  
  # Get real value
  true_value<-as.numeric(as.character(df_list[[1]][[target_col]][index]))
  
  # Use prediction value & stack model make new prediction
  prediction_value<-predict(stack_model,newdata=stack_df,type="response")
  
  # Output performance of stack model
  result<-check_model_performance(prediction_value,threshold,positive,negative,df_list[[1]],index,target_col)
  
  # as a list
  return (list(
    prediction_value=prediction_value,
    true_value=true_value,
    accuracy=result$accuracy,
    tpr=result$tpr,
    tnr=result$tnr,
    F1=result$F1
  ))
}
