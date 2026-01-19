#' Create Stack Model for MLR
#' 
#' @param df Data frame that you use to make prediction
#' @param target_col That column that you want to make prediction for
#' @param test_index The test index selected earlier
#' @param model_list The list of ensemble model you made ealier
#' @return stacked model you generate from ensemble models
#' @export 
create_stack_model_mlr<-function (df,target_col,test_index,model_list) {
  
  # Create stack_df with length of test_index
  stack_df<-data.frame(true_value=df[[target_col]][test_index])
  
  # For every model, make prediction with data set of test_index
  # and collect their result in stack_df in format of 
  # mi<-prediction value
  for (i in 1:length(model_list)) {
    stack_df[[paste0("model",i)]]<-predict(model_list[[i]],df[test_index,])
  }
  
  # Find relationship between true value and each predicted 
  # value that generated from each model
  # The stack model will automatically tells you how each model 
  # should contribute to the overall final prediction
  stack_model<-lm(true_value~., data=stack_df)
  return (stack_model)
}

#' Use Stack Model Make Prediction & Check Its Accuracy with RMSE
#' 
#' @param stack_model The stack model you build earlier with ensemble models
#' @param model_list The list of ensemble model you made ealier
#' @param df Data frame that you use to make prediction
#' @param validation_index Index used for validation purpose
#' @param target_col The feature you want to make prediction with
#' @return Stack model's predict value & actual value & RMSE
#' @export
stack_test_mlr<-function (stack_model,model_list,df,validation_index,target_col,target_treatment="none") {
  
  # Create an empty data frame with length of validation index
  stack_df_val<-data.frame(matrix(nrow=length(validation_index),ncol=0))
  
  # Input prediction value of each ensemble model made on validation index
  # in format of mi<-prediction value
  for (i in 1:length(model_list)) {
    stack_df_val[[paste0("model",i)]]<-predict(model_list[[i]],df[validation_index,])
  }
  
  # Make prediction with stack model, each ensemble model contribute differently 
  # to the overall prediction, collect those value
  prediction_value<-predict(stack_model,newdata=stack_df_val)
  # Get their real value
  true_value<-df[[target_col]][validation_index]
  
  # Call `reverse_num` function if transformation occured
  # change them back to their original value
  if (tolower(target_treatment) != "none") {
    prediction_value<-reverse_num(prediction_value,target_treatment)
    true_value<-reverse_num(true_value,target_treatment)
  }
  
  # Calculate rmse based on prediction & real value & output result
  rmse<-sqrt(mean((true_value-prediction_value)^2))
  cat("The rmse for stack model of these",length(model_list),"models is",rmse,".")
  
  # Get prediction & true value for more possible testify like plot or cor
  return(list(
    prediction_value=prediction_value,
    true_value=true_value
  ))
}
