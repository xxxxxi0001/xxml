#' Reverse numerical value into original value, support log, log1p, sqrt, square, default none
#' 
#' @param x The variable that's been or not been transformed
#' @param treatment Could be "log","log1p","sqrt","square","none"
#' @return Transform back original value
#' @export 
reverse_num<-function(x,treatment) {
  if (tolower(treatment)=="log") {
    return(exp(x))
    }
  if (tolower(treatment)=="log1p") {
    return(exp(x)-1)
    }
  if (tolower(treatment)=="sqrt") {
    return(x^2)
    }
  if (tolower(treatment)=="square") {
    return(sqrt(x))
    }
  if (tolower(treatment)=="none") {
    return(x)
    }
}

#' Make Ensemble Prediction for Numcerical Prediction
#' @param model_list List of logistic regression model
#' @param df The data frame those index fall into
#' @param test_index The list of index for ensemble train
#' @param target_treatment If transformation occured in feature transformation
#' @return Mean Prediction made by number of ensemble's logistic model
#' @export
make_ensemble_predict_numerical<-function (model_list,df,test_index,target_treatment="none") {
  
  # Initialization
  prediction_list<-list()
  
  # For each model in ensemble model, make prediction
  for (i in 1:length(model_list)) {
    prediction_list[[i]]<-predict(model_list[[i]], df[test_index,], type="response")
  }
  
  #  & calculate mean
  ensemble_predictions<-Reduce("+",prediction_list)/length(model_list)
  
  # check if transformation occured or not
  if (tolower(target_treatment)!="none") {
    ensemble_predictions<-reverse_num(ensemble_predictions,target_treatment)
  }
  
  return(ensemble_predictions)
}

#' Calculate How Each Ensemble Model should be weighted in Overall Model with RMSE
#' @param model_list The list of ensemble model
#' @param df Data frame that you use to make prediction
#' @param test_index The test index selected earlier
#' @param target_col That column that you want to make prediction for
#' @param target_treatment If target column has been transformed, what method it use
#' @return How each model should contribute to overall prediction
#' @export
ensemble_weight_RMSE<-function(model_list,df,test_index,target_col,target_treatment="none") {
  
  # Initialization
  prediction<-list()
  rmse<-numeric(length(model_list))
  weight_list<-numeric(length(model_list))
  rmse_total<-0
  
  # For each model, make prediction with test index & get their 
  # original value with function `reverse_num`
  # Then use original value calculate rmse & rmse total for 
  # later weight calculation
  for (i in 1:length(model_list)) {
    prediction[[i]]<-predict(model_list[[i]],df[test_index,])
    real_value<-df[[target_col]][test_index]
    prediction_value<-prediction[[i]]
    
    # Only run into reverse_num if transformation occured
    if (tolower(target_treatment)!="none") {
      real_value<-reverse_num(real_value,target_treatment)
      prediction_value<-reverse_num(prediction_value,target_treatment)
    }

    rmse[i]<-sqrt(mean((real_value-prediction_value)^2))
    cat("The model",i,"'s RMSE is",round(rmse[i],3),"\n")
    
    rmse_total<-rmse[i]+rmse_total
  }
  
  # Calculate weight and output result
  weight_list<-(1/rmse^2)/sum(1/rmse^2)
  cat("Each of their weight are",round(as.numeric(weight_list),3),".")
  
  # Return weight for later use
  return(as.list(weight_list))
}

