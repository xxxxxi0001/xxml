#' Backward p-value for Multi-Linear Regression Model
#' 
#' @param df The data frame those index fall into
#' @param train_index The index for ensemble/not ensemble train 
#' @param target_col The target column that need to be predicted
#' @return A list of trained emseble Logistic Model Based on List of Index you provide
#' @export 
backward_p_mlr<-function (df,train_index,target_col) {
  
  # For index create for ensemble model, give ensemble model
  if (is.list(train_index)) {
    regression_model<-list()
    for (i in 1:length(train_index)) {
      
      # Initialize Index of Training
      index<-unlist(train_index[[i]])
      
      # Initialize Model
      regression_model[[i]]<-lm(as.formula(paste(target_col, "~ .")), data=df[index,],na.action=na.omit)
      
      # Initialize Coefficient
      coeffecient<-summary(regression_model[[i]])$coefficients
      
      # Initialize p-value list
      p_values<-coeffecient[-1,"Pr(>|t|)"]
    
      # If non-significant p appear, run in loop
      if (max(p_values)>0.05) {
        repeat {
          # Get significant features, rerun model and write over model with new model
          significant_feature<-names(p_values[p_values<0.05])
          string_features<-as.formula(paste(target_col, "~", paste(significant_feature,collapse="+")))
          regression_model[[i]]<-lm(string_features,data=df[index,],na.action=na.omit)
      
          # write over coefficient with important coefficient
          coeffecient<-summary(regression_model[[i]])$coefficients
     
          # write over p-value with important p
          p_values<-coeffecient[-1,"Pr(>|t|)"]
      
          # if all p are good, end loop 
          if (max(p_values)<0.05) break
          
          # If all p-values are NA, end loop and call error    
          if (all(is.na(p_values))) {
            cat("All p-values are NA, this model is not working")
            break
            }
          }
        }
      }
    }
  
  # For index create for only one model, return one model
  if (is.numeric(train_index)) {
    
    # Initialize Index of Training
    index<-train_index
    # Initialize Model
    regression_model<-lm(as.formula(paste(target_col, "~ .")), data=df[index,],na.action=na.omit)
    # Initialize Coefficient
    coeffecient<-summary(regression_model)$coefficients
    # Initialize p-value list
    p_values<-coeffecient[-1,"Pr(>|t|)"]
    
    # If non-significant p appear, run in loop
    if (max(p_values)>0.05) {
      
      repeat {
        # Get significant features, rerun model and write over model with new model
        significant_feature<-names(p_values[p_values<0.05])
        string_features<-as.formula(paste(target_col, "~", paste(significant_feature,collapse="+")))
        regression_model<-lm(string_features,data=df[index,],na.action=na.omit)
      
        # write over coefficient with important coefficient
        coeffecient<-summary(regression_model)$coefficients
     
        # write over p-value with important p
        p_values<-coeffecient[-1,"Pr(>|t|)"]
      
        # if all p are good, end loop 
        if (max(p_values)<0.05) break
        # If all p-values are NA, end loop and call error 
        if (all(is.na(p_values))) {
          cat("All p-values are NA, this model is not working")
          break
          }
        }
      }
    }
  return(regression_model)  
}
