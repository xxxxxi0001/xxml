#' Split index into k fold
#'
#' @param k How many fold you want to split
#' @param df Data Frame you wanna test with
#' @param target_col The column that need to be predict
#' @param train_index Training index that need to split in k fold
#' @param positive target_col's positive value
#' @param negative target_col's negative value
#' @return k stratified index list 
#' @export
k_stratified_cv<-function(k,df,target_col,train_index,positive,negative){
  
  # Get train_index
  train_label<-df[[target_col]][train_index]
  
  # Get positive negative's index
  positive_label<-which(train_label==positive)
  negative_label<-which(train_label==negative)
  
  # Get positive negative's local position in train index
  positive_index<-train_index[positive_label]
  negative_index<-train_index[negative_label]
  
  # Initialize length of positive/negative that need to be selected
  length_sub_p<-floor(length(positive_index)/k)
  length_sub_n<-floor(length(negative_index)/k)
  
  # Initialize a list to store all index
  index_list<-vector("list",k)
  
  # Loop over k stratified
  for (i in 1:k) {
    
    # If this is the last one, include all index (so no one is left out)
    # Avoid residual problem
    if (i==k){
      index_list[[i]]<-c(positive_index,negative_index)
      break
    }
  
    # Get positive index, negative index as designated portion so it won't 
    # fall in class imbalance
    p_sub<-positive_index[1:length_sub_p]
    n_sub<-negative_index[1:length_sub_n]
  
    # combine them as index list and shuffle so it won't be positive/negative 
    # cluster together
    index_list[[i]]<-sample(c(p_sub,n_sub))
    # remove selected index
    positive_index<-positive_index[-(1:length_sub_p)]
    negative_index<-negative_index[-(1:length_sub_n)]
  }
  return(index_list)
}

#' Cross Validation with k Stratified Fold Index
#'
#' @param df Data Frame you wanna test with
#' @param index_list The index list generated earlier (with function k_stratified_cv)
#' @param positive target_col's positive value
#' @param negative target_col's negative value
#' @param positive_weight The portion of positive of overall value
#' @param negative_weight The portion of negative of overall value
#' @param target_col The column need to be predicted
#' @param model_type This function support "logistic regression", "random forest","c5.0","decision tree"
#' @param threshold Above threshold is considered positive, vice versa (default=0.5)
#' @return k stratified index list 
#' @export
cross_validation<-function(df,index_list,positive,negative,positive_weight,negative_weight,target_col,model_type,threshold=0.5){
  
  # make sure target feature is factor (categorical feature)
  df[[target_col]]<-factor(df[[target_col]], levels=c(negative, positive))
  
  # Initialization for overall performance calculation
  F1_total<-0
  accuracy_total<-0
  tpr_total<-0
  tnr_total<-0
  
  # For each index list in k fold index list
  for (i in 1:length(index_list)) {
    
    # Select list i as test, rest as train
    test_index<-index_list[[i]]
    train_index<-unlist(index_list[-i])
    
    # If model_type is logistic regression, train logistic regression model with backward p
    if (tolower(model_type) %in% c("logistic regression","lr")) {
      
      # train model with train index
      model<-backward_p_lr(df,train_index,target_col,positive,positive_weight,negative_weight)
    
      # get prediction probability with test index
      predict_v<-predict(model,df[test_index,],type="response")
      
      }
    
    # If model_type is random forest, train random forest model with probability as result
    else if (tolower(model_type) %in% c("random forest","rf")) {
      
      # Train random forest model with train index
      rf_model<-randomForest(as.formula(paste(target_col, "~ .")), data=df[train_index,], ntree=500, mtry=floor(sqrt(ncol(df))), importance=TRUE)
    
      # get prediction probability with test index
      predict_v<-predict(rf_model,df[test_index,],type="prob")[,as.character(positive)]
      
    }
    
    
    # If model_type is C5.0, train C5.0 model with probability as result
    else if (tolower(model_type) %in% c("c5.0","c5","c50")) {

      # train C5.0 model with train index
      feature<-setdiff(names(df),target_col)
      c5_m<-C5.0(x=df[train_index, feature],y=df[train_index, target_col],trials=10)
      
      # get prediction probability with test index
      predict_v<-predict(c5_m, df[test_index,], type="prob")[,as.character(positive)]
      
    }
    
    # If model_type is rpart, train rpart model with probability as result
    else if (tolower(model_type) %in% c("rpart","decision tree")) {

      # train rpart model with train index
      tree_m<-rpart(as.formula(paste(target_col, "~ .")), data=df[train_index,],method="class")
      
      # get prediction probability with test index
      predict_v<-predict(tree_m, df[test_index,], type="prob")[,as.character(positive)]
      
    }
    
    # If model does not belong to any model above, stop and send suggestion
    else {
      
      stop("Only support logistic regression, random forest, c5.0, decision tree")
      
    }
    
    # output result so user can follow & not confused
    cat("Now is runnung fold",i,"\n")
    # get performance's value
    result<-check_model_performance(predict_v,threshold, positive, negative, df, test_index, target_col)
    accuracy<-result$accuracy
    tpr<-result$tpr
    tnr<-result$tnr
    F1<-result$F1
    
    # calculate overall performance's value by adding up for future average calculation
    F1_total<-F1_total+F1
    accuracy_total<-accuracy_total+accuracy
    tpr_total<-tpr_total+tpr
    tnr_total<-tnr_total+tnr
  }
  
  # get overall performance's value by calculate the mean
  F1_overall<-round(F1_total/length(index_list),2)
  accuracy_overall<-round(accuracy_total/length(index_list),2)
  tpr_overall<-round(tpr_total/length(index_list),2)
  tnr_overall<-round(tnr_total/length(index_list),2)
  
  # and output result
  cat("Overall the cross validation of this model's performance's accuracy is ",accuracy_overall,"%, true positive rate is",tpr_overall,"%, true negative rate is", tnr_overall,"% and F1 is",F1_overall,".\n")
  return(list(
    accuracy_overall=accuracy_overall,
    tpr_overall=tpr_overall,
    tnr_overall=tnr_overall,
    F1_overall=F1_overall
  ))
}
