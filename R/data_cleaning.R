#' Check NA and zero values in numeric columns
#'
#' @param df Data frame to check
#' @return Zero Count
#' @export
check_na_zero<-function(df){
  
  # Initialize for total number of zero/na value
  total_count<-0
  
    # Ignore non-numerical column
    for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])){
    
      # Count NA, if appear, show which column has NA
      na_count <-sum(is.na(df[[i]]))
      # Add up add NA/zero value incase no zero/NA appear
      total_count<-na_count+total_count
      if (na_count>0) {
        cat("There are total", na_count ,"NA value in column", colnames(df)[i],"\n")
        }
    
      # Count zero value, if appear, show which column has zero value
      zero_count<-length(which(df[[i]] == 0))
      # Add up add NA/zero value incase no zero/NA appear
      total_count<-zero_count+total_count
      if (zero_count>0) {
        cat("There are total", zero_count, "Zero value in column", colnames(df)[i], "\n")
        }
      }
    }
  
  # if no na/zero value detect, output this information
  if (total_count==0) {
    cat("This data set does not have any zero value or NA value for its numerical features!\n")
  }
}

#' Replace zeros with NA except in specified columns
#'
#' @param df Data frame with zero value 
#' @param ignore_cols Zero value that does not need to be changed into NA
#' @return Data frame with designated columns' zero value replaced into na
#' @export
replace_zero_with_na<-function(df,ignore_cols) {
  
  # For only column that their zero value need to change into NA
  for (i in 1:ncol(df)) {
  if (is.numeric(df[[i]]) && !(colnames(df)[i] %in% ignore_cols)){
     
    # If their zero value is above 0, return result & change them into NA
    zero_count<-length(which(df[[i]] == 0))
    if (zero_count>0) {
      cat("Total", zero_count, "Zero value in column", colnames(df)[i],"has successfully changed into NA","\n")
      df[[i]][df[[i]]==0]<-NA
      }
    }
  }
 return(df) 
}

#' Median imputation for selected columns
#'
#' @param df The data frame that has NA need median imputation treatment
#' @param target_col Columns name that need its NA be treated with median imputation
#' @return Data frame with designated column successfully impute
#' @export
median_imputation<-function(df,target_col) {
  
  # loop over each column and perform median imputation
  for (i in target_col) {
    m_value<-median(df[[i]], na.rm=TRUE)
    df[[i]][is.na(df[[i]])]<-m_value
      
    cat("Feature",i,"is successfully imputated with median \n")
  }
  
  # return data frame
  return(df)
}

#' Z-score based outlier treatment (±3 SD)
#'
#' @param df The data frame that has feature need outlier treatment 
#' @param variable Variables that need outlier treatment & normally
#' @return Data frame with extreme values capped at mean ± 3*sd
#' @export
z_score_outlier<-function(df,variable) {
  
  # for each variable 
  for (i in variable) {
    
    # calculate their mean, standard deviation and z-score
    mean_v<-mean(df[[i]],na.rm = TRUE)
    sd_v<-sd(df[[i]],na.rm = TRUE)
    z_v<-(df[[i]]-mean_v)/sd_v
    
    # calculate upper boundary & lower boundary
    up_b<-mean_v+3*sd_v
    lower_b<-mean_v-3*sd_v
    
    # Get how many outliers detected for output result
    outlier<-sum(z_v > 3,na.rm = TRUE)+sum(z_v < -3,na.rm = TRUE)
    
    # Replace upper outliers with upper boundary, lower outliers with lower boundary
    df[[i]][z_v > 3]<-up_b
    df[[i]][z_v < -3]<-lower_b
    
    # Output result
    cat("Z-Score: There are total",outlier, "outliers detected for feature", i, ". Above upper boundary ", round(up_b,2), "is replaced into ", round(up_b,2), ". Below lower boundary", round(lower_b,2), "is replaced into", round(lower_b,2), "\n")
  }
  
  # return data frame
  return(df)
}

#' IQR-based outlier treatment with capping at 5% / 95% quantiles
#'
#' @param df Data frame with Outliers
#' @param variables Features in Data Frame that need outlier Treatment
#' @return Return data frame with variables' outliers treated
#' @export
IQR_outlier<-function(df,variables){
  for (i in variables) {
  
  # Identify first quartile, third quartile and IQR
  Q1<-quantile(df[[i]],0.25)
  Q3<-quantile(df[[i]],0.75)
  IQR<-Q3-Q1
  
  # Identify lower quartile and upper quartile
  low_q<-Q1-1.5*IQR
  up_q<-Q3+1.5*IQR
  
  # Collect outliers information
  outlier<-sum(df[[i]]<low_q) + sum(df[[i]]>up_q)

  # Get 5% value and 95% value for replacement
  caps<-quantile(df[[i]],probs=c(0.05,0.95))
  
  # For below lower quartile replaced by 5%
  df[[i]][df[[i]]<low_q]<-caps[1]
  # For above upper quatile repplaced by 95%
  df[[i]][df[[i]]>up_q]<-caps[2]
  
  cat("IQR: There are total",outlier, "outliers detected for feature", i, ". Above upper quartile ", up_q, "is replaced into ", caps[2], ". Below lower quartile", low_q, "is replaced into", caps[1], "\n")
  }
  return(df)
}

#' Check multicollinearity via correlation matrix
#'
#' @param variables_df Variables selected for multicollinearity checking (has to be numerical)
#' @param threshold Above what correlation value is considered as strong correlation
#' @return Nothing will return but will tell you what features are highly correlated
#' @export
check_multicollinearity<-function(variables_df, threshold=0.8) {
  
  # create a data frame of correlational relationship between these variables
  cor_df<-as.data.frame(as.table(cor(variables_df,use="complete.obs")))

  # Remove Duplicated Rows
  remove_rows<-c()
  
  # Loop over all row 1 and row 2 (variable name), 
  # row 1 variable is equal to row 2 variable & row 2 variable is also equal 
  # to row 1 variable & this happen the first time, collect in remove rows
  for (i in 1:nrow(cor_df)) {
    for (j in 1:nrow(cor_df)) {
      if (as.character(cor_df[i,1])==as.character(cor_df[j,2]) && as.character(cor_df[i,2]) == as.character(cor_df[j,1]) && i<j) {
        remove_rows<-c(remove_rows,j)
      }
    }
  }
  
  # and remove them
  cor_df<-cor_df[-remove_rows, ]
  
  total_count<-0
  
  # Display Result
  for (i in 1:nrow(cor_df)) {
    
    # If greater than threshold and is not happened between self & self
    # Threshold set as 0.8, you can change this feature
    # Return the result
    if (abs(cor_df[i,3])>=threshold & cor_df[i,1] != cor_df[i,2]) {
      cat("There is a strong correlational relationship (", round(as.numeric(cor_df[i,3]),2),") occur betewen",as.character(cor_df[i,1]),"and",as.character(cor_df[i,2]),".\n")
      total_count<-total_count+1
    }
  }
  if (total_count==0) {
    cat("There is no multicolineariality detect in this data set!")
  }
}

#' Initialize distance matrix for kNN imputation
#'
#' @param df Data frame that need to be treated
#' @return The euclidean distance list that will be used
#' @export
initialize_distance_find_best_k<-function(df){
  
  # Find all numerical column
  numerical_columns<-names(df)[sapply(df,is.numeric)]
  # Find all numerical col with NA
  excluded_columns<-names(df)[colSums(is.na(df))>0]
  # Exlude column with NA
  target_columns<-setdiff(numerical_columns,excluded_columns)
  # Normalization & calculate euclidean distance
  df_matrix<-as.matrix(df[,target_columns])
  df_matrix<-scale(df[, target_columns])
  df_distance<-as.matrix(dist(df_matrix, method="euclidean"))
  return(df_distance)
}

#' Get indices of non-NA values in a feature
#'
#' @param target_feature Specific feature that need to find not na value
#' @return Target_feature's all index that are not na
#' @export
initialize_not_na_index<-function(target_feature){
    
    # Find index that are not na
    not_na_index<-which(!is.na(target_feature))
    return(not_na_index)
}

#' Sample indices to tune k for kNN imputation
#'
#' @param test_proportion Proportion you want to select from not_na_index to test best k
#' @param not_na_index The not_na_index you get from previous function
#' @return The index you wanna use to test best k
#' @export
initialize_test_k_index<-function(test_proportion,not_na_index) {
  
  # From index does not have na, randomly select certain portion of index 
  test_k_index<-sample(not_na_index,size=floor(test_proportion*length(not_na_index)))
  return(test_k_index)
}

#' Find best k for kNN imputation based on RMSE
#'
#' @param max_k Number of k you wanna test
#' @param test_k_index The index you wanna use to test best k
#' @param df_distance The euclidean distance matrix build before
#' @param not_na_index The not_na_index you get from previous function
#' @param target_feature The feature need to find best k
#' @return Smallest_k used for kNN imputation
#' @export
find_best_k<-function(max_k,test_k_index,df_distance,not_na_index,target_feature){
  
  # Initialize
  smallest_RMSE<-Inf
  smallest_k<-Inf
  
  
  # We gonna test k from 1 to 20
  for (k in 1:max_k) {
  
    # Initialization for total standard error
    se_total<-0

    # loop over all test k index that randomly selected earlier
    for (j in test_k_index){
      
      # Initialize
      df_temp<-df_distance
      # Set self to Inf so it won't be considered in smallest euclidean distance 
      df_temp[j,j]<-Inf
    
      # Get all distance of this specific index
      total_distance<-df_temp[j,not_na_index]
    
      # Ordered them the distance
      ordered_dist<-order(total_distance)
    
      # Choose the closest k's distance and get their original index in original data set
      original_index<-not_na_index[ordered_dist][1:k]
    
      # and calculate closest k's distance's mean value
      # This is our prediction value
      test_value<-round(mean(target_feature[original_index]))
    
      # Get actual value
      actual_value<-target_feature[j]
    
      # Add up the standard error for later RMSE calculation 
      se_total<-se_total+(actual_value-test_value)^2
    }
  
    # Get RMSE value for prediction's accuracy
    RMSE<-sqrt(se_total/length(test_k_index))
  
    # If this k's RMSE is smaller than any previous RMSE
    if (RMSE < smallest_RMSE){
    
      # update RMSE
      smallest_RMSE<-RMSE
    
      # update k
      smallest_k<-k
    }
  }
  # print out the best k we find and have its RMSE as additional value
  cat("The best k is", smallest_k,"which give smallest error of", smallest_RMSE,"\n")
  
  # remove temporary vector
  rm(df_temp,total_distance)
  
  # clear vector storage
  gc()
  
  return(smallest_k)
}

#' Perform kNN imputation for a single feature
#'
#' @param df Data frame that need NA kNN imputation treatment
#' @param smallest_k The best k calculate before
#' @param target_feature Specific column that need NA kNN imputation
#' @param df_distance The euclidean distance matrix made before
#' @return Feature value that successfully imputated
#' @export
kNN_Imputation<-function(df,smallest_k,target_feature,df_distance){
  # Initialization
  k<-smallest_k
  na_cols<-which(is.na(target_feature))
  na_rows<-which(is.na(target_feature))
  df_temp<-df_distance
  
  # For all values in target_feature
  for (i in na_rows) {
  
    # Ignore self
    df_temp[i,i]<-Inf
  
    # Ignore self's NA value
    df_temp[i,na_cols]<-Inf
  
    # Get smallest best k euclidean distance
    closest_k<-order(df_temp[i,])[1:k]
  
    # Impute the mean value of these k numbers
    target_feature[i]<-round(mean(target_feature[closest_k]))
  }
  return(target_feature)
}

#' Automation kNN imputation for all columns with NA
#'
#' @param df The data frame that has NA need kNN imputation treatment
#' @param test_por Proportion of the dataset used for k testing (default = 10%)
#' @param max_k Maximum number of k test for best k (default=20)
#' @param ignore_col Columns name that does not need its NA be treated (default=NULL)
#' @return Data frame with designated column successfully impute
#' @export
automation_knn_imputation<-function(df,test_por=0.1,max_k=20,ignore_col=NULL) {
  
  # Collect all column name with NA
  na_col_name<-names(df)[colSums(is.na(df))>0]
  
  # If there are columns want to be ignored, remove them from column name
  if (!is.null(ignore_col)) {
    na_col_name<-setdiff(na_col_name,ignore_col)
  }
  
  # For all column's that their NA need to be impute, call functions sequentially 
  # This function will tell you which column it is working on & how it plans to work on
  for (i in na_col_name) {
    target_feature<-df[[i]]
    df_distance<-initialize_distance_find_best_k(df)
    not_na_index<-initialize_not_na_index(target_feature)
    test_k_index<-initialize_test_k_index(test_por,not_na_index)
    cat("Now is processing column",i,"\n")
    smallest_k<-find_best_k(max_k,test_k_index,df_distance,not_na_index,target_feature)
    df[[i]]<-kNN_Imputation(df,smallest_k,target_feature,df_distance)
    cat("Feature",i,"is successfully imputed with kNN. \n")
    
    # Remove unncessary information to release storage
    rm(df_distance)
    gc()
  }
  return(df)
}
