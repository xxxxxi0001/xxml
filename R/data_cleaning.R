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
replace_na_with_zero<-function(df,ignore_cols) {
  
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
