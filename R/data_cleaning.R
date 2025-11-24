#' Check NA and zero values in numeric columns
#'
#' @param df Data frame to check
#' @return Printed messages
#' @export
check_na_zero <- function(df){

  total_count <- 0

  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]])){

      na_count <- sum(is.na(df[[i]]))
      total_count <- na_count + total_count
      if (na_count > 0) {
        cat("There are total", na_count ,"NA value in column", colnames(df)[i],"\n")
      }

      zero_count <- length(which(df[[i]] == 0))
      total_count <- zero_count + total_count
      if (zero_count > 0) {
        cat("There are total", zero_count, "Zero value in column", colnames(df)[i], "\n")
      }
    }
  }

  if (total_count == 0) {
    cat("This data set does not have any zero value or NA value for its numerical features!\n")
  }
}

#' Replace zeros with NA except in specified columns
#'
#' @param df Data frame
#' @param ignore_cols Character vector of column names whose zeros should NOT be replaced
#' @return Data frame with zeros replaced by NA in selected numeric columns
#' @export
replace_na_with_zero <- function(df, ignore_cols) {

  for (i in 1:ncol(df)) {
    if (is.numeric(df[[i]]) && !(colnames(df)[i] %in% ignore_cols)){

      zero_count <- length(which(df[[i]] == 0))
      if (zero_count > 0) {
        cat("Total", zero_count, "Zero value in column", colnames(df)[i],
            "has successfully changed into NA","\n")
        df[[i]][df[[i]] == 0] <- NA
      }
    }
  }
  return(df)
}

#' Median imputation for selected columns
#'
#' @param df Data frame
#' @param target_col Character vector of column names to impute
#' @return Data frame with NA imputed by median
#' @export
median_imputation <- function(df, target_col) {

  for (i in target_col) {
    m_value <- median(df[[i]], na.rm = TRUE)
    df[[i]][is.na(df[[i]])] <- m_value
    cat("Feature", i, "is successfully imputated with median \n")
  }

  return(df)
}

#' Z-score based outlier treatment (±3 SD)
#'
#' @param df Data frame
#' @param variable Character vector of numeric variable names
#' @return Data frame with extreme values capped at mean ± 3*sd
#' @export
z_score_outlier <- function(df, variable) {

  for (i in variable) {

    mean_v <- mean(df[[i]], na.rm = TRUE)
    sd_v   <- sd(df[[i]], na.rm = TRUE)
    z_v    <- (df[[i]] - mean_v) / sd_v

    up_b    <- mean_v + 3 * sd_v
    lower_b <- mean_v - 3 * sd_v

    outlier <- sum(z_v > 3, na.rm = TRUE) + sum(z_v < -3, na.rm = TRUE)

    df[[i]][z_v > 3]  <- up_b
    df[[i]][z_v < -3] <- lower_b

    cat("There are total", outlier,
        "outliers detected for feature", i,
        ". Above upper boundary ", round(up_b,2),
        "is replaced into ", round(up_b,2),
        ". Below lower boundary", round(lower_b,2),
        "is replaced into", round(lower_b,2), "\n")
  }

  return(df)
}

#' IQR-based outlier treatment with capping at 5% / 95% quantiles
#'
#' @param df Data frame
#' @param variables Character vector of numeric variable names
#' @return Data frame with outliers capped
#' @export
IQR_outlier <- function(df, variables){
  for (i in variables) {

    Q1  <- quantile(df[[i]], 0.25, na.rm = TRUE)
    Q3  <- quantile(df[[i]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1

    low_q <- Q1 - 1.5 * IQR
    up_q  <- Q3 + 1.5 * IQR

    outlier <- sum(df[[i]] < low_q, na.rm = TRUE) +
      sum(df[[i]] > up_q, na.rm = TRUE)

    caps <- quantile(df[[i]], probs = c(0.05, 0.95), na.rm = TRUE)

    df[[i]][df[[i]] < low_q] <- caps[1]
    df[[i]][df[[i]] > up_q]  <- caps[2]

    cat("There are total", outlier,
        "outliers detected for feature", i,
        ". Above upper quartile ", up_q,
        "is replaced into ", caps[2],
        ". Below lower quartile", low_q,
        "is replaced into", caps[1], "\n")
  }
  return(df)
}

#' Check multicollinearity via correlation matrix
#'
#' @param variables_df Data frame of numeric variables
#' @param threshold Correlation threshold to flag strong correlation
#' @return Printed messages of strongly correlated pairs
#' @export
check_multicollinearity <- function(variables_df, threshold = 0.8) {

  cor_df <- as.data.frame(as.table(cor(variables_df, use = "complete.obs")))

  remove_rows <- c()

  for (i in 1:nrow(cor_df)) {
    for (j in 1:nrow(cor_df)) {
      if (as.character(cor_df[i,1]) == as.character(cor_df[j,2]) &&
          as.character(cor_df[i,2]) == as.character(cor_df[j,1]) &&
          i < j) {
        remove_rows <- c(remove_rows, j)
      }
    }
  }

  cor_df <- cor_df[-remove_rows, ]

  total_count <- 0

  for (i in 1:nrow(cor_df)) {
    if (abs(cor_df[i,3]) >= threshold & cor_df[i,1] != cor_df[i,2]) {
      cat("There is a strong correlational relationship (",
          round(as.numeric(cor_df[i,3]),2),") occur betewen",
          as.character(cor_df[i,1]),"and",as.character(cor_df[i,2]),".\n")
      total_count <- total_count + 1
    }
  }
  if (total_count == 0) {
    cat("There is no multicolineariality detect in this data set!")
  }
}
