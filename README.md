This package is designed to support **classification & regression modeling pipelines**

## ✨ Install from GitHub

install.packages("devtools")

devtools::install_github("xxxxxi0001/xxml")

## ✨ Features

### Data Cleaning Utilities
- `check_na_zero()` — Detect NA / Zero in all numeric variables  
- `replace_na_with_zero()` — Convert designated zero values to NA  
- `median_imputation()` — Impute NA using median  
- `z_score_outlier()` — Replace outliers using z-score capping  
- `IQR_outlier()` — IQR capping for skewed variables  
- `check_multicollinearity()` — Detect high correlations

### Missing Value Imputation
Full k-Nearest Neighbors **custom implementation**, including:
- `initialize_distance_find_best_k()`
- `initialize_not_na_index()`
- `initialize_test_k_index()`
- `find_best_k()`
- `kNN_Imputation()`
- `automation_knn_imputation()` — one-step imputation automation

### Data Splitting & Cross Validation
- `three_set_partition()` — stratified train/test/validation split  
- `ensemble_train_partition()` — partitions for bagging  
- `check_class_imbalance()` — check positive/negative distribution  
- `k_stratified_cv()` — k-fold stratified cross-validation  
- `cross_validation()` — full pipeline validation

### Logistic Regression (Backward p-selection)
- `backward_p_lr()` — iterative removal of non-significant predictors

### Machine Learning Models
Support for:
- Logistic Regression  
- Random Forest  
- C5.0 Decision Tree  
- rpart Decision Tree

### Ensemble Learning
- `make_ensemble_predict()` — mean probability ensemble  
- `find_best_threshold()` — threshold tuning for F1  
- `ensemble_weight_F1()` — model weights from F1 performance  
- `emsemble_result_with_weight()` — weighted ensemble prediction

---

## Usage Example

library(xxml)

### Load your data
df <- read.csv("your_data.csv")

### Basic NA / zero check
check_na_zero(df)

### Convert zero to NA for Cholesterol
df <- replace_na_with_zero(df, ignore_cols = "Oldpeak")

### Median imputation for RestingBP
df <- median_imputation(df, "RestingBP")

### Automatic kNN imputation for all NA columns
df <- automation_knn_imputation(df)

### Outlier treatment
df <- z_score_outlier(df, c("Age", "RestingBP", "MaxHR"))

### Partition dataset (50% train, 25% test, 25% val)
parts <- three_set_partition(df, 888, "HeartDisease", 0.5, 0.25, 1, 0)

train_index <- parts$train_index

test_index  <- parts$test_index

val_index   <- parts$validation_index

### Fit logistic regression backward p
log_models <- backward_p_lr(df, train_index, "HeartDisease", 1, 1, 1)

### Ensemble prediction
pred <- make_ensemble_predict(log_models, df, test_index, positive = 1)

### Evaluate
check_model_performance(pred, 0.5, 1, 0, df, test_index, "HeartDisease")
