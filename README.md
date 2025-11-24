An R package for **data cleaning**, **feature engineering**, **kNN imputation**,  
**logistic regression (with backward p selection)**,  
**random forest / C5.0 modeling**, and  
**ensemble learning with threshold & F1-based weights**.

This package is designed to support **classification modeling pipelines**,  

---

## ✨ Features

### 🔧 Data Cleaning Utilities
- `check_na_zero()` — Detect NA / Zero in all numeric variables  
- `replace_na_with_zero()` — Convert designated zero values to NA  
- `median_imputation()` — Impute NA using median  
- `z_score_outlier()` — Replace outliers using z-score capping  
- `IQR_outlier()` — IQR capping for skewed variables  
- `check_multicollinearity()` — Detect high correlations

### 🔍 Missing Value Imputation
Full k-Nearest Neighbors **custom implementation**, including:
- `initialize_distance_find_best_k()`
- `initialize_not_na_index()`
- `initialize_test_k_index()`
- `find_best_k()`
- `kNN_Imputation()`
- `automation_knn_imputation()` — one-step imputation automation

### 🧪 Data Splitting & Cross Validation
- `three_set_partition()` — stratified train/test/validation split  
- `ensemble_train_partition()` — partitions for bagging  
- `check_class_imbalance()` — check positive/negative distribution  
- `k_stratified_cv()` — k-fold stratified cross-validation  
- `cross_validation()` — full pipeline validation

### 📉 Logistic Regression (Backward p-selection)
- `backward_p_lr()` — iterative removal of non-significant predictors

### 🌲 Machine Learning Models
Support for:
- Logistic Regression  
- Random Forest  
- C5.0 Decision Tree  
- rpart Decision Tree

### 🤖 Ensemble Learning
- `make_ensemble_predict()` — mean probability ensemble  
- `find_best_threshold()` — threshold tuning for F1  
- `ensemble_weight_F1()` — model weights from F1 performance  
- `emsemble_result_with_weight()` — weighted ensemble prediction

---

## 📦 Installation

### Install from GitHub

install.packages("devtools")
devtools::install_github("xxxxxi0001/xxml")
