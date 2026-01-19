This package is designed to support **classification & regression modeling pipelines**

## ✨ Install from GitHub

install.packages("devtools")

devtools::install_github("xxxxxi0001/xxml")

## ✨ Features

### One Step Data Cleaning
- `automate_data_cleaning` — One step outlier & NA treatment based on distribution & portion of NA

### Data Cleaning Utilities
- `check_na_zero()` — Detect NA / Zero in all numeric variables  
- `replace_zero_with_na()` — Convert designated zero values to NA  
- `median_imputation()` — Impute NA using median  
- `z_score_outlier()` — Replace outliers using z-score capping  
- `IQR_outlier()` — IQR capping for skewed variables  
- `check_multicollinearity()` — Detect high correlations
- `automation_knn_imputation()` — one-step knn imputation
  
  * `initialize_distance_find_best_k()`
  * `initialize_not_na_index()`
  * `initialize_test_k_index()`
  * `find_best_k()`
  * `kNN_Imputation()`

## (Categorical: Support rf, c5, rpart, lr)

### Data Splitting 
- `three_set_partition()` — stratified train/test/validation split  
- `ensemble_train_partition()` — partitions for bagging

### Class Imbalance
- `check_class_imbalance()` - check if class imbalance occur

### Cross Validation
- `k_fold_stratified()` - stratified train index into k fold for pipeline testing
- `cross_validation()` - cross validation support rf, c5, rpart, lr

### Logistic Regression (Backward p-selection)
- `backward_p_lr()` — iterative removal of non-significant predictors

### Ensemble Learning
- `make_ensemble_predict_categorical()` — mean probability ensemble  
- `find_best_threshold()` — threshold tuning for F1  
- `ensemble_weight_F1()` — model weights from F1 performance  
- `emsemble_result_with_weight_categorical()` — weighted ensemble prediction

### Check Modle Performance
- `check_model_performance()` - TPR, TNR, F1, Accuracy

### Stack (categorical)
- `generate_stack_df()` - helper func
- `stack_model_lr()` - generate new model with exsiting model with lr
- `stack_model_predict_lr()` - use stack model make prediction

## (Regression)

### Data Splitting 
- `three_set_partition_no_target()` — randomly partitioned index into designated portion
- `ensemble_train_partition_no_target()` — randomly partitioned train index into designated portion

### Multi-Linear Regression (Backward p-selection)
- `backward_p_mlr()` - iterative removal of non-significant predictors

### Ensemble Learning
- `reverse_num()` - if feature changed in feature transformation, change it back
- `make_ensemble_predict_numerical()` - use ensemble model make mean prediction
- `ensemble_weight_with_RMSE()` - calculate each model's weight based on RMSE
- `ensemble_result_with_weight_numerical()` - final prediction with weight

### Stack (Regression)
- `create_stack_model_mlr()` - generate a new multi-linear regression model based on existing model
- `stack_test_mlr()` - check how stack model perform

---

## ❤️ Usage Example (Categorical)

library(xxml)

### Load your data
df <- read.csv("your_data.csv")

### Basic NA / zero check
check_na_zero(df)

### Convert zero to NA for Cholesterol
df <- replace_zero_with_na(df, ignore_cols = "Oldpeak")

### One-Step Clean Data with Outlier & NA
df <- automate_data_cleaning(df)

### Feature Engineering (Derive New Feature, Transformation, Multicolineariality, PCA)
df_LR$doubleRISK<-df_LR$MaxHR * df_LR$Oldpeak


df_LR$Age_square<-(df_LR$Age)^2
hist(df_LR$Age_square,prob=TRUE, main="Histogram of Age After", xlab="Age")
lines(density(df_LR$Age_square, na.rm=TRUE))

variables<-df_LR[,c("Age_square","RestingBP","Cholesterol","MaxHR","Oldpeak_square","doubleRISK_sqrt")]
check_multicollinearity(variables)

pca_results<-prcomp(df_LR[,c("Age_square","RestingBP","Cholesterol","MaxHR","Oldpeak_square","doubleRISK_sqrt")])
summary(pca_results)
pca_df<-as.data.frame(pca_results$x[,1:5])

### Partition dataset (50% train, 25% test, 25% val)
index <- three_set_partition(df, "HeartDisease", 0.5, 0.25, 1, 0)

train_index <- index$train_index

test_index  <- index$test_index

val_index   <- index$validation_index

### Fit logistic regression backward p
log_models <- backward_p_lr(df, train_index, "HeartDisease", 1, 1, 1)

### Ensemble prediction
pred <- make_ensemble_predict(log_models, df, test_index, positive = 1)

### Evaluate
check_model_performance(pred, 0.5, 1, 0, df, test_index, "HeartDisease")

### Tune with Best Threshold & Weight
best_threshold<-find_best_threshold(ensemble_predictions,df_encoded_LR,test_index,"HeartDisease",1,0)

weight_list<-ensemble_weight_F1(logistic_model_list,df_encoded_LR,test_index,best_threshold,"HeartDisease",1,0)

ensemble_predictions_val<-ensemble_result_with_weight(logistic_model_list,df_encoded_LR,validation_index,weight_list,1)

result_ensemble<-check_model_performance(ensemble_predictions_val,best_threshold,1,0,df_encoded_LR,validation_index,"HeartDisease")

## ❤️ Usage Example (Regression)

library(xxml)

### Load your data
df <- read.csv("your_data.csv")

### Basic NA / zero check
check_na_zero(df)

### Convert zero to NA for Cholesterol
df <- replace_zero_with_na(df, ignore_cols = "Oldpeak")

### One-Step Clean Data with Outlier & NA
df <- automate_data_cleaning(df)

### Feature Engineering (Transformation, Multicollinearity, Normalization, One-Hot)
df$DONATION_AMT_log<-log(df$DONATION_AMT)
hist(df$DONATION_AMT_log,prob=TRUE, main="Histogram of Donation Amount After", xlab="Donor Age")
lines(density(df$DONATION_AMT_log, na.rm=TRUE))

variables<-df[,c("DONATION_AMT_log","DONOR_AGE","INCOME_LEVEL","SES","MEDIAN_HOME_VALUE_log","MEDIAN_HOUSEHOLD_INCOME_sqrt","DONATION_RESPONSE_sqrt1","MONTHS_SINCE_LAST_GIFT_square","EMAILS_12_log","LIFETIME_GIFT_COUNT_sqrt","LIFETIME_EMAILS_sqrt","LIFETIME_GIFT_AMOUNT_sqrt","LIFETIME_MAX_GIFT_AMT_sqrt","LIFETIME_MIN_GIFT_AMT_sqrt","LIFETIME_AVG_GIFT_AMT_sqrt")]
check_multicollinearity(variables)

df_scaled[col_numeric]<-scale(df[col_numeric==TRUE])

onehot_features<-model.matrix(~URBANICITY+DONOR_GENDER+HOME_OWNER,data=df_scaled)
onehot_df<-as.data.frame(onehot_features)[,-1]

### Partition (50,25,25)
set.seed(888)
partition_result<-three_set_partition_no_target(df_encoded,0.5,0.25)
train_index<-partition_result$train_index
test_index<-partition_result$test_index
validation_index<-partition_result$validation_index

### Construction with Backward p
mlr_list<-backward_p_mlr(df_encoded,train_partition_index,"DONATION_AMT_log")

### Tune with Weight
weight_list<-ensemble_weight_RMSE(mlr_list,df_encoded,test_index,"DONATION_AMT_log","log")

prediction<-emsemble_result_with_weight(mlr_list,df_encoded,validation_index,weight_list,"log")

### Check Result with RMSE & Correlation & Plot
real_value<-exp(df_encoded[["DONATION_AMT_log"]][validation_index])
rmse<-sqrt(mean((real_value-prediction)^2))

correlation<-cor(prediction,real_value)

plot(prediction,real_value)
abline(0,1, col=2)

### Stack
stack_model<-create_stack_model_mlr(df_encoded,"DONATION_AMT_log",test_index,mlr_list)
result<-stack_test_mlr(stack_model,mlr_list,df_encoded,validation_index,"DONATION_AMT_log","log")

### Check Stack Result (Correlation, plot)
prediction_value<-result$prediction_value
true_value<-result$true_value

correlation<-cor(prediction_value,true_value)

plot(prediction_value,true_value)
abline(0,1, col=2)
