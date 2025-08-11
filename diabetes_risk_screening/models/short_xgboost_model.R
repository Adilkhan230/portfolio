library(caret)
library(xgboost)
library(pROC)
library(ROCR)
library(tidyverse)
library(tidymodels)
library(smotefamily)
library(SHAPforxgboost)

# Load dataset
df <- read.csv("df_selected.csv")
#Select first ten features(by SHAP) and target 
df <- df %>% select(c("RIDAGEYR","HUQ010","MCQ300C_Yes","BMXBMI","MCQ080_Yes","HUQ030","BPXSY1","SLQ050_Yes","BPXDI1","MCQ160A_Yes","target"))

set.seed(123)

# Split into train/test (80/20)
trainIndex <- createDataPartition(df$target, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Create model matrices for XGBoost
train_matrix <- model.matrix(target ~ . , data = train_data)[, -1]
test_matrix <- model.matrix(target ~ . , data = test_data)[, -1]

# Extract labels and convert to 0/1 numeric
train_labels <- as.numeric(train_data$target)
test_labels <- as.numeric(test_data$target)

# Safety check for matching rows
stopifnot(nrow(train_matrix) == length(train_labels))
stopifnot(nrow(test_matrix) == length(test_labels))

# Create DMatrix objects
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Define parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train model with early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)

# Predict on test set
predictions <- predict(xgb_model, dtest)

# Convert probabilities to binary classes (threshold 0.5)
pred_binary <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix and metrics
conf_matrix <- confusionMatrix(factor(pred_binary), factor(test_labels))

# Compute AUC
conf_matrix <- confusionMatrix(factor(pred_binary), factor(test_labels))
print(conf_matrix)

f1_score <- conf_matrix$byClass["F1"]
accuracy <- conf_matrix$overall["Accuracy"]

cat("F1 Score:", round(f1_score, 4), "\n")
cat("Accuracy:", round(accuracy, 4), "\n")

# AUC и Gini index
roc_obj <- roc(test_labels, predictions)
auc_val <- auc(roc_obj)
gini_index <- 2 * auc_val - 1

cat("AUC:", round(auc_val, 4), "\n")
cat("Gini Index:", round(gini_index, 4), "\n")

# Kolmogorov-Smirnov (KS) statistic
pred_rocr <- prediction(predictions, test_labels)
perf_ks <- performance(pred_rocr, "tpr", "fpr")

# KS = max difference между TPR и FPR
ks_stat <- max(attr(perf_ks, "y.values")[[1]] - attr(perf_ks, "x.values")[[1]])
cat("Kolmogorov-Smirnov (KS) statistic:", round(ks_stat, 4), "\n")
#xgb.save(xgb_model, "xgb_selection_model.model")

# Feature importance
importance <- xgb.importance(model = xgb_model)
print(importance)
xgb.plot.importance(importance, top_n = 20)
#saveRDS(xgb_model, "short_xgb_model.model")