library(lightgbm)
library(caret)
library(pROC)
library(ROCR)
library(tidyverse)

# Load and prepare data
df <- read.csv("df_selected.csv")
df <- df %>% select(-c("BMXWT", "BMXWAIST", "MCQ160D_Yes", "X"))

# Convert character columns to factors and target to factor
df <- df %>% mutate(across(where(is.character), as.factor))
df$target <- as.factor(df$target)

set.seed(123)

# Train/test split (80/20)
trainIndex <- createDataPartition(df$target, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Convert factors to numeric codes for LightGBM
# LightGBM requires numeric matrix inputs; categorical variables encoded as integers starting at 0
encode_factors <- function(df){
  df[] <- lapply(df, function(x) {
    if(is.factor(x)) as.numeric(x) - 1 else x
  })
  return(df)
}

train_numeric <- encode_factors(train_data %>% select(-target))
test_numeric <- encode_factors(test_data %>% select(-target))

train_label <- as.numeric(as.character(train_data$target))
test_label <- as.numeric(as.character(test_data$target))

# Create LightGBM datasets
dtrain <- lgb.Dataset(data = as.matrix(train_numeric), label = train_label)
dtest <- lgb.Dataset(data = as.matrix(test_numeric), label = test_label)

# Parameters
params <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = 0.1,
  num_leaves = 31,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5
)

# Train LightGBM model with early stopping
lgb_model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  valids = list(test = dtest),
  early_stopping_rounds = 10,
  verbose = 1
)

# Predict on test set
predictions <- predict(lgb_model, as.matrix(test_numeric))

# Threshold = 0.5 to get classes
pred_binary <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix & metrics
conf_matrix <- confusionMatrix(factor(pred_binary), factor(test_label))

print(conf_matrix)

f1_score <- conf_matrix$byClass["F1"]
accuracy <- conf_matrix$overall["Accuracy"]

cat("F1 Score:", round(f1_score, 4), "\n")
cat("Accuracy:", round(accuracy, 4), "\n")

# AUC and Gini
roc_obj <- roc(test_label, predictions)
auc_val <- auc(roc_obj)
gini_index <- 2 * auc_val - 1

cat("AUC:", round(auc_val, 4), "\n")
cat("Gini Index:", round(gini_index, 4), "\n")

# KS statistic
pred_rocr <- prediction(predictions, test_label)
perf_ks <- performance(pred_rocr, "tpr", "fpr")
ks_stat <- max(attr(perf_ks, "y.values")[[1]] - attr(perf_ks, "x.values")[[1]])
cat("Kolmogorov-Smirnov (KS) statistic:", round(ks_stat, 4), "\n")

# Feature importance plot
importance <- lgb.importance(lgb_model, percentage = TRUE)
print(importance)
lgb.plot.importance(importance, top_n = 20)
#saveRDS(lgb_model, "light_model.rds")

