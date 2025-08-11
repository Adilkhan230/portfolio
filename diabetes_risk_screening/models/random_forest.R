library(caret)
library(randomForest)
library(pROC)
library(ROCR)
library(tidyverse)

# Load dataset
df <- read.csv("df_selected.csv")
df <- df %>% select(-c("BMXWT", "BMXWAIST", "MCQ160D_Yes"))

# Convert character columns to factors (except target if numeric)
df <- df %>% mutate(across(where(is.character), as.factor))

# Convert target to factor for classification
df$target <- factor(df$target)

set.seed(123)

# Split into train/test (80/20)
trainIndex <- createDataPartition(df$target, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Train Random Forest model
rf_model <- randomForest(target ~ . - X, data = train_data,
                         ntree = 500,   # number of trees
                         mtry = floor(sqrt(ncol(train_data) - 2)), # default for classification
                         importance = TRUE)

# Predict on test set (probabilities for class "1")
pred_prob <- predict(rf_model, test_data, type = "prob")[, 2]

# Convert probabilities to binary classes (threshold 0.5)
pred_binary <- ifelse(pred_prob > 0.5, 1, 0)

# Confusion matrix and metrics
conf_matrix <- confusionMatrix(factor(pred_binary), factor(as.numeric(test_data$target) - 1))
print(conf_matrix)

f1_score <- conf_matrix$byClass["F1"]
accuracy <- conf_matrix$overall["Accuracy"]

cat("F1 Score:", round(f1_score, 4), "\n")
cat("Accuracy:", round(accuracy, 4), "\n")

# Compute AUC and Gini index
roc_obj <- roc(as.numeric(test_data$target) - 1, pred_prob)
auc_val <- auc(roc_obj)
gini_index <- 2 * auc_val - 1

cat("AUC:", round(auc_val, 4), "\n")
cat("Gini Index:", round(gini_index, 4), "\n")

# Kolmogorov-Smirnov (KS) statistic
pred_rocr <- prediction(pred_prob, as.numeric(test_data$target) - 1)
perf_ks <- performance(pred_rocr, "tpr", "fpr")
ks_stat <- max(attr(perf_ks, "y.values")[[1]] - attr(perf_ks, "x.values")[[1]])
cat("Kolmogorov-Smirnov (KS) statistic:", round(ks_stat, 4), "\n")

# Feature importance and plot
importance_rf <- importance(rf_model)
print(importance_rf)
varImpPlot(rf_model, n.var = 20)
saveRDS(rf_model, "rf_model.rds")
