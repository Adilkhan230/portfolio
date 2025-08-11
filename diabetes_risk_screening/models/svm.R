library(caret)
library(pROC)
library(ROCR)
library(tidyverse)

# Загрузка и подготовка данных
df <- read.csv("df_selected.csv")
df <- df %>% select(-c("BMXWT", "BMXWAIST", "MCQ160D_Yes", "X"))

# Перекодируем target в фактор с корректными уровнями для caret
# Предполагаем, что исходно target = 0/1
df$target <- factor(df$target, levels = c(0, 1), labels = c("No", "Yes"))

set.seed(123)

# Разбиение на обучающую и тестовую выборки
trainIndex <- createDataPartition(df$target, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Обучение модели SVM с радиальным ядром
svm_model <- train(
  target ~ ., 
  data = train_data,
  method = "svmRadial",
  preProcess = c("center", "scale"),
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  metric = "ROC"
)

# Получение вероятностей для тестовой выборки
probs_df <- predict(svm_model, test_data, type = "prob")
pred_probs <- probs_df[, "Yes"]  # вероятности положительного класса

# Предсказания классов (по умолчанию threshold = 0.5)
pred_classes <- predict(svm_model, test_data)

# Матрица ошибок и метрики
conf_matrix <- confusionMatrix(pred_classes, test_data$target)
print(conf_matrix)

f1_score <- conf_matrix$byClass["F1"]
accuracy <- conf_matrix$overall["Accuracy"]

cat("F1 Score:", round(f1_score, 4), "\n")
cat("Accuracy:", round(accuracy, 4), "\n")

# ROC, AUC, Gini
roc_obj <- roc(response = test_data$target, predictor = pred_probs, levels = rev(levels(test_data$target)))
auc_val <- auc(roc_obj)
gini_index <- 2 * auc_val - 1

cat("AUC:", round(auc_val, 4), "\n")
cat("Gini Index:", round(gini_index, 4), "\n")

# KS statistic
pred_rocr <- prediction(pred_probs, ifelse(test_data$target == "Yes", 1, 0))
perf_ks <- performance(pred_rocr, "tpr", "fpr")
ks_stat <- max(attr(perf_ks, "y.values")[[1]] - attr(perf_ks, "x.values")[[1]])
cat("Kolmogorov-Smirnov (KS) statistic:", round(ks_stat, 4), "\n")
#saveRDS(svm_model, "svm_model.rds")

