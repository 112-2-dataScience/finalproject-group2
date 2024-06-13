library(tidyverse)
library(caret)
library(ggplot2)
library(readr)
library(randomForest)
library(dplyr)

x_train <- read.csv("Data/X_train_lda.csv")
x_test <-read.csv("Data/X_test_lda.csv")
y_train <- read.csv("Data/y_train.csv")
y_test <- read.csv("Data/y_test.csv")

y_train <- y_train$x
y_train <- as.factor(y_train)
y_test <- y_test$x

#print(sum(is.na(x_train)))
#print(sum(is.na(y_train)))
#print(sum(is.na(x_test)))
#print(sum(is.na(y_test)))

trees <- seq(1, 200)
mtry <- seq(1, 50, by=10)

err <- numeric(length(trees))
acc <- numeric(length(trees))
results <- list()

set.seed(123)
for (tree in trees) {
  
  for (try in mtry) {
    
    model <- randomForest(x_train, y_train, ntree=tree, mtry=try, proximity=TRUE)
    
    # 計算錯誤率
    err_rate <- model$err.rate[length(model$err.rate)]
    #err[tree] <- model$err.rate[length(model$err.rate)]
    
    # 計算 accuracy
    predictions <- predict(model, x_test)
    acc[try] <- mean(predictions == y_test)
    
    results[[length(results) + 1]] <- data.frame(tree=tree, try=try, accuracy=acc[try])

      }
  
}

results_df <- bind_rows(results)
# min_oob <- min(results_df$error_rate)
max_acc <- max(results_df$accuracy)

# best tree = 8, best mtry = 31
final_model <- randomForest(x_train, y_train, ntree=8, mtry = 31)
prediction <- predict(final_model, x_test, type = "prob") 

# 1: rain, 2: not rain, threshold = 0.3 0.4 0.5
# If prob of 2(rain) > threshold, set as 2
pred_class <- ifelse(prediction[, 2] >= 0.5, 2, 1)

predictions <- as.data.frame(pred_class)
colnames(predictions) <- c("Predicted class")

y_test <- as.factor(y_test)
predictions <- factor(pred_class, levels = levels(y_test))

accuracy <- mean(predictions == y_test)

cm <- confusionMatrix(predictions, y_test)
recall <- cm$byClass["Sensitivity"]

ggplot(results_df, aes(x=tree, y=accuracy, group=factor(try), color=factor(try))) +
  geom_line() +
  labs(title="Random Forest Accuracy by Number of Trees and Mtry",
       x="Number of Trees",
       y="Accuracy",
       color="Mtry Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
