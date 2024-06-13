library(xgboost)
library(caret)
install.packages('MLmetrics')
library(MLmetrics)

X_train_lda <- read.csv('X_train_lda.csv')
X_test_lda <- read.csv('X_test_lda.csv')
y_train <- read.csv('y_train.csv')
y_test <- read.csv('y_test.csv')


y_train <- y_train$x -1 #rain: yes<-1, no<-0
y_test <- y_test$x -1


y_train <- factor(y_train, levels = c(0,1))
y_test <- factor(y_test, levels = c(0,1))



xgb_grid <- expand.grid(
  nrounds = c( 100),
  eta = c( 0.1, 0.3),
  max_depth = c(3, 6, 9),
  gamma = c(0, 1),
  colsample_bytree = c( 0.7),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.5, 0.7, 1)
)

set.seed(123)
train_control <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = multiClassSummary,
  classProbs = FALSE,
  verboseIter = TRUE
)

xgb_train <- train(
  x = as.matrix(X_train_lda),
  y = y_train,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  metric = "Accuracy"
)

best_model <- xgb_train$finalModel

preds <- predict(best_model, as.matrix(X_test_lda), type = "response")
pred_labels <- ifelse(preds > 0.7, 0, 1)

accuracy <- Accuracy(pred_labels, y_test)
f1_score <- F1_Score(pred_labels, y_test)
recall <- Recall(pred_labels, y_test)

cat("Test Accuracy: ", accuracy, "\n")
cat("Test F1 Score: ", f1_score, "\n")
cat("Test Recall: ", recall, "\n")

best_model$params


