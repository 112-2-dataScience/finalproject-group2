# [Group02] 大台北區降雨數據分析及多模型預測效能比較
政大校園氣候潮濕且四季有雨，而氣象預報卻不總是準確，學生們飽受其累。因此，我們構想自行展開進行天氣預測的研究，期望透過比較找出最適合的模型，不僅提升天氣預報的準確性，更為校園生活帶來實質的便利。 

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|李承恩|資科大四|109703010|收集資料,資料前處理| 
|楊榮瑜|哲學碩四|1091540054|EDA,特徵工程|
|黃妍心|國貿大四|109301091|建模：Logistic Regression、海報製作|
|陳昊暄|風管大三|110207402|建模：XGBoost、期末報告|
|陳為政|教育大三|110102050|建模：RandomForest、期末報告|


## Folder organization and its related description
### docs
![images](DS第二組A1海報初版.png)
### presentation link:  
（https://www.canva.com/design/DAGGA6j6UgI/_jtcOQspIFz_kFdtnX5zzw/edit?utm_content=DAGGA6j6UgI&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton）

## data
- Source(https://codis.cwa.gov.tw/)
- Format:CSV
- Size:1869筆
- **資料介紹**
本次研究，我們從 CODiS 氣候資料服務系統抓取 2019/01 至 2024/04 的觀測資料。然而，我們未採用文山測站，而是選擇抓取位於中山區的台北測站資料，因其為署屬有人站，資料與前者相比豐富許多，更能滿足訓練之需求。我們採用前一天的資料如相對溼度、A 型蒸發量、雲量以預測當天的下雨情形，依降水量區分為無雨、小雨、中雨、大雨及暴雨，經遺失值清洗後共計為 1869 筆。
- **資料前處理**
- 如附錄的code


## EDA
- 本次研究的 EDA 過程如下：
1. **確認資料類型**：檢查數據集中各列的數據類型。
2. **確認標籤分布**：分析目標標籤的分布情況。
3. **將非數值資料轉為數值型態**：對類別數據進行編碼。
4. **確認偏度**：分析各數值特徵的偏度。
5. **標準化**：對數擓特徵進行標準化處理。
6. **確認數值範圍**：檢查特徵值的範圍。
7. **去除離群值**：識別並去除離群值。
8. **計算相關係數與P-value**：檢驗特徵間的相關性和顯著性。
9. **降維**：應用 LDA 與 PCA 技術進行特徵降維

    
## Code
訓練集、測試集：data folder 中的 lda_data
### 模型比較一：羅吉斯回歸
```R
# 載入必要的套件
library(caret)
library(MLmetrics)

# Part 1: 數據前處理 preprocess
# 讀取CSV檔並轉換標籤
preprocess <- function(data_path) {
  data <- read.csv(data_path)
  if (!"x" %in% names(data)) {
    stop("錯誤：CSV檔案中找不到 'x' 欄位。請檢查檔案欄位名稱。")
  }
  data$x <- factor(data$x - 1, levels = c(0, 1))
  data$x
}

# Part 2: 模型訓練與交叉驗證 train_cv
train_cv <- function(X, y) {
  train_control <- trainControl(method = "cv", number = 5)  # 使用5折交叉驗證
  #羅吉斯回歸模型（邏輯回歸）
  train(as.data.frame(X), y, method = "glm", family = binomial(link = 'logit'), trControl = train_control)
}

# Part 3: 模型預測
prediction <- function(model, new_data) {
  predict(model, newdata = as.data.frame(new_data), type = 'prob')[,2]  # 返回第二列，即陽性類別的概率
}

# Part 4: 性能評估
evaluation <- function(pred_probs, y_test, thresholds) {
  #pred_probs（模型預測的概率）#y_test（實際標籤) #thresholds（閾值）
  results <- data.frame(threshold = numeric(), accuracy = numeric(), recall = numeric())
  #各閾值下的指標計算
  for (threshold in thresholds) {
    pred_labels <- ifelse(pred_probs > threshold, 1, 0)
    accuracy <- Accuracy(pred_labels, as.numeric(as.character(y_test)))
    recall <- Recall(pred_labels, as.numeric(as.character(y_test)))
    results <- rbind(results, data.frame(threshold = threshold, accuracy = accuracy, recall = recall))
  }
  
  results
}

# 主程式
main <- function() {
  # 讀取訓練集和測試集特徵數據
  X_train_lda <- read.csv('/Users/huangyanxin/Downloads/1122datascience/final/dsfinal_data/X_train_lda.csv')
  X_test_lda <- read.csv('/Users/huangyanxin/Downloads/1122datascience/final/dsfinal_data/X_test_lda.csv')
  
  # 讀取並前處理訓練集和測試集標籤數據
  y_train <- preprocess('/Users/huangyanxin/Downloads/1122datascience/final/dsfinal_data/y_train.csv')
  y_test <- preprocess('/Users/huangyanxin/Downloads/1122datascience/final/dsfinal_data/y_test.csv')
  
  # 確認特徵數據和標籤數據的行數匹配
  if (nrow(X_train_lda) != length(y_train)) {
    stop("錯誤：訓練集特徵數據和標籤數據的行數不匹配。")
  }
  if (nrow(X_test_lda) != length(y_test)) {
    stop("錯誤：測試集特徵數據和標籤數據的行數不匹配。")
  }
  
  # 訓練模型並進行交叉驗證
  logit_model <- train_cv(X_train_lda, y_train)
  # 進行預測
  pred_probs <- prediction(logit_model, X_test_lda)
  
  # 定義不同的閾值
  thresholds <- c(0.5, 0.4, 0.3)
  
  # 計算並顯示性能指標
  metrics <- evaluation(pred_probs, y_test, thresholds)
  
  print(metrics)
}

# 執行主程式
main()
```

### 模型比較二：隨機森林

```R
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
```

### 模型比較三：XGBoost
```R
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
```

## results

### 一、模型性能比較
#### Logistic Regression 模型
| 閾值 | 準確率 | 召回率 |
|:----:|:------:|:------:|
| 0.5  | 0.691  | 0.727  |
| **0.4**  | **0.697**  | **0.800**  |
| 0.3  | 0.685  | 0.831  |

#### XGBoost 模型
| 閾值 | 準確率 | 召回率 |
|:----:|:------:|:------:|
| 0.5  | 0.671  | 0.705  |
| **0.4**  | **0.685**  | **0.814**  |
| 0.3  | 0.685  | 0.831  |

#### Random Forest 模型
| 閾值 | 準確率 | 召回率 |
|:----:|:------:|:------:|
| 0.5  | 0.644  | 0.704  |
| 0.4  | 0.665  | 0.733  |
| 0.3  | 0.641  | 0.631  |



#### 分析
- 在不同閾值下，模型性能有顯著差異。降低閾值通常會增加召回率但可能影響準確率。
- **XGBoost** 和 **Logistic Regression** 模型在閾值為0.4時，顯示較高的準確率和召回率，是此數據集上的最佳設定。
- 模型目標為達到最高的召回率，並最小化真實下雨但預測沒下雨（FN)的機率。可選擇 XGBoost 或 Logistic Regression，並將閾值設置為 0.3 或 0.4。

### 二、改進方向
**Threshold 調整**
- 為了提高降雨預測的可靠性並減少用戶未攜帶雨具的風險，我們將預測閾值從 0.5 調整到 0.4，此情況下被 label 成會下雨 的數量會增加（Predicted positive 增加）
- 這樣可以減少 FN 的發生，增強模型檢測降雨的能力。
- 同時，使用邏輯迴歸模型，因其簡單且穩定，更好地處理小數據集和噪音數據。

**提升預測性能、處理複雜性**
- 納入更多特徵：增加如雲量、風速變化、歷史降雨數據等特徵
- 提供更豐富的資訊，有助於模型更準確地捕捉降雨的模式和規律。

**延伸到多分類預測**：無雨、小雨、中雨、大雨及暴雨
提供更具體的天氣預報，進一步提高模型的應用價值。

## References
### 一、套件
本項目使用以下 R 語言套件進行數據分析和模型建構：
1. `readxl`：用於讀取Excel文件。
2. `corrplot`：生成變量間的相關性圖。
3. `ggplot2`：用於創建複雜的圖表。
4. `dplyr`：數據操縱工具，方便數據處理。
5. `caret`：機器學習的數據分割、前處理、後驗分析等。
6. `skimr`：提供快速而有用的數據概覽。
7. `moments`：計算偏度和峰度等統計量。
8. `MASS`：提供大量的統計技術，如線性和羅吉斯迴歸。
9. `caTools`：數據分割、模型評估等工具。
10. `class`：K 最近鄰算法的函數。

### 二、參考文章與出版物 
- 蕭偉泓（2022），《應用卷積神經網路於雲影像降雨預測》，嶺東科技大學資訊管理系碩士班。
- Google最先進天氣模型MetNet-3，預測結果超越傳統數值預報模型，[詳細資料](https://www.ithome.com.tw/news/136634)。
- Rain Prediction: ANN，[Kaggle連結](https://www.kaggle.com/code/karnikakapoor/rain-prediction-ann)。
- 資料降維 — LDA 線性區別分析，[閱讀更多](https://medium.com/data-science-navigator/資料降維-lda-線性區別分析-b8adb3df0e01)。

