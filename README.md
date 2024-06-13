# [Group02] 大台北區降雨數據分析及多模型預測效能比較
The goals of this project.

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|李承恩|資科大四|109703010|收集資料,資料前處理| 
|楊榮瑜|哲學碩四|1091540054|EDA,特徵工程|
|黃妍心|國貿大四|109301091|建模|
|陳昊暄|風管大三|110207402|建模|
|陳為政|教育大三|110102050|建模|
## Quick start
Please provide an example command or a few commands to reproduce your analysis, such as the following R script:
```R
Rscript code/your_script.R --input data/training --output results/performance.tsv
```

## Folder organization and its related description
idea by Noble WS (2009) [A Quick Guide to Organizing Computational Biology Projects.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424) PLoS Comput Biol 5(7): e1000424.

### docs
* Your presentation, 1122_DS-FP_groupID.ppt/pptx/pdf (i.e.,1122_DS-FP_group1.ppt), by **06.13**
* Any related document for the project, i.e.,
  * discussion log
  * software user guide

### data
* Input
** Source:https://codis.cwa.gov.tw/
** Format:CSV
** Size:1869筆

### code
* Analysis steps:
* EDA:
*    確認資料類型
*    確認標籤分布
*      將非數值資料轉為數值型態
*    確認偏度
*      標準化
*      確認數值範圍
*    去除離群值
*    計算相關係數與P-value
*    降維:LDA與PCA
* Which method or package do you use?
*  EDA:
*   library(readxl)
*   library(corrplot)
*   library(ggplot2)
*   library(dplyr)
*   library(caret)
*   library(skimr)
*   library(moments)
*   library(MASS)
*   library(caTools)
*   library(class)
* How do you perform training and evaluation?
  * Cross-validation, or extra separated data
* What is a null model for comparison?

## code
* Analysis steps:
*   EDA:
*    確認資料類型
*    確認標籤分布
*    將非數值資料轉為數值型態
*    確認偏度
*    標準化
*    確認數值範圍
*    去除離群值
*    計算相關係數與P-value
*    降維:LDA與PCA
* Which method or package do you use?
*  EDA:
*   library(readxl)
*   library(corrplot)
*   library(ggplot2)
*   library(dplyr)
*   library(caret)
*   library(skimr)
*   library(moments)
*   library(MASS)
*   library(caTools)
*   library(class)
* How do you perform training and evaluation?
  * Cross-validation, or extra separated data
* What is a null model for comparison?

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




### results
* What is your performance?
* Is the improvement significant?

## References
* Packages you use
*   library(readxl)
*   library(corrplot)
*   library(ggplot2)
*   library(dplyr)
*   library(caret)
*   library(skimr)
*   library(moments)
*   library(MASS)
*   library(caTools)
*   library(class)
* Related publications
