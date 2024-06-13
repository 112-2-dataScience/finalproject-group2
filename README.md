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
