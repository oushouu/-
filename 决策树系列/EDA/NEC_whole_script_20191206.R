```{r ライブラリの導入}
library(plyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)


library(ModelMetrics) # for calling ce() function
library(rpart.plot) #for plot trees
library(ipred) # for call bagging()
library(caret) #for calling confusion matrix
library(randomForest)
library(gbm) #for calling gradient boosting machines
library(ROCR) #for ploting roc curve

# Sys.setenv(LANGUAGE = "en")
```
# =============================================data input====================================================

```{r データを読み込む}
# 列のタイプを先に決める(しなければ勝手にNAが多い列が違うタイプに判定して、読み込みが死ぬほど遅い)
data_types1 = c("date", "text", "text", "numeric", "numeric", "numeric", "date", "date", "text", "text", "text", "text", "text")
data_types2 = c("text", "text", "text", "numeric", "numeric", "numeric", "date", "date", "text", "text", "text", "text", "text")

# Excel側で新規シートに必要な列を抽出して、列名を統一してから、R側でデータを読み込む
master <- read_xlsx("組織コード一覧(SAP)_BUコード追記_20180709.xlsx", sheet = 2)
trans_1 <- read_xlsx("工事プロ管データ_20181010.xlsx", sheet = 2, col_types = data_types1)
trans_2 <- read_xlsx("プロ管(18.10-18.12).xlsx", sheet = 2, col_types = data_types2)
trans_3 <- read_xlsx("プロ管(1901-1903).xlsx", sheet = 2, col_types = data_types2)
trans_4 <- read_xlsx("プロ管1904-1909.xlsx", sheet = 2, col_types = data_types2)
```



```{r 日付データタイプと統一}
# 連合する前に「計上年月」変数のデータ型をdateに統一する
trans_1 %<>% 
  mutate(計上年月 = ymd(計上年月),
             工事初回受注登録年月日 = ymd(工事初回受注登録年月日),
             工事最遅検収予定年月日 = ymd(工事最遅検収予定年月日)
  )

trans_2 %<>% 
  mutate(計上年月 = ymd(str_c(計上年月, "01")),
             工事初回受注登録年月日 = ymd(工事初回受注登録年月日),
             工事最遅検収予定年月日 = ymd(工事最遅検収予定年月日)
  )

trans_3 %<>% 
  mutate(計上年月 = ymd(str_c(計上年月, "01")),
             工事初回受注登録年月日 = ymd(工事初回受注登録年月日),
             工事最遅検収予定年月日 = ymd(工事最遅検収予定年月日)
  )

trans_4 %<>% 
  mutate(計上年月 = ymd(str_c(計上年月, "01")),
             工事初回受注登録年月日 = ymd(工事初回受注登録年月日),
             工事最遅検収予定年月日 = ymd(工事最遅検収予定年月日)
  )

```


```{r 連合}
# 三つのトランザクションテーブルを連合する
trans_all <- trans_1 %>% union_all(trans_2) %>% union_all(trans_3) %>% union_all(trans_4)
```

```{r 結合}
# 連合された表をマスターと結合する
df <- trans_all %>% left_join(master)
```

# =============================================data process====================================================

```{r グループ化}
# 事前にNAデータ件数を確認
trans_all %>% 
  filter(is.na(工事原価累計))

trans_all %>% 
  filter(is.na(工事初回受注登録年月日))

trans_all %>% 
  filter(is.na(工事最遅検収予定年月日))

trans_group <- trans_all %>% 
  group_by(`WBS 要素`, 計上年月) %>% 
  summarise(受注売価 = sum(受注売価, na.rm = TRUE),
                総原価見込額 = sum(総原価見込額, na.rm = TRUE),
                工事原価累計 = sum(工事原価累計, na.rm = TRUE),
                工事初回受注登録年月日 = min(工事初回受注登録年月日, na.rm = TRUE),
                工事最遅検収予定年月日 = max(工事最遅検収予定年月日, na.rm = TRUE))

# 集計したデータのNA値（is.infiniteで）確認する
trans_group %>% 
  filter(is.na(工事原価累計))

trans_group %>% 
  filter(is.infinite(工事初回受注登録年月日))

trans_group %>% 
  filter(is.infinite(工事最遅検収予定年月日))
```

```{r}
print(str_c("総レコード数：", as.character(nrow(trans_all))))
print(str_c("総案件数：", as.character(nrow(count(group_by(trans_all, `WBS 要素`))))))
```

```{r loss-control patterns definition}
# NEC13_ロスコン予測分析_20191028 P.10 loss-control definition
trans_group_pattern <- trans_group %>% 
  mutate(見込赤字 = case_when(
    受注売価 >= 総原価見込額 ~ 0,
    受注売価 < 総原価見込額 ~ 1),
    実績赤字 = case_when(
      受注売価 >= 工事原価累計 ~ 0,
      受注売価 < 工事原価累計 ~ 1),
    予算超過 = case_when(
      総原価見込額 >= 工事原価累計 ~ 0,
      総原価見込額 < 工事原価累計 ~ 1),
    パターン = case_when(
      見込赤字 == 0 & 実績赤字 == 0 & 予算超過 == 0 ~ 1,
      見込赤字 == 0 & 実績赤字 == 0 & 予算超過 == 1 ~ 2,
      見込赤字 == 1 & 実績赤字 == 0 & 予算超過 == 0 ~ 3,
      見込赤字 == 0 & 実績赤字 == 1 & 予算超過 == 1 ~ 4,
      見込赤字 == 1 & 実績赤字 == 1 & 予算超過 == 0 ~ 5,
      見込赤字 == 1 & 実績赤字 == 1 & 予算超過 == 1 ~ 6),
    ロスコン判定 = case_when(
      パターン %in% c(1,2) ~ 0,
      パターン %in% c(3,4,5,6) ~ 1)
  )
# mutate(パターン1フラグ = ifelse(パターン == 1, 1, 0),
#        パターン2フラグ = ifelse(パターン == 2, 1, 0),
#        パターン3フラグ = ifelse(パターン == 3, 1, 0),
#        パターン4フラグ = ifelse(パターン == 4, 1, 0),
#        パターン5フラグ = ifelse(パターン == 5, 1, 0),
#        パターン6フラグ = ifelse(パターン == 6, 1, 0),
#        )

trans_group_pattern

# 案件＆四半期単位に集約
trans_group_pattern %>%
  select(計上年月, `WBS 要素`, パターン) %>% 
  distinct() %>% 
  filter(month(計上年月) %in% c(3,6,9,12)) %T>%
  print() %>% 
  group_by(パターン) %>% 
  count() %>% 
  mutate(比率 = format(n/33819, scientific=F)) 
```

```{r パターンとロスコン判定のマスタを作る}
master_pattern_and_lossconJudge <- trans_group_pattern %>% 
  select(計上年月, `WBS 要素`, パターン, ロスコン判定) %>% 
  distinct()
```


```{r add exception flag 1}
# 年月単位の案件に、もしにロスコン判定が存在すれば、その案件番号とロスコン発生した最初の年月の情報を抽出する
master_exp1 <- trans_group_pattern %>% 
  filter(ロスコン判定 == 1) %>% 
  group_by(`WBS 要素`) %>% 
  mutate(ロスコン初計上年月 = min(計上年月)) %>% 
  select(`WBS 要素`, ロスコン初計上年月) %>% 
  distinct()

master_exp1 #401件

master_wbs_losscon <- trans_group_pattern %>% 
  filter(ロスコン判定 == 1) %>% 
  filter(month(計上年月) %in% c(3,6,9,12)) %>%
  group_by(`WBS 要素`) %>% 
  mutate(四半期初回ロスコン = min(計上年月)) %>% 
  select(`WBS 要素`, 四半期初回ロスコン) %>% 
  distinct()

master_wbs_losscon  #335件


# 赤字後データの除外フラグ&ロスコン案件マーク（仮教師ｙ）を追加
df_with_exp1 <- df %>% 
  left_join(master_exp1) %>% 
  left_join(master_wbs_losscon) %>% 
  mutate(赤字後除外フラグ = case_when(
    is.na(ロスコン初計上年月) ~ 0,
    !is.na(ロスコン初計上年月) & 計上年月 < ロスコン初計上年月 ~ 0,
    !is.na(ロスコン初計上年月) & 計上年月 >= ロスコン初計上年月 ~ 1
  ),
  ロスコン案件マーク = ifelse(is.na(四半期初回ロスコン), 0, 1)
  )

# ロスコン後データの除外Vsロスコン後データレコード数確認
df_with_exp1 %>% 
  count(赤字後除外フラグ)

# ロスコン後データの除外Vsロスコン後データ案件数確認
df_with_exp1 %>% 
  select(`WBS 要素`, 赤字後除外フラグ) %>% 
  distinct() %>% 
  dplyr::count(赤字後除外フラグ)
```




```{r add exception flag 2 from exp1 = 0 dataset}

# 直近（2019年9月）のレコードが実在する案件list
list_uncertained201909 <- df_with_exp1 %>% 
  filter(赤字後除外フラグ == 0) %>% 
  filter(計上年月 == "2019-09-01") %>% 
  select(`WBS 要素`) %>% 
  distinct() %>% 
  .$`WBS 要素`

df_with_exp1_exp2 <- df_with_exp1 %>% 
  mutate(直近除外フラグ = case_when(
    `WBS 要素` %in% list_uncertained201909 ~ 1,
    !(`WBS 要素` %in% list_uncertained201909) ~0
  ))

# 結果未定データの除外Vs結果未定データレコード数確認
df_with_exp1_exp2 %>% 
  filter(赤字後除外フラグ == 0) %>% 
  group_by(直近除外フラグ) %>% 
  count()

# 結果未定データの除外Vs結果未定データ案件数確認
df_with_exp1_exp2 %>% 
  filter(赤字後除外フラグ == 0) %>% 
  group_by(直近除外フラグ) %>%
  select(`WBS 要素`, 直近除外フラグ) %>% 
  distinct() %>% 
  count()

```


```{r train_test_split_data}
# 学習用／テスト用データ
train_test_split_data <- df_with_exp1_exp2 %>% 
  filter(赤字後除外フラグ == 0, 直近除外フラグ == 0)
# select(`WBS 要素`,
#        計上年月,
#        受注売価,
#        総原価見込額,
#        工事原価累計,
#        工事最遅検収予定年月日,
#        工事初回受注登録年月日,
#        利益センタ,
#        ロスコン案件マーク)

# 2018年12月のレコードが存在している案件
list_test_set201812 <- train_test_split_data %>% 
  filter(計上年月 == "2018-12-01") %>% 
  group_by(`WBS 要素`) %>% 
  count() %>% 
  .$`WBS 要素` 

# 学習用
training_set <- train_test_split_data %>% 
  filter(!(`WBS 要素` %in% list_test_set201812))

# テスト用
test_set <- train_test_split_data %>% 
  filter(`WBS 要素` %in% list_test_set201812)

```


```{r}
print(str_c("学習用レコード数：", as.character(nrow(training_set))))
print(str_c("学習用案件数：", as.character(nrow(distinct(select(training_set, `WBS 要素`))))))
print(str_c("テスト用レコード数：", as.character(nrow(test_set))))
print(str_c("テスト用案件数：", as.character(nrow(distinct(select(test_set, `WBS 要素`))))))
```

# =============================================data edition====================================================

```{r 学習・テスト用データセットを｛WBS要素＆計上年月｝単位で集計する}
# 学習・テスト用データセットを｛WBS要素＆計上年月｝単位で集計する
train_test_split_data_group <- train_test_split_data %>% 
  group_by(`WBS 要素`, 計上年月) %>% 
  summarise(受注売価 = sum(受注売価, na.rm = TRUE),
                総原価見込額 = sum(総原価見込額, na.rm = TRUE),
                工事原価累計 = sum(工事原価累計, na.rm = TRUE),
                工事初回受注登録年月日 = min(工事初回受注登録年月日, na.rm = TRUE),
                工事最遅検収予定年月日 = max(工事最遅検収予定年月日, na.rm = TRUE),
                ロスコン案件マーク = as.logical(max(ロスコン案件マーク, na.rm = TRUE))
  ) %>% 
  mutate(計上年月末 = ceiling_date(計上年月, "month") - days(1),
              月度 = as.integer(month(計上年月)),
              工事期間総日数 = as.integer(工事最遅検収予定年月日 - 工事初回受注登録年月日),
              経過日数 = as.integer(計上年月末 - 工事初回受注登録年月日)
  ) %>% 
  mutate(工期進捗率 = 経過日数/工事期間総日数,
              見込原価率 = 総原価見込額/受注売価,
              実績原価率 = 工事原価累計/受注売価,
              原価率進捗 = 実績原価率/工期進捗率
  )                                  #多変数比率に関する特徴変数を作る
```

```{r 4Q計上粒度で金額変動に関する変数を追加}
# 4Q計上粒度で金額変動に関する変数を追加
train_test_split_data_group_4Q_change <- train_test_split_data_group %>% 
  
  filter(month(計上年月) %in% c(3,6,9,12)) %>% 　#四半期絞り
  
  mutate(row_flag = 1) %>% #wbsの総計上回数のcumsumを計算するために
  
  group_by(`WBS 要素`) %>% 　#以下はWBS単位で金額変動に関する変数を追加
  
  mutate(前回受注売価 = lag(受注売価),　　#前四半期の計上金額を取る
               前回総原価見込額 = lag(総原価見込額),
               前回工事原価累計 = lag(工事原価累計),
               
               前回見込原価率 = lag(見込原価率),
               前回実績原価率 = lag(実績原価率)) %>% 
  
  mutate(受注売価増減額 = ifelse(is.na(前回受注売価), 0, 受注売価 - 前回受注売価), #フラグ作り
                受注売価変化度 = 受注売価増減額/前回受注売価,
                受注売価変動フラグ = ifelse(受注売価増減額 != 0, 1, 0),
                受注売価上昇フラグ = ifelse(受注売価増減額 > 0, 1, 0),
  ) %>% 
  
  mutate(総原価見込額増減額 = ifelse(is.na(前回総原価見込額), 0, 総原価見込額 - 前回総原価見込額),
                  総原価見込額変化度 = 総原価見込額増減額/前回総原価見込額,
                  総原価見込額変動フラグ = ifelse(総原価見込額増減額 != 0, 1, 0),
                  総原価見込額上昇フラグ = ifelse(総原価見込額増減額 > 0, 1, 0),
  ) %>% 
  
  mutate(工事原価累計増減額 = ifelse(is.na(前回工事原価累計), 0, 工事原価累計 - 前回工事原価累計),
                  工事原価累計変化度 = 工事原価累計増減額/前回工事原価累計,
                  工事原価累計変動フラグ = ifelse(工事原価累計増減額 != 0, 1, 0),
                  工事原価累計上昇フラグ = ifelse(工事原価累計増減額 > 0, 1, 0),
  ) %>% 
  
  mutate(見込原価率増減額 = ifelse(is.na(前回見込原価率), 0, 見込原価率 - 前回見込原価率),
                 見込原価率変化度 = 見込原価率増減額/前回見込原価率,
                 見込原価率変動フラグ = ifelse(見込原価率増減額 != 0, 1, 0),
                 見込原価率上昇フラグ = ifelse(見込原価率増減額 > 0, 1, 0),
  ) %>% 
  
  mutate(実績原価率増減額 = ifelse(is.na(前回実績原価率), 0, 実績原価率 - 前回実績原価率),
                 実績原価率変化度 = 実績原価率増減額/前回実績原価率,
                 実績原価率変動フラグ = ifelse(実績原価率増減額 != 0, 1, 0),
                 実績原価率上昇フラグ = ifelse(実績原価率増減額 > 0, 1, 0),
                 # 
                 # 前回実績原価率上昇フラグ = lag(実績原価率上昇フラグ),
                 # 実績原価率二回連続上昇フラグ = ifelse(実績原価率上昇フラグ == 1 & 
                 #                                        前回実績原価率上昇フラグ ==1, 1, 0)
  ) %>% 
  
  mutate(総計上回数 = cumsum(row_flag),　　# 上昇層回数と連続上昇区分ラベル作り
              受注売価上昇総回数 = cumsum(受注売価上昇フラグ),
              総原価見込額上昇総回数 = cumsum(総原価見込額上昇フラグ),
              工事原価累計上昇総回数 = cumsum(工事原価累計上昇フラグ),
              見込原価率上昇総回数 = cumsum(見込原価率上昇フラグ),
              実績原価率上昇総回数 = cumsum(実績原価率上昇フラグ),
              label_受注売価_up = cumsum(c(0, diff(受注売価上昇フラグ)<0)),
              label_総原価見込額_up = cumsum(c(0, diff(総原価見込額上昇フラグ)<0)),
              label_工事原価累計_up = cumsum(c(0, diff(工事原価累計上昇フラグ)<0)),
              label_見込原価率_up = cumsum(c(0, diff(見込原価率上昇フラグ)<0)),
              label_実績原価率_up = cumsum(c(0, diff(実績原価率上昇フラグ)<0)),
  ) %>% 
  group_by(`WBS 要素`, label_受注売価_up) %>% 　# 連続上昇回数計算（0リセット）
  mutate(受注売価連続上昇回数 = cumsum(受注売価上昇フラグ)) %>% 
  group_by(`WBS 要素`, label_総原価見込額_up) %>% 
  mutate(総原価見込額連続上昇回数 = cumsum(総原価見込額上昇フラグ)) %>%
  group_by(`WBS 要素`, label_工事原価累計_up) %>% 
  mutate(工事原価累計連続上昇回数 = cumsum(工事原価累計上昇フラグ)) %>%
  group_by(`WBS 要素`, label_見込原価率_up) %>% 
  mutate(見込原価率連続上昇回数 = cumsum(見込原価率上昇フラグ)) %>%
  group_by(`WBS 要素`, label_実績原価率_up) %>% 
  mutate(実績原価率連続上昇回数 = cumsum(実績原価率上昇フラグ))

```

# =============================================Tree based modeling====================================================
```{r choose varibles which will be used in model}

# define a name list of train(test) variables
classification_var <- c("WBS 要素", "受注売価", "総原価見込額", "ロスコン案件マーク", "月度", "工事期間総日数", "工期進捗率", "見込原価率", "原価率進捗", "総計上回数", "受注売価上昇総回数", "総原価見込額上昇総回数", "見込原価率上昇総回数", "受注売価連続上昇回数", "総原価見込額連続上昇回数", "見込原価率連続上昇回数")


# select colunms in the name list
Tree_base_data <- train_test_split_data_group_4Q_change[ ,classification_var]

# deal with NA and other improper data
Tree_base_data_fct <- Tree_base_data %>% 
  mutate(ロスコン案件マーク = as.factor(ロスコン案件マーク), #factorに変換
                  月度 = as.factor(月度)) %>% 
  mutate(見込原価率 = ifelse(is.na(見込原価率), 0, 見込原価率), #見込原価率NAの場合、0に一致する
              工期進捗率 = ifelse(is.na(工期進捗率), 見込原価率, 工期進捗率), #工期進捗率NAの場合、見込原価率に一致する
              見込原価率上昇総回数 = ifelse(is.na(見込原価率上昇総回数), 0, 見込原価率上昇総回数), #NAの場合、0に一致する
              見込原価率連続上昇回数 = ifelse(is.na(見込原価率連続上昇回数), 0, 見込原価率連続上昇回数),
              工事期間総日数 = ifelse(is.na(工事期間総日数), mean(.$工事期間総日数, na.rm = TRUE), 工事期間総日数),
              原価率進捗= ifelse(is.na(原価率進捗), 1, 原価率進捗)) %>% 
  mutate(工期進捗率 = ifelse(工期進捗率>1, 1, 工期進捗率))


```



```{r split into train and test set}
NEC_train <- Tree_base_data_fct %>% 
  filter(!(`WBS 要素` %in% list_test_set201812)) %>% 
  select(-`WBS 要素`)

NEC_test <- Tree_base_data_fct %>% 
  filter(`WBS 要素` %in% list_test_set201812) %>% 
  select(-`WBS 要素`)

# for validate models
# Total number of rows in the iris data frame
n <- nrow(NEC_test)

n_valid <- round(.5 * n) 

set.seed(123)
valid_indices <- sample(1:n, n_valid)

# Subset the iris data frame to training indices only
NEC_subvalid <- NEC_test[valid_indices, ]  

# Exclude the training indices to create the test set
NEC_subtest <- NEC_test[-valid_indices, ] 
```

```{r Train a claasification tree model}
class_tree_model <- rpart(formula = ロスコン案件マーク ~ ., 
                          data = NEC_train, 
                          method = "class",
                          xval = 20)

rpart.plot(x = class_tree_model, yesno = 2, type = 1, extra = "auto")
```


```{r}
names(class_tree_model)

class_tree_model$variable.importance
class_tree_model$control

```

```{r performance}
# Generate predictions on the validation set using the gini model
class_tree_pred <- predict(object = class_tree_model, 
                           newdata = NEC_test,
                           type = "class")  

class_tree_pred_prob <- predict(object = class_tree_model, 
                                newdata = NEC_test,
                                type = "prob")

# Calculate the confusion matrix for the test set
confusionMatrix(data = class_tree_pred,       
                reference = NEC_test$ロスコン案件マーク) 


AUC_tree <- auc(actual = NEC_test$ロスコン案件マーク, 
                predicted = class_tree_pred_prob[,2])

# Compute the RMSE
rmse(actual = NEC_test$ロスコン案件マーク, 
     predicted = class_tree_pred)

AUC_tree
```



### tuning

```{r tuning the model}
# Plot the "CP Table"
plotcp(class_tree_model)

# Print the "CP Table"
print(class_tree_model$cptable)

# Retrieve optimal cp value based on cross-validated error
opt_index <- which.min(class_tree_model$cptable[, "xerror"])
cp_opt <- class_tree_model$cptable[opt_index, "CP"]

# Prune the model (to optimized cp value)
class_tree_model_cp_optimal <- prune(tree = class_tree_model,
                                     cp = cp_opt)

# Plot the optimized model
rpart.plot(x = class_tree_model_cp_optimal, yesno = 2, type = 1, extra = "auto")

#the optimal model does not change anything because the last split of the previous model already has the smallest xerror

```

```{r generate a grid of hyperparameter values}
# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(10, 30, 10)
# maxdepth <- seq(16, 30, 2)
maxcompete <- c(3,4,5)
criterion <- c("gini", "information")

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(minsplit = minsplit, maxcompete = maxcompete, criterion = criterion) #expand.grid() very useful

# Check out the grid
head(hyper_grid)

# Print the number of grid combinations
nrow(hyper_grid)
```

# =============================================Bagged Tree Modeling====================================================

```{r}
bagged_tree_model <-  bagging(formula = ロスコン案件マーク ~ ., 
                              data = NEC_train,
                              xval = 10,
                              coob = TRUE)

# Print the model
print(bagged_tree_model)

# see the result of the first sub-tree model
# bagged_tree_model$mtrees[1]
```

```{r prediction and confusion matrix}
# Generate predicted classes using the model object
bagged_tree_model_pred <- predict(object = bagged_tree_model,    
                                  newdata = NEC_test,  
                                  type = "class")  # return classification labels

# Print the predicted classes
# print(bagged_tree_model_pred)

# Calculate the confusion matrix for the test set
confusionMatrix(data = bagged_tree_model_pred,       
                reference = NEC_test$ロスコン案件マーク)  
```

```{r r predict on a test set and compute AUC}
# Generate predictions on the test set
bagged_tree_model_pred_prob <- predict(object = bagged_tree_model,
                                       newdata = NEC_test,
                                       type = "prob")

# `pred` is a matrix
# class(bagged_tree_model_pred_prob)

# Look at the pred format
head(bagged_tree_model_pred_prob)

# Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
AUC_Bagged <- auc(actual = NEC_test$ロスコン案件マーク, 
                  predicted = bagged_tree_model_pred_prob[,2])
```

```{r cross-validate a bagged tree model in caret}
# Specify the training configuration
ctrl <- trainControl(method = "cv",     # Cross-validation
                     number = 10,      # 5 folds
                     classProbs = TRUE,                  # For AUC
                     summaryFunction = twoClassSummary)  # For AUC


# rename factor level from "TRUE", "FALSE" to "yes", "no"
NEC_train_dup <- NEC_train %>% 
  mutate(ロスコン案件マーク = revalue(ロスコン案件マーク, c("TRUE" = "yes", "FALSE" = "no")))
NEC_test_dup <- NEC_test %>% 
  mutate(ロスコン案件マーク = revalue(ロスコン案件マーク, c("TRUE" = "yes", "FALSE" = "no")))

# Cross validate the titanic model using "treebag" method; 
# Track AUC (Area under the ROC curve)
set.seed(1)  # for reproducibility
bagged_tree_caret_model <- train(ロスコン案件マーク ~ .,
                                          data = NEC_train_dup, 
                                          method = "treebag",
                                          metric = "ROC",
                                          trControl = ctrl,
                                          na.action=na.exclude)

# Inspect the contents of the model list  
names(bagged_tree_caret_model)

# Print the CV AUC
bagged_tree_caret_model$results[,"ROC"]
```

```{r generate predictions from the caret model}
# Generate predictions on the test set
bagged_tree_caret_model_pred <- predict(object = bagged_tree_caret_model, 
                                        newdata = NEC_test_dup,
                                        type = "prob")

# check if actual and predict have same numbers of observation
nrow(bagged_tree_caret_model_pred)
length(NEC_test_dup$ロスコン案件マーク)

# Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
auc(actual = NEC_test_dup$ロスコン案件マーク, 
    predicted = bagged_tree_caret_model_pred[,"yes"])

```

# =============================================Random Forest Modeling====================================================

```{r Train a Random Forest model}
# 原価率進捗 has several inf values which cannot automatically be processed in RF
# also, since 原価率進捗 is less important potetial variable in a single tree, so it can be removed here
NEC_train_RF <- NEC_train_dup %>% 
  select(-原価率進捗)

NEC_test_RF <- NEC_test_dup %>% 
  select(-原価率進捗)

# Train a Random Forest
set.seed(2)  # for reproducibility
random_forest_model <- randomForest(formula = ロスコン案件マーク ~ ., 
                                    data = NEC_train_RF, 
                                    mtry = 13) #use the best tuneRF result

# Print the model output                             
print(random_forest_model)
```


```{r Evaluate out-of-bag error}
# Grab OOB error matrix & take a look
err <- random_forest_model$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

class(random_forest_model)

# Plot the model trained in the previous exercise
plot(random_forest_model)

# Add a legend since it doesn't have one by default
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))
```

```{r Evaluate model performance on a test set}

# Generate predicted classes using the model object
random_forest_model_pred <- predict(object = random_forest_model,   # model object 
                                    newdata = NEC_test_RF,  # test dataset
                                    type = "class") # return classification labels

random_forest_model_pred_prob <- predict(object = random_forest_model,   # model object 
                                         newdata = NEC_test_RF,  # test dataset
                                         type = "prob") # return classification probs

# Calculate the confusion matrix for the test set
cm <- confusionMatrix(data = random_forest_model_pred,       # predicted classes
                      reference = NEC_test_RF$ロスコン案件マーク)  # actual classes


# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)

print(cm)

AUC_RF <- auc(actual = NEC_test_RF$ロスコン案件マーク, 
              predicted = random_forest_model_pred_prob[,2])

AUC_RF
```

## TUning

```{r Tuning a Random Forest via mtry}
# Execute the tuning process
# tuneRF仅限于找最佳的特征变量个数（mtry），而不能用于找其他最佳参数，非常规调参方法
set.seed(2)              
res <- tuneRF(x = subset(NEC_train_RF, select = -ロスコン案件マーク),
              y = NEC_train_RF$ロスコン案件マーク,
              ntreeTry = 500,
              doBest = F)

# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)

# If you just want to return the best RF model (rather than results)
# you can set `doBest = TRUE` in `tuneRF()` to return the best RF model
# instead of a set performance matrix.
```

```{r Tuning a Random Forest via tree depth 注意：1時間かかる！！！}
# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(3, 13, 2)
nodesize <- seq(3, 10, 3)
sampsize <- round(nrow(NEC_train_RF) * c(0.7, 0.8))

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = ロスコン案件マーク ~ ., 
                        data = NEC_train_RF,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

# 最優結果
#     mtry　nodesize　sampsize
# 24	13	  3	        18934	
```

```{r}
names(random_forest_model)
```
```{r}
random_forest_model$votes

```


```{r based on optimal modeling}
best_RF_model <- randomForest(formula = ロスコン案件マーク ~ ., 
                              data = NEC_train_RF,
                              mtry = 13,
                              nodesize = 3,
                              sampsize = 18934)

#  compare best_model with titanic_model

print(random_forest_model)
print(best_RF_model)

```

```{r auc after tuning}
best_RF_model_pred <- predict(object = best_RF_model,   # model object 
                              newdata = NEC_test_RF,  # test dataset
                              type = "class") # return classification label

best_RF_model_pred_prob <- predict(object = best_RF_model,   # model object 
                                   newdata = NEC_test_RF,  # test dataset
                                   type = "prob") # return classification probs

confusionMatrix(data = best_RF_model_pred,       
                reference = NEC_test_RF$ロスコン案件マーク) 

auc(actual = NEC_test_RF$ロスコン案件マーク, 
    predicted = best_RF_model_pred_prob[,"yes"])


```

# =============================================Grandient Boosting Modeling===========================================


```{r Train a GBM model}
# For binary classification, gbm() requires the response to be encoded as 0/1 (numeric), so we will have to convert from a "no/yes" factor to a 0/1 numeric response column.

# Convert factor"1" to 1, factor"0" to 0
NEC_train_GBM <- NEC_train_RF %>% 
  mutate(ロスコン案件マーク = ifelse(ロスコン案件マーク == "yes", 1, 0))

NEC_test_GBM <- NEC_test_RF %>% 
  mutate(ロスコン案件マーク = ifelse(ロスコン案件マーク == "yes", 1, 0))

# Train a 10000-tree GBM model
set.seed(2)
gradient_boosting_model <- gbm(formula = ロスコン案件マーク ~ ., 
                               distribution = "bernoulli", 
                               data = NEC_train_GBM,
                               n.trees = 10000)

# Print the model object                    
print(gradient_boosting_model)

names(gradient_boosting_model)

# summary() prints variable importance
# 这里模型中的y必须是数字而非factor，否则报错
summary(gradient_boosting_model)
```

```{r Prediction using a GBM model}

# Generate predictions on the test set
gradient_boosting_model_pred <- predict(object = gradient_boosting_model, 
                                        newdata = NEC_test_GBM,
                                        n.trees = 10000)

# Generate predictions on the test set (scale to response)
gradient_boosting_model_pred_res <- predict(object = gradient_boosting_model, 
                                            newdata = NEC_test_GBM,
                                            n.trees = 10000,
                                            type = "response")

# Compare the range of the two sets of predictions
range(gradient_boosting_model_pred)
range(gradient_boosting_model_pred_res)

head(gradient_boosting_model_pred)
head(gradient_boosting_model_pred_res)

# 重要结论，事实上基于type为response的preds2就是基于type为link的preds1经过sigmoid函数的映射值
p = 1/(1 + exp(-1 * gradient_boosting_model_pred))

# 实验结果表明二者完全一致
identical(gradient_boosting_model_pred_res, p)

# 延伸：实际上typy="response"和type="prob"返回的都是模型应变量y的概率，
# 只不过gradient boosting machine返回的是link（实数集范围），对应的概率只能是response；
# 而random forest之类的返回的是class（具体分类结果），对应的概率是prob。仅此而已。

# 所以另一个推论是：boosting算法中用了logistic回归算法对各个子决策树分类器进行处理
```

```{r Evaluate test set AUC}
# Generate the test set AUCs using the two sets of preditions & compare
auc(actual = NEC_test_GBM$ロスコン案件マーク, predicted = gradient_boosting_model_pred)  #default
AUC_GBM <- auc(actual = NEC_test_GBM$ロスコン案件マーク, predicted = gradient_boosting_model_pred_res)  #rescaled

# 另一重要结论：事实上Logit(p)和p
```

```{r　Early stopping in GBMs}
# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = gradient_boosting_model, 
                          method = "OOB", 
                          oobag.curve = TRUE)

# 一个不处理会导致下一步报错的细节：
# gbm中的input data必须只包含formul中用到的变量，必须完全一致
# titanic_train_notna_numericy_colselected <- titanic_train_notna_numericy %>% 
#   select(Survived, Pclass ,Sex ,Age ,SibSp ,Parch ,Fare ,Embarked)
# 
# titanic_test_numericy_colselected <- titanic_test_numericy %>% 
#   select(Survived, Pclass ,Sex ,Age ,SibSp ,Parch ,Fare ,Embarked)

# Train a CV GBM model
set.seed(2)
gradient_boosting_model_cv <- gbm(formula = ロスコン案件マーク ~ ., 
                                  distribution = "bernoulli", 
                                  data = NEC_train_GBM,
                                  n.trees = 10000,
                                  cv.folds = 5)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = gradient_boosting_model_cv, 
                         method = "cv")

# Compare the estimates                         
print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))                         
print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))
```


```{r OOB vs CV-based early stopping}
# Generate predictions on the test set using ntree_opt_oob number of trees
gradient_boosting_model_cv_pred_oob <- predict(object = gradient_boosting_model_cv, 
                                               newdata = NEC_test_GBM,
                                               n.trees = ntree_opt_oob)

# Generate predictions on the test set using ntree_opt_cv number of trees
gradient_boosting_model_cv_pred_cv <- predict(object = gradient_boosting_model_cv, 
                                              newdata = NEC_test_GBM,
                                              n.trees = ntree_opt_cv)   

# Generate the test set AUCs using the two sets of preditions & compare
auc_GB_best_oob <- auc(actual = NEC_test_GBM$ロスコン案件マーク, predicted = gradient_boosting_model_cv_pred_oob)  #OOB
auc_GB_best_cv <- auc(actual = NEC_test_GBM$ロスコン案件マーク, predicted = gradient_boosting_model_cv_pred_cv)  #CV 

# Compare AUC 
print(paste0("Test set AUC (OOB): ", auc_GB_best_oob))                         
print(paste0("Test set AUC (CV): ", auc_GB_best_cv))
```


```{r export var importance table and plot}
GBM_importance <- gradient_boosting_model %>% summary()

write.csv(GBM_importance, "GBM_importance.csv") 

GBM_importance %>% 
  mutate(var = fct_reorder(var, rel.inf)) %>% 
  ggplot(aes(var, rel.inf)) +
  geom_col() +
  coord_flip() +
  labs(title = "GBMモデル説明変数重要度係数") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("GBMモデル説明変数重要度係数.png", width = 8, height = 6)
```


```{r Plot & compare ROC curves}
# List of predictions
preds_list <- list(class_tree_pred_prob[,2], 
                   bagged_tree_model_pred_prob[,2],
                   random_forest_model_pred_prob[,2],
                   gradient_boosting_model_pred_res)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(NEC_test_GBM$ロスコン案件マーク), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("single tree", "bagged trees", "random forest", "gradient boosting"),
       fill = 1:m)

# length(class_tree_pred_prob[,2])
# length(bagged_tree_model_pred_prob[,2])
# length(random_forest_model_pred_prob[,2])
# length(gradient_boosting_model_pred_res)


print(paste0("Single Tree AUC: ", AUC_tree))
print(paste0("Bagged Tree AUC: ", AUC_Bagged))
print(paste0("Random Forest AUC: ", AUC_RF))
print(paste0("Gradient Boosting AUC: ", AUC_GBM))
```



# =============================================scoring data output====================================================
```{r}
score_result <- read.csv("スコアリング結果_201812.csv")

score_resul_edit <- score_result %>% 
  mutate(計上年月 = ymd(str_c(計上年月, "01")),
             計上年月日 = ceiling_date(計上年月, "month") - days(1)) 


write.csv(score_resul_edit, "score_resul_edit_201812.csv")
```



```{r 藤本さんのoutputデータにscoreがNAじゃない行数とGBM scoreの行数が一致する}
score_resul_edit %>% 
  filter(!is.na(X3変数モデルランク)) %>% 
  nrow()

nrow(NEC_train)+nrow(NEC_test)
range()

# ここにするのは、GBMのoutput dataを吐く、powerbiでrelationを作ること

score_var <- c("WBS 要素", "計上年月", "受注売価", "総原価見込額", "ロスコン案件マーク", "月度", "工事期間総日数", "工期進捗率", "見込原価率", "原価率進捗", "総計上回数", "受注売価上昇総回数", "総原価見込額上昇総回数", "見込原価率上昇総回数", "受注売価連続上昇回数", "総原価見込額連続上昇回数", "見込原価率連続上昇回数")

# select colunms in the name list
Tree_base_data0 <- train_test_split_data_group_4Q_change[ ,score_var]

# deal with NA and other improper data
Tree_base_data_fct0 <- Tree_base_data0 %>% 
  mutate(ロスコン案件マーク = as.factor(ロスコン案件マーク), #factorに変換
                  月度 = as.factor(月度)) %>% 
  mutate(見込原価率 = ifelse(is.na(見込原価率), 0, 見込原価率), #見込原価率NAの場合、0に一致する
              工期進捗率 = ifelse(is.na(工期進捗率), 見込原価率, 工期進捗率), #工期進捗率NAの場合、見込原価率に一致する
              見込原価率上昇総回数 = ifelse(is.na(見込原価率上昇総回数), 0, 見込原価率上昇総回数), #NAの場合、0に一致する
              見込原価率連続上昇回数 = ifelse(is.na(見込原価率連続上昇回数), 0, 見込原価率連続上昇回数),
              工事期間総日数 = ifelse(is.na(工事期間総日数), mean(.$工事期間総日数, na.rm = TRUE), 工事期間総日数),
              原価率進捗= ifelse(is.na(原価率進捗), 1, 原価率進捗)) %>% 
  mutate(工期進捗率 = ifelse(工期進捗率>1, 1, 工期進捗率))

NEC_train_test <- Tree_base_data_fct0%>% 
  mutate(ロスコン案件マーク = revalue(ロスコン案件マーク, c("TRUE" = "yes", "FALSE" = "no"))) %>% 
  mutate(ロスコン案件マーク = ifelse(ロスコン案件マーク == "yes", 1, 0))

GBM_score <- predict(object = gradient_boosting_model,
                     newdata = select(NEC_train_test, -c(`WBS 要素`, 原価率進捗, 計上年月)),
                     n.trees = 10000,
                     type = "response")


NEC_train_test_with_score <- NEC_train_test %>% 
  mutate(GBMスコア = GBM_score)


NEC_train_test_with_score %>% 
  select(`WBS 要素`, 計上年月) %>% 
  unique



NEC_train_test_with_score_rank <- NEC_train_test_with_score %>% 
  arrange(計上年月, desc(GBMスコア)) %>%
  mutate(flag1 = 1) %>% 
  group_by(計上年月) %>%
  mutate(GBMモデルランク = cumsum(flag1)) %>% 
  select(-flag1)

NEC_GBM_score_rank <- NEC_train_test_with_score_rank %>% 
  select(`WBS 要素`, 計上年月, GBMスコア, GBMモデルランク)

write.csv(NEC_GBM_score_rank, "NEC_GBM_score_rank_201812.csv") 


```