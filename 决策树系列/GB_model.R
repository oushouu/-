# change console language to English
# Sys.setenv(LANGUAGE = "en")

# import library
library(tidyverse)
library(lubridate)
library(magrittr)

library(rpart) #for decision tree
library(rpart.plot) #for plot trees
library(caret) #for calling confusion matrix
library(ROCR) #for ploting roc curve
library(ModelMetrics) # for calling ce() auc() function
library(ipred) # for call bagging()
library(randomForest)
library(IDPmisc) # for removing inf na.action = NaRV.omit
library(gbm) #for calling gradient boosting machines
library(openxlsx) #for save reults in xlsx sheets

# show directory file
list.files(path = "./002_LossCon/devdata")

# read data
train <- read.delim("./002_LossCon/devdata/train.txt")
losscon <- read.delim("./002_LossCon/devdata/losscon.txt")
test1 <- read.delim("./002_LossCon/devdata/test1.txt")
test1s <- read.delim("./002_LossCon/devdata/test1s.txt")
test2 <- read.delim("./002_LossCon/devdata/test2.txt")
test2s <- read.delim("./002_LossCon/devdata/test2s.txt")

import_name_list <- c("train", "test1", "test1s", "test2", "test2s")

# put train test datasets into a list for loop processing
import_data <- list()
import_data$train <- train
import_data$test1 <- test1
import_data$test1s <- test1s
import_data$test2 <- test2
import_data$test2s <- test2s


# [losscon data WBS count] = 11750
# [train data WBS count] = 10214
# [test1 data WBS count] = 1562
# [test2 data WBS count] = 1225


#====================================Feature Engineering=========================================
# Group by [WBS*YM]}

WBS_YM_GROUP <- list()

for(data in import_name_list){
  
  WBS_YM_GROUP[[data]] <- import_data[[data]] %>% 
    select(WBS.要素, 計上年月, 販売伝票, 
           利益センタ, 得意先コード, 
           `受注売価.円貨.`, 総原価見込額, `工事原価累計.円貨.`, 
           工事初回受注登録年月日, 工事最遅検収予定年月日,
           BU, X2階層, X3階層) %>% 
    
    mutate(工事初回受注登録年月日 = ymd(工事初回受注登録年月日),
                      工事最遅検収予定年月日 = ymd(工事最遅検収予定年月日)) %>%  
    
    group_by(WBS.要素, 計上年月) %>% 
    
    summarise(受注売価 = sum(`受注売価.円貨.`, na.rm = TRUE),
                  総原価見込額 = sum(総原価見込額, na.rm = TRUE),
                  工事原価累計 = sum(`工事原価累計.円貨.`, na.rm = TRUE),
                  工事初回受注登録年月日 = min(工事初回受注登録年月日, na.rm = TRUE),
                  工事最遅検収予定年月日 = max(工事最遅検収予定年月日, na.rm = TRUE)) %>%
    
    mutate(計上年月日 = ymd(paste0(計上年月, "01")),
                計上年月末 = ceiling_date(計上年月日, "month") - days(1),
                月度 = str_sub(計上年月, 5, 6),
                工事期間総日数 = as.integer(工事最遅検収予定年月日 - 工事初回受注登録年月日),
                経過日数 = as.integer(計上年月末 - 工事初回受注登録年月日)) %>%
    
    mutate(工期進捗率 = 経過日数/工事期間総日数,
                見込原価率 = 総原価見込額/受注売価,
                実績原価率 = 工事原価累計/受注売価,
                原価率進捗 = 実績原価率/工期進捗率
    )    
}




# filter 4Q data only & left join with losscon & add wbs_losscon labels & delete flags}

WBS_YM_GROUP_4Q_LABELED <- list()

for(data in import_name_list){
  
  WBS_YM_GROUP_4Q_LABELED[[data]] <- WBS_YM_GROUP[[data]] %>% 
    
    # filter YM = 4Q
    filter(str_sub(計上年月,5,6) %in% c("03", "06", "09", "12")) %>% 
    
    # add losscon situations (of that moment, not label variable) 
    left_join(losscon) %>% 
    
    
    
    group_by(WBS.要素) %>% 
    
    # add label variable (regard the whole wbs as losscon or not)
    mutate(ロスコン案件マーク = ifelse(max(ロスコン) == 1, 1, 0)) %>% 
    
    # delete flag for after-losscon records
    mutate(delete_flag_happened = ifelse(cumsum(ロスコン) > 0, 1, 0)) %>% 
    
    # delete flag for result uncertained wbs (those unfinished till 201912)
    mutate(delete_flag_unfinished = ifelse(ロスコン案件マーク == 0 & max(計上年月) == "201912", 1, 0))
  
}






# [WBS*YM] keep delete flag as 0 & add features}

WBS_YM_GROUP_FEATURED_LABELED <- list()

for(data in import_name_list){
  
  WBS_YM_GROUP_FEATURED_LABELED[[data]] <- WBS_YM_GROUP_4Q_LABELED[[data]] %>% 
    
    # filter unlosscon records
    filter(delete_flag_happened == 0) %>% 
    
    # filter finished wbs
    filter(delete_flag_unfinished == 0) %>% 
    
    # add features
    mutate(row_flag = 1) %>% #wbsの総計上回数のcumsumを計算するために
    
    group_by(WBS.要素) %>% 　#以下はWBS単位で金額変動に関する変数を追加
    
    mutate(前回受注売価 = lag(受注売価),　　#前四半期の計上金額を取る
                 前回総原価見込額 = lag(総原価見込額),
                 前回工事原価累計 = lag(工事原価累計),
                 
                 前回見込原価率 = lag(見込原価率),
                 前回実績原価率 = lag(実績原価率)
    ) %>% 
    
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
                   見込原価率上昇フラグ = ifelse(見込原価率増減額 > 0, 1, 0)
    ) %>% 
    
    mutate(実績原価率増減額 = ifelse(is.na(前回実績原価率), 0, 実績原価率 - 前回実績原価率),
                   実績原価率変化度 = 実績原価率増減額/前回実績原価率,
                   実績原価率変動フラグ = ifelse(実績原価率増減額 != 0, 1, 0),
                   実績原価率上昇フラグ = ifelse(実績原価率増減額 > 0, 1, 0)
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
                label_実績原価率_up = cumsum(c(0, diff(実績原価率上昇フラグ)<0))
    ) %>% 
    
    group_by(WBS.要素, label_受注売価_up) %>% 　# 連続上昇回数計算（0リセット）
    mutate(受注売価連続上昇回数 = cumsum(受注売価上昇フラグ)) %>% 
    group_by(WBS.要素, label_総原価見込額_up) %>% 
    mutate(総原価見込額連続上昇回数 = cumsum(総原価見込額上昇フラグ)) %>%
    group_by(WBS.要素, label_工事原価累計_up) %>% 
    mutate(工事原価累計連続上昇回数 = cumsum(工事原価累計上昇フラグ)) %>%
    group_by(WBS.要素, label_見込原価率_up) %>% 
    mutate(見込原価率連続上昇回数 = cumsum(見込原価率上昇フラグ)) %>%
    group_by(WBS.要素, label_実績原価率_up) %>% 
    mutate(実績原価率連続上昇回数 = cumsum(実績原価率上昇フラグ))
  
}



# ============================================fitting preparing=====================================================

# define a name list of train(test) variables
classification_var_trail1 <- c("WBS.要素", "計上年月","受注売価", "総原価見込額", "ロスコン案件マーク", "月度", 
                               "工事期間総日数", "工期進捗率", "見込原価率", "原価率進捗",
                               "総計上回数", "受注売価上昇総回数",
                               "総原価見込額上昇総回数", "見込原価率上昇総回数", "受注売価連続上昇回数", "総原価見込額連続上昇回数", "見込原価率連続上昇回数")

TREE_DATA_with_WBS <- list()

for(data in import_name_list){
  
  TREE_DATA_with_WBS[[data]] <- WBS_YM_GROUP_FEATURED_LABELED[[data]][ ,classification_var_trail1] %>%
    
    #factorに変換
    mutate(ロスコン案件マーク = as.factor(ロスコン案件マーク)) %>% 
    
    #NA処理
    mutate(見込原価率 = ifelse(is.na(見込原価率), 0, 見込原価率), 
                工期進捗率 = ifelse(is.na(工期進捗率), 見込原価率, 工期進捗率), 
                見込原価率上昇総回数 = ifelse(is.na(見込原価率上昇総回数), 0, 見込原価率上昇総回数), 
                見込原価率連続上昇回数 = ifelse(is.na(見込原価率連続上昇回数), 0, 見込原価率連続上昇回数),
                工事期間総日数 = ifelse(is.na(工事期間総日数), mean(.$工事期間総日数, na.rm = TRUE), 工事期間総日数)
                # 原価率進捗= ifelse(is.na(原価率進捗), 1, 原価率進捗)
    ) %>%
    
    #工期進捗率>1の処理
    mutate(工期進捗率 = ifelse(工期進捗率>1, 1, 工期進捗率))
}

# Get rid of WBS from features because WBS directly decide label
TREE_DATA_remove_WBS <- list()
for(data in import_name_list){
  TREE_DATA_remove_WBS[[data]] <- TREE_DATA_with_WBS[[data]] %>% 
    select(-WBS.要素) %>% 
    select(-計上年月)
  
}



TEST_SET <- TREE_DATA_remove_WBS[-1]

# set row numbers of datasets
NEC_train <- TREE_DATA_remove_WBS$train 



# for python deep learning Use
TREE_DATA_with_WBS$train %>% write.csv("TREE_DATA_with_WBS_train.csv", na = "", fileEncoding = "UTF-8")
TREE_DATA_with_WBS$test1 %>% write.csv("TREE_DATA_with_WBS_test1.csv", na = "", fileEncoding = "UTF-8")
TREE_DATA_with_WBS$test1s %>% write.csv("TREE_DATA_with_WBS_test1s.csv", na = "", fileEncoding = "UTF-8")
TREE_DATA_with_WBS$test2 %>% write.csv("TREE_DATA_with_WBS_test2.csv", na = "", fileEncoding = "UTF-8")
TREE_DATA_with_WBS$test2s %>% write.csv("TREE_DATA_with_WBS_test2s.csv", na = "", fileEncoding = "UTF-8")


# ============================================fit Grandient Boosting classifier=====================================================
#  Grandient Boosting Modeling
set.seed(369)
Model_GB <-  gbm(formula = ロスコン案件マーク ~ ., 
                 distribution = "bernoulli", 
                 data = NEC_train %>% mutate(ロスコン案件マーク = ifelse(ロスコン案件マーク == "1", 1, 0)),
                 n.trees = 5000,
                 cv.folds = 10)

# ============================================calculate score=====================================================
Model_GB_prob <- c() # vector of GB model socre
for(test in test_list){
  # calculate losscon score(probability) based on Gradient Boosting Model
  Model_GB_prob[[test]] <- predict(object = Model_GB,
                                   newdata = TEST_SET[[test]] %>% mutate(ロスコン案件マーク = ifelse(ロスコン案件マーク == "1", 1, 0)),
                                   n.trees = 1000,
                                   type = "response")
}


# ============================================add rank=====================================================
TEST_4Q_GB_SCORE <- list() # list of test 4q socre table
for(test in test_list){
  TEST_4Q_GB_SCORE[[test]] <- TREE_DATA_with_WBS[[test]] %>%
    # NaRV.omit() %>% 
    cbind(GB_score = Model_GB_prob[[test]]) %>% 
    mutate(rank_base = 1) %>% 
    group_by(計上年月) %>% 
    arrange(desc(GB_score)) %>% 
    mutate(GB_rank = cumsum(rank_base)) %>% 
    ungroup(計上年月) %>% 
    arrange(WBS.要素, 計上年月)
}

# filter the specific Year-Month dataset
filter_YM <- c(test1 = 201812, test1s = 201812, test2 = 201903, test2s = 201903)
WBS_GB_SCORE <- list()
for(test in test_list){
  WBS_GB_SCORE[[test]] <- TEST_4Q_GB_SCORE[[test]] %>% 
    filter(計上年月 == filter_YM[[test]]) %>% 
    select(WBS.要素, GB_score, GB_rank) %>% 
    arrange(GB_rank)
}

# ============================================write output=====================================================
# for GB model
WBS_GB_SCORE_xlsx <- createWorkbook()

for(test in test_list){
  addWorksheet(WBS_GB_SCORE_xlsx, test)
  writeData(WBS_GB_SCORE_xlsx, sheet = test, x = WBS_GB_SCORE[[test]])
}

saveWorkbook(WBS_GB_SCORE_xlsx, "./002_LossCon/output/Gradient Boosting Models' Score and Rank.xlsx")
