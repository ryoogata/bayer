require(caret)
require(dplyr)
require(data.table)
require(qdapTools)

# 指数表示の回避
options(scipen=10)

#
# データの読み込み
#
all.train <- data.table::fread("./data/train.modify.tsv", stringsAsFactors = FALSE, sep = "\t", data.table = FALSE)
test <- data.table::fread("./data/test.tsv", stringsAsFactors = FALSE, sep = "\t", data.table = FALSE, header = FALSE)

all.train$response <- paste0("L", all.train$response)

# 目的変数の factor 化
all.train$response <- as.factor(all.train$response)

# 列名の修正
# names(all.train)[4:83] <- paste0("c",1:80)

#
# Dummy 変数なし
#

# 再現性のため乱数シードを固定
set.seed(10)

# 訓練データと検証データに分割する
# Train 用の列番号を作成
inTrain <- caret::createDataPartition(all.train$response, p = .8, list = FALSE)
train.train <- all.train[inTrain,]
train.test <- all.train[-inTrain,]

dim(train.train)
# [1] 11110    84
train.train$response %>% table

dim(train.test)
# [1] 1115   84




