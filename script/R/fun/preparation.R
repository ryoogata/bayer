require(caret)
require(dplyr)
require(data.table)
require(qdapTools)

# 指数表示の回避
options(scipen=10)

#
# データの読み込み
#
train <- data.table::fread("./data/train.tsv", stringsAsFactors = FALSE, sep = "\t", data.table = FALSE)
test <- data.table::fread("./data/test.tsv", stringsAsFactors = FALSE, sep = "\t", data.table = FALSE, header = FALSE)

train$nchar <- nchar(train[,1])
test$nchar <- nchar(test[,1])

# 列:before の文字列を 1 文字づつ分割して列を作成
# onechars <- apply(data.frame(train[,"before"]), 1, onestr) %>%
#  t(.)

# 分割した文字列の data.frame を保存
# 作成に時間がかかるので
# write.csv(onechars,"./data/onechars.csv", quote = FALSE, row.names = FALSE)

onechars <- data.table::fread("./data/onechars.csv", stringsAsFactors = FALSE, data.table = FALSE)

train.modify <- cbind(train, onechars)

NROW(unique(sort(train[,"after"])))
# [1] 4217

response <- unique(sort(train[,"after"])) %>%
  data.frame(after = .)

response$id <- rownames(response)
names(response)[2] <- "response"
response <- response[,2:1]

train.modify$response <-  qdapTools::lookup(train.modify$after, response[,2:1])

write.table(train.modify, "data/train.modify.tsv", quote = FALSE, sep = "\t", row.names = FALSE)

