require(caret)
require(caretEnsemble)
require(pROC)
require(doParallel)

require(glmnet)

source("script/R/fun/tools.R")
result.glmnet.df <- readRDS("result/result.glmnet.df.data")

#
# 前処理
#
source("script/R/fun/Data-pre-processing.R")

my_preProcess <- c("center", "scale")

data_preProcess <- "none"
data_preProcess <- "nzv"
data_preProcess <- "dummy"
data_preProcess <- "dummy.nzv.highlyCorDescr"

if ( data_preProcess == "none") {
  TRAIN <- all.train
  TRAIN.TRAIN <- train.train
  TRAIN.TEST <- train.test
  TEST <- test
} else if ( data_preProcess == "nzv") {
  TRAIN <- all.nzv.train
  TRAIN.TRAIN <- train.nzv.train
  TRAIN.TEST <- train.nzv.test
  TEST <- test
} else if ( data_preProcess == "dummy") {
  TRAIN <- train.dummy
  TRAIN.TRAIN <- train.dummy.train
  TRAIN.TEST <- train.dummy.test
  TEST <- test.dummy
} else if ( data_preProcess == "dummy.nzv.highlyCorDescr") {
  TRAIN <- train.dummy.nzv.highlyCorDescr
  TRAIN.TRAIN <- train.dummy.nzv.highlyCorDescr.train
  TRAIN.TEST <- train.dummy.nzv.highlyCorDescr.test
  TEST <- test.dummy.nzv.highlyCorDescr
}


#
# glmnet
#

# seeds の決定
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 500)
seeds[[51]] <- sample.int(1000, 1)

my_control <- trainControl(
  method = "cv"
  ,number = 10
  ,summaryFunction = multiClassSummary
  ,classProbs = TRUE
  ,verbose = TRUE
  ,savePredictions = "final"
  ,index = createResample(TRAIN.TRAIN$response, 10)
  ,seeds = seeds
)

doParallel <- trainControl(
  method = "cv"
  ,number = 10
  ,summaryFunction = multiClassSummary
  ,classProbs = TRUE
  ,allowParallel = TRUE
  ,verboseIter = TRUE
  ,savePredictions = "final"
  ,index = createResample(TRAIN.TRAIN$response, 10)
  ,seeds = seeds
)


# 説明変数一覧の作成
explanation_variable <- names(subset(TRAIN, select = -c(before, after, nchar, response)))

cl <- makeCluster(detectCores(), type = 'PSOCK', outfile = " ")
registerDoParallel(cl)

model_list <- caretList(
  x = TRAIN.TRAIN[,explanation_variable]
  ,y = TRAIN.TRAIN$response
  #,trControl = my_control
  ,trControl = doParallel
  #,preProcess = my_preProcess
  ,tuneList = list(
    glmnet = caretModelSpec(
      method = "glmnet"
      #,metric = "logLoss"
      ,family = "multinomial"
      ,tuneGrid = expand.grid(
                              # alpha = 1:5 * 0.1
                              # ,lambda = 10^{1:5 * -1}
                              alpha = 1 * 0.01
                              ,lambda = 1 * 0.01
                             )
    )
  )
)

stopCluster(cl)
registerDoSEQ()

model_list[[1]]$times
model_list[[1]]$finalModel

# Solution Path
plot(model_list[[1]]$finalModel, label = TRUE)
plot(model_list[[1]]$finalModel, label = TRUE, xvar = "lambda")

varImp(model_list[[1]], scale = FALSE, useModel = FALSE)
varImp(model_list[[1]], scale = FALSE)
plot(varImp(model_list[[1]], scale = FALSE))

ggplot(model_list[[1]]) 

#
# モデル比較
#
allProb <- caret::extractProb(
                              list(model_list[[1]])
                              ,testX = subset(TRAIN.TEST, select = -c(response,datetime))
                              ,testY = unlist(subset(TRAIN.TEST, select = c(response)))
                             )

# dataType 列に Test と入っているもののみを抜き出す
testProb <- subset(allProb, dataType == "Test")
tp <- subset(testProb, object == "Object1")

# 精度確認
y_pred  <- dplyr::select(tp, c(good, human, mechanical, weather)) %>%
  as.matrix(.)

MLmetrics::MultiLogLoss(y_true = tp$obs, y_pred = y_pred)

# 結果の保存
result.glmnet.df <- rbind(result.glmnet.df, summaryResult.LogLoss(model_list[[1]]))
saveRDS(result.glmnet.df, "result/result.glmnet.df.data")


# predict() を利用した検算 
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test.verification <- predict(
    model_list[[1]]$finalModel
    ,newx = Matrix(as.matrix(subset(TRAIN.TEST, select = -c(response, datetime))), sparse = TRUE )
    ,s = model_list[[1]]$bestTune$lambda
    ,type = "response"
  )
} else {
  # preProcess を指定している場合
  pred_test.verification <- preProcess(
    subset(TRAIN.TEST, select = -c(response, datetime))
    ,method = my_preProcess
  ) %>%
    predict(., subset(TRAIN.TEST, select = -c(response, datetime))) %>%
    as.matrix(.) %>%
    Matrix(., sparse = TRUE ) %>%
    predict(
      model_list[[1]]$finalModel
      ,newx = .
      ,s = model_list[[1]]$bestTune$lambda
      ,type = "response"
    )
}

MLmetrics::MultiLogLoss(y_true = TRAIN.TEST$response, y_pred = pred_test.verification)



#
# データにモデルの当てはめ
#
if (is.null(model_list[[1]]$preProcess)){
  # preProcess を指定していない場合
  pred_test <- predict(
    model_list[[1]]$finalModel
    ,newx = Matrix(as.matrix(subset(TEST, select = -c(datetime))), sparse = TRUE )
    ,s = model_list[[1]]$bestTune$lambda
    ,type = "response"
  )
  
  PREPROCESS <- "no_preProcess"
} else {
  # preProcess を指定している場合
  pred_test <- preProcess(
      TEST
      ,method = my_preProcess
    ) %>%
      predict(., subset(TEST, select = -c(datetime))) %>%
      as.matrix(.) %>%
      Matrix(., sparse = TRUE ) %>%
      predict(
        model_list[[1]]$finalModel
        ,newx = .
        ,s = model_list[[1]]$bestTune$lambda
        ,type = "response"
      )
  
  PREPROCESS <- paste(my_preProcess, collapse = "_")
}

#submitの形式で出力(CSV)
#データ加工
out <- data.frame(TEST$datetime, pred_test)

sapply(out, function(x) sum(is.na(x)))


# 予測データを保存
for(NUM in 1:10){
  DATE <- format(jrvFinance::edate(from = Sys.Date(), 0), "%Y%m%d")
  SUBMIT_FILENAME <- paste("./submit/submit_", DATE, "_", NUM, "_", PREPROCESS, "_glmnet.csv", sep = "")
  
  if ( !file.exists(SUBMIT_FILENAME) ) {
    write.table(out, #出力データ
                SUBMIT_FILENAME, #出力先
                quote = FALSE, #文字列を「"」で囲む有無
                col.names = FALSE, #変数名(列名)の有無
                row.names = FALSE, #行番号の有無
                sep = "," #区切り文字の指定
    )
    break
  }
}
