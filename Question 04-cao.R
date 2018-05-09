
# Question 04 
library(readr)
library(dplyr)
library(magrittr)
# Random Forest -----------------------------------------------------------
control<-rpart.control(minbucket=3,minsplit=10,xval=114)
#讀取資料
data <- read_csv("Glass_data.csv")
data %<>% as.data.frame()
data[,10] %<>% as.factor()
num <- sample(1:114,size = 92,replace = F)
traindata <- data[num,]
testdata <- data[-num,]
table(data$Type)
# 載入隨機樹森林package
library(randomForest)
randomForest(x = data[,-10], y = data$Type, ntree = 500, proximity = T,      data = data, importane = T) 
# 決定多少個節點
tuneRF(data[,-10], data[,10])
# 跑隨機樹森林模型(節點數=根號(變數個數))
model <- rfcv(trainx = data[,-10],
              trainy = data$Type,
              data = data,
              importane = T, 
              proximity = T,
              nodesize = 3,
              step = 0.9,
              ntree = 500,
              cv.fold = 5)
model$importance
# 跑隨機樹森林模型(變數改為9個)
model <- randomForest(formula = Type ~ .,
                      data = traindata,
                      mtry = 9,
                      nodesize=1,
                      importane = T, 
                      proximity = T, 
                      ntree = 500,cv.fold = 114)
model
model$votes
model$predicted
# leave-one-out
lou <- rfcv(traindata[,-10], traindata[,10], step=0.7)
lou$error.cv
which(lou$error.cv == min(lou$error.cv))
# In this case, the minimum true error rate is about 0.30 when 6 predictors are used to split the node
# 混淆
C <- table(model$predicted,traindata$Type) ; C
# apparent error rate (測試樣本的錯誤率)
1-sum(diag(C))/nrow(traindata)
# 森林中每棵樹的節點個數
hist(treesize(model, terminal=TRUE))
# 錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(model)
# 衡量每一個變數對Y值的重要性，取到小數點第二位
round(importance(model), 2)
# 分對分錯
library(cluster)
clusters <- pam(1-model$proximity, k=6, diss = TRUE)
table(clusters$clustering,traindata$Type)
# 畫圖
# 群集分析(投影在二維)
mds <- MDSplot(rf = model,
               fac = data$Type,
               k = 2, 
               pch = 1)
mds$GOF
mds$points
# Boosting ----------------------------------------------------------------

#install.packages("adabag")
library(adabag)
#glass.control<-rpart.control(minbucket=3,minsplit=10,xval=100)
traindata.boost <- boosting(Type~.,
                            data=traindata,
                            boos = T,
                            mfinal=10 ) 
traindata.boost$importance
test <- predict.boosting(traindata.boost,traindata)
test$confusion
traindata.pred <- predict.boosting(traindata.boost,testdata)
# 混淆矩陣
traindata.pred$confusion
# 真實錯誤率
C <- traindata.pred$confusion
1-sum(diag(C))/nrow(testdata)


# Boosting.cv -------------------------------------------------------------
boostingmodel <- boosting.cv(Type~.,
                             data = data,
                             v = 114,
                             boos = T,
                             mfinal = 10)
boostingmodel

# xgboost -----------------------------------------------------------------
# xgboost

library(useful)
# 形容模型的公式
# 因為是建立樹模型，我們不需要截距項
Formula <- Type~refractive+sodium+Magnesium+Aluminum+Silicon+Potassium+Calcium+Barium+Iron-1
# 因為是建立樹模型，我們使用所有level的類別變數
glassX <- build.x(Formula,data = data,contrasts = F)
glassY <- build.y(Formula,data = data) 
num_class <- 7 
library(xgboost)
# xgboost 參數設定 (xgboost parameters setup)
param = list("objective" = "multi:softmax", # 訓練目標
             "eval_metric" = "merror",      # 多分類錯分率 (多分類對數損失用mlogloss)
             "num_class" = num_class
)
glassboost <- xgboost(data = glassX,
                      label = glassY,
                      nrounds = 20,
                      params = param,
                      nfold = 114,
                      boos = T,
                      control=)
glassboost
xgb.plot.multi.trees(glassboost,feature_names = colnames(glassX))
# 重要變數
xgb.plot.importance(xgb.importance(glassboost,
                                   feature_names = colnames(glassX)))

xdata <- build.x(Formula,data = testdata,contrasts = FALSE)
# 計算預測值 (get prediction)
Ypred <- predict(glassboost,xdata)
# 混淆矩陣 (confusion matrix)
C <- table(testdata$Type,Ypred);C
# 真實錯誤率
1-sum(diag(C))/sum(C)


