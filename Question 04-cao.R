
# Question 04 -------------------------------------------------------------
library(readr)
library(dplyr)
library(magrittr)


# Random Forest -----------------------------------------------------------

#讀取資料
data <- read_csv("Glass_data.csv")
data %<>% as.data.frame()
data[,10] %<>% as.factor()
num <- sample(1:114,size = 92,replace = F)
traindata <- data[num,]
testdata <- data[-num,]

# 載入隨機樹森林package
library(randomForest)
# 決定多少個節點
tuneRF(traindata[,-10], traindata[,10])
# 跑隨機樹森林模型(節點數=根號(變數個數))
randomforestM <- randomForest(formula = Type ~ .,
                              data = traindata,
                              nodesize = 4,
                              importane = T, 
                              proximity = T, 
                              ntree = 500)
randomforestM
# 跑隨機樹森林模型(變數改為6個)
randomforestM <- randomForest(formula = Type ~ .,
                              data = traindata,
                              mtry = 6,
                              importane = T, 
                              proximity = T, 
                              ntree = 500)
randomforestM
randomforestM$votes
randomforestM$predicted
# leave-one-out
lou <- rfcv(traindata[,-10], traindata[,10], step=0.7)
lou
which(lou$error.cv == min(lou$error.cv))
# In this case, the minimum true error rate is about 0.30 when 4 predictors are used to split the node
# 混淆
C <- table(randomforestM$predicted,traindata$Type)
# apperant error rate
1-sum(diag(C))/nrow(traindata)
# 森林中每棵樹的節點個數
hist(treesize(randomforestM, terminal=TRUE))
# 錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(randomforestM)
# 衡量每一個變數對Y值的重要性，取到小數點第二位
round(importance(randomforestM), 2)
# 分對分錯
clusters <- pam(1-randomforestM$proximity, k=6, diss = TRUE)
table(clusters$clustering,traindata$Type)
# 畫圖
# 群集分析(投影在二維)
mds <- MDSplot(rf = randomforestM,
               fac = traindata$Type,
               k = 2, 
               pch = 16)


# Boosting ----------------------------------------------------------------

#install.packages("adabag")
library(adabag)
#glass.control<-rpart.control(minbucket=3,minsplit=10,xval=100)
traindata.boost <- boosting(Type~.,data=traindata,boos = F,mfinal=10 ) 
traindata.pred <- predict.boosting(traindata.boost,testdata)
# 混淆矩陣
traindata.pred$confusion
# 錯誤率
C <- traindata.pred$confusion
1-sum(diag(C))/nrow(testdata)
#
library(useful)
# 形容模型的公式
# 因為是建立樹模型，我們不需要截距項
Formula <- Type~refractive+sodium+Magnesium+Aluminum+Silicon+Potassium+Calcium+Barium+Iron-1
# 因為是建立樹模型，我們使用所有level的類別變數
traindata$Type %<>% as.numeric() %>% -1 %<>% as.factor()
glassX <- build.x(Formula,data = traindata,contrasts = F)
glassY <- build.y(Formula,data = traindata) 
num_class <- 7 
# xgboost
library(xgboost)
# xgboost 參數設定 (xgboost parameters setup)
param = list("objective" = "multi:softmax", # 訓練目標
             "eval_metric" = "merror",      # 多分類錯分率 (多分類對數損失用mlogloss)
             "num_class" = num_class
)
glassboost <- xgboost(data = glassX,
                      label = glassY,
                      nrounds = 20,
                      params = param)
glassboost
xgb.plot.multi.trees(glassboost,feature_names = colnames(glassX))
# 重要變數
xgb.plot.importance(xgb.importance(glassboost,
                                   feature_names = colnames(glassX)))
# 使用traindata建立的提升樹
testdata$Type %<>% as.numeric()%>% -1 %<>% as.factor()
library(Matrix)
xdata = sparse.model.matrix(Type ~ .-1, data = traindata)
# 計算預測值 (get prediction)
Ypred <- predict(glassboost,xdata)
Ypred <- t(matrix(Ypred,num_class ,length(Ypred)/num_class ))
# colnames(Ypred) = levels(iris2$Species)
Ypred = levels(traindata$Type)[max.col(Ypred)]
Ypred = factor(Ypred,levels=levels(traindata$Type))
# 混淆矩陣 (confusion matrix)
t0 = table(traindata$Type,Ypred)
t0


