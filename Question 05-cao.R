# Question 05

# 隨機森林 --------------------------------------------------------------------

# 讀取資料(訓練資料及測試資料)
library(readr)
library(tidyverse)
library(magrittr)
library(readr)
Wage_data <- read_csv("Wage_data.csv", 
                      col_types = cols(Age = col_factor(levels = c("A1","A2", "A3")),
                                       Education = col_factor(levels = c("ED1", "ED2", "ED3", "ED4")),
                                       Experience = col_factor(levels = c("EX1","EX2", "EX3")),
                                       Gender = col_factor(levels = c("G0", "G1")), 
                                       Mstat = col_factor(levels = c("M0", "M1")), 
                                       Occupation = col_factor(levels = c("O1", "O2", "O3", "O4", "O5", "O6")), 
                                       Race = col_factor(levels = c("R1", "R2", "R3")), 
                                       Sector = col_factor(levels = c("SE0", "SE1", "SE2")), 
                                       South = col_factor(levels = c("S0", "S1")), 
                                       Union = col_factor(levels = c("U0", "U1")),
                                       Wage = col_factor(levels = c("A", "B", "C"))))
head(Wage_data)
num <- sample(1:nrow(Wage_data),ceiling(nrow(Wage_data)*0.8),replace = F)
Wage_train <- Wage_data[num,]
Wage_test <- Wage_data[-num,]  
table(Wage_data$Wage)

# 載入隨機樹森林package
#install.packages("randomForest")
library(randomForest)

# 跑隨機樹森林模型
model <- randomForest(formula = Wage~. ,
                      data = Wage_data,
                      importane = T, 
                      proximity = T,
                      v = 534,
                      ntree = 500)
model
model$importance
#錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(model)
C <- table(Wage_train$Wage,model$predicted)
1-sum(diag(C))/sum(C) # 錯誤率很高
#真實錯誤率
pred1 <- predict(model,newdata=Wage_test)
table(pred1)
table(Wage_test$Wage,pred1)
1-sum(diag(C))/sum(C) # 錯誤率很高

# 隨機森林交叉驗證
rfmodel <- rfcv(trainx = Wage_data[,-1] ,
                trainy = Wage_data$Wage,
                data = Wage_data,
                importane = T, 
                proximity = T,
                v = 534,
                ntree = 500)
rfmodel$error.cv

rfmodel2 <- randomForest(Wage~.,
                         data = Wage_data,mtry=6,
                         importane = T, 
                         proximity = T,
                         ntree = 500)
rfmodel2$importance

# 羅吉斯迴歸 -------------------------------------------------------------------

library(nnet)
model2 <- multinom(Wage~., data = Wage_train, maxit = 250)
summary(model2)
pred <- predict(model2, Wage_train)
c <- table(Wage_train$Wage,pred);c

# 錯誤率(訓練資料集驗證結果)
1-sum(diag(c))/sum(c)

# 真實錯誤率
pred <- predict(model2, newdata = Wage_test)
c <- table(Wage_test$Wage,pred)
1-sum(diag(c))/sum(c)

# 羅吉斯迴歸-交叉驗證 --------------------------------------------------------------

# 將資料隨機分成十等分  
# install.packages("caret")  
# 固定folds函数的分组  
set.seed(15)  
require(caret)  
folds <- createFolds(y=Wage_data$Wage,k=534)  
#建構for循還，得10次交叉驗證的測試集精確度、訓練集準確度  

max=0 
max2=0
num=0  
accuracy <- c()
for(i in 1:534){  
  
  fold_test <- Wage_data[folds[[i]],]   #取folds[[i]]作為測試集  
  fold_train <- Wage_data[-folds[[i]],]   # 剩下的資料作為訓練集  
  
  print("***組別***")  
  
  fold_pre <- multinom(Wage~., data = fold_train, maxit = 250)  
  pred <- predict(fold_pre, fold_train[,-1])
  c <- table(fold_train$Wage,pred)
  fold_accuracy <- sum(diag(c))/sum(c)
  print(i)  
  print("***測試集精確度***")  
  print(fold_accuracy)  
  print("***訓練集精確度***")  
  pred2 <- predict(fold_pre,newdata=fold_test[,-1])  
  C <- table(fold_test$Wage,pred2)
  fold_accuracy2<- sum(diag(C))/sum(C)
  print(fold_accuracy2)  
  
  accuracy<-c(accuracy,fold_accuracy2)
}  
print(accuracy)
mean(accuracy)


# 羅吉斯超哥 交叉驗證 --------------------------------------------------------------

install.packages('glmnet')
library(glmnet)
x <- model.matrix( ~ .-1, Wage_data[,-1])
y <- Wage_data$Wage
cvfit <- cv.glmnet(x, y, family="multinomial", type.measure="class", nfolds=534)
summary(cvfit)
predict.value <- predict(cvfit, x, s = "lambda.min", type = "class")
C <- table(predict.value,Wage_data$Wage) 
1-sum(diag(C))/sum(C)

# 提升樹模型 -------------------------------------------------------------------

# Running the boosting with M = 20 steps without bootstrap sampling:
library(adabag)
model3 <- boosting(formula = Wage~.,
                   data = Wage_data,
                   boos = T, 
                   mfinal = 20)
model3
pred <- predict.boosting(model3,Wage_test)
pred$confusion

model4 <- boosting.cv(Wage~.,data = Wage_data,par = TRUE,v = 10 ,mfinal = 10,coeflearn = "Zhu")


# 觀察原始資料 ------------------------------------------------------------------

# 觀看觀察值是否相符

num <- c()
for (i in 1:nrow(Wage_data)){
  n1 <- -1
  for(j in 1:nrow(Wage_data)){
  n <- sum(1*(Wage_data[,-1][i,]==Wage_data[,-1][j,]))
  if(n == 10){n1 <- n1+1}
  }
  num <- c(num,n1)
}
e <- c()
for (i in 1:length(num)) {
  if(num[i] != 0){e <- c(e,i)}
}
equaldata <- Wage_data[e,]

head(equaldata)

d <- c()
num1 <- c(1:nrow(equaldata))

for (i in 1:101){
  d <- rbind(d,equaldata[i,])
  for(j in (i+1):267){
    n <- sum(1*(equaldata[,-1][i,]== equaldata[,-1][j,]))
    if(n == 10){d <- rbind(d,equaldata[j,])}
  }
}
d

# 資料整理過後的決策樹 --------------------------------------------------------------

newdata <- Wage_data[-e,]
newmodel <- randomForest(formula = Wage~. ,
                         data = newdata,
                         mtry = 1,
                         importane = T, 
                         proximity = T,
                         ntree = 500)
newmodel
# Running the boosting with M = 20 steps without bootstrap sampling:
library(adabag)
newmodel3 <- boosting(formula = Wage~.,data = newdata,boos = T,mfinal = 1)
newmodel3

# xgboost -----------------------------------------------------------------

library(useful)
Formula <- Wage~.-1
dataX <- build.x(Formula,data = Wage_train,contrasts = FALSE)
dataY <- build.y(Formula,data = Wage_train)
library(xgboost)
model5 <- xgboost(data = dataX,
                  label = dataY,
                  max.depth = 3,
                  eta = .5, 
                  nthread = 4, 
                  nrounds = 200,
                  objective ="multi:softmax", 
                  num_class=4,
                  eval_metric = "merror")
dataX2 <- build.x(Formula,data = Wage_test,contrasts = FALSE)
pred <- predict(model5 , newdata = dataX2)
C <- table(Wage_test$Wage,pred)

# 真實錯誤率
1-sum(diag(C))/sum(C)

# 做交叉驗證
dataX1 <- build.x(Formula, data = Wage_data,contrasts = FALSE)
dataY1 <- build.y(Formula, data = Wage_data)
model6 <- xgb.cv(data = dataX1,
                 label = dataY1,
                 max.depth = 3,
                 eta = .5, 
                 nthread = 4, 
                 nrounds = 20,
                 objective ="multi:softmax", 
                 num_class=4,
                 eval_metric = "merror",
                 nfold = 5)
model6 #交叉驗證的真實錯誤率也很高

# 留下A,C變數的觀察值 -------------------------------------------------------------
wowdata <- c()
for(i in 1:nrow(Wage_data)){
  if(Wage_data$Wage[i] == "A" || Wage_data$Wage[i] == "C"){wowdata <- c(wowdata,Wage_data[i,])}
}  

