library(dplyr)
library(magrittr)
glass <- read.csv("~/Desktop/多變量mid/Glass_data.csv")
glass$Type %<>% as.factor()
str(glass)
head(glass)
tmp <- sample(c(1:length(glass[,1])), 100, replace = F)
glass_train <- glass[tmp,]
glass_test <- glass[-tmp,]

# Classified Tree ---------------------------------------------------------

library(rpart)
library(rpart.plot )
glass.control<-rpart.control(minisplit=10,minbucket=3,xval=0)
glass.treeorig<-rpart(Type~., data=glass_train,method="class",control=glass.control)
plot(glass.treeorig)
text(glass.treeorig)
prp(glass.treeorig,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2) 
printcp(glass.treeorig)
summary(glass.treeorig)
glass.prunetree<-prune.rpart(glass.treeorig,cp=0.02) 
# 第一種畫法
plot(glass.prunetree)
text(glass.prunetree)
prp(glass.prunetree,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)
# 看是否有相關係數>0.7的部分
cor(glass_train[,-10])

newglass.treenew<-rpart(Type~., data=glass_train,method="class",parms=list(split="information"),control=glass.control) 
printcp(newglass.treenew)

plot(newglass.treenew) 
text(newglass.treenew)
prp(newglass.treenew,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2) 
glass.control<-rpart.control(minbucket=3,minsplit=10,xval=100)
newglass.treenewcv<- rpart(Type~., data=glass_train,method="class",
                           parms=list(split='gini'),control=glass.control) 
printcp(newglass.treenewcv)

# 用Test data 去分類
newtest.tpred<-predict(newglass.treenewcv,glass_test) 
newtest.tpred
newtest.tpred1<-predict(newglass.treenewcv,glass_test,type = "class")
a <- table(newtest.tpred1,glass_test$Type)
temp <- NULL
#for( i in 1:length(glass_test[,1])){
  #temp[i] <- which(newtest.tpred[i,] == max(newtest.tpred[i,]))}

for( i in 1:length(glass_test[,1])){
  temp[i] <- which.max(newtest.tpred[i,])}

# True error rate
1 - sum(diag(a))/length(glass_test[,1])


# Random forest -----------------------------------------------------------

library(randomForest)
A <- randomForest(Type~., importance=TRUE, nodesize=3, proximity=TRUE, data=glass_train)
print(A)
B <-randomForest(Type~., importance=TRUE, mtry=6, nodesize=3, proximity=TRUE, data=glass_train)
print(B)
