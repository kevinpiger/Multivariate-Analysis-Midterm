library(readr)
glass <- read_csv("Glass_data.csv")
head(glass)
set.seed(15) # 3 + 12
tmp <- sample(1:114, 100, replace = F)
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
    shadow.col="gray",
    extra=2)# 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
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
                          parms=list(split='information'),control=glass.control) 
printcp(newglass.treenewcv)

# 用Test data 去分類
newtest.tpred<-predict(newglass.treenewcv,glass_test)
temp <- NULL
for( i in 1:length(glass_test[,1])){
temp[i] <- which(newtest.tpred[i,] == max(newtest.tpred[i,]))}

# True error rate
1 - sum(temp == glass_test$Type)/100


# Linear Discriminant Analysis --------------------------------------------
library(MASS)
glass.lda<-lda(Type~.,data=glass_train)
newglass.ldapred<-predict(glass.lda,glass_train[,-10])
table(glass_train$Type,newglass.ldapred$class)
newglass.ldacv<-lda(Type~.,data=glass_train,CV=T)  
table(glass_train$Type,newglass.ldacv$class)

newglass.ldatest<-predict(glass.lda,glass_test) 
newglass.ldatest$class
sum(newglass.ldatest$class %>% as.numeric == glass_test$Type)/length(newglass.ldatest$class)

# Nearest Neighbor Methdos ------------------------------------------------
# 未完成
library(class)
# k = 3
newglass.knn<-knn(glass_train[,1:9],glass_train[,1:9],glass_train[,10],k=3,prob=T) 
table(glass_train$Type,newglass.knn)
# Apperant error rate
sum(newglass.knn == glass_train$Type)/length(glass_train$Type)
# k = 2
newglass.knn<-knn(glass_train[,1:9],glass_train[,1:9],glass_train[,10],k=2,prob=T) 
table(glass_train$Type,newglass.knn)
# Apperant error rate
sum(newglass.knn == glass_train$Type)/length(glass_train$Type)
# k = 1
newglass.knn<-knn(glass_train[,1:9],glass_train[,1:9],glass_train[,10],k=1,prob=T) 
table(glass_train$Type,newglass.knn)
# Apperant error rate
sum(newglass.knn == glass_train$Type)/length(glass_train$Type)

newglass.knncv<-knn.cv(glass_train[,1:9],glass_train[,10],k=1,prob=T) 
table(glass_train$Type,newglass.knncv)
sum(newglass.knncv == glass_train$Type)/length(glass_train$Type)


newfish1.test<-newfish.test[,c(1,2,5,7,8)]
newfish.knntest<-knn(newfish1[,2:6],newfish1.test,newfish1[,"Species"],k=1,prob=T) 
newfish.knntest