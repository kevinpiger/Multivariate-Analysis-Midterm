data <- read.csv("~/Desktop/多變量mid/EU_Jobs.csv", row.names = "Country")
head(data)
X <- data[,1:5] 
Y <- data[,6:9]
# 標準化的相關係數
cxy <- cancor(scale(X,scale = T, center = T),scale(Y,scale = T, center = T))
cxy
# 檢測要用幾個CCA變數
# 檢測之前先檢測資料是否為常態
# 為了之後CCA的檢定（相關係數檢定)
library(MVN)
# Mardia multinormal test
mvn(data, mvnTest = c("mardia"),multivariatePlot = "qq"
    ,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# Henze-Zirkler multinormal test
mvn(data, mvnTest = c("hz"),multivariatePlot = "qq"
    ,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# Royston multinormal test
mvn(data, mvnTest = c("royston"),multivariatePlot = "qq"
,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# Dorni-Haansen's multinormal test
mvn(data, mvnTest = c("dh"),multivariatePlot = "qq"
    ,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# 有過也有不過＠＠，移除outlier試看看
# 移除 outlier
# 8個potential outliers 
result<-mvn(data,mvnTest = c("hz"), desc = FALSE, 
            multivariateOutlierMethod = "adj", showOutliers = TRUE, showNewData = TRUE)
newdata <- result$newData

# Mardia multinormal test
mvn(newdata, mvnTest = c("mardia"),multivariatePlot = "qq"
    ,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# Henze-Zirkler multinormal test
mvn(newdata, mvnTest = c("hz"),multivariatePlot = "qq"
    ,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# Royston multinormal test
mvn(newdata, mvnTest = c("royston"),multivariatePlot = "qq"
    ,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# Dorni-Haansen's multinormal test
mvn(newdata, mvnTest = c("dh"),multivariatePlot = "qq"
    ,multivariateOutlierMethod = "adj",showOutliers = TRUE)

# 用刪減過的資料做看看
library(CCP)
nX <- newdata[,1:5]
nY <- newdata[,6:9]
ncxy <- cancor(scale(nX,scale = T, center = T),scale(nY,scale = T, center = T))
rho <- ncxy$cor

# 用顯著水準為0.05看
# 檢定要用幾個Canonical variates
# H0: L1=L2=L3=L4=0, H1: L1!=0,L2=L3=L4=0
# H0:  L1!=0,L2=L3=L4=0, H1: L1!=0,L2!=0, L3=L4=0
# H0: L1!=0,L2!=0, L3=L4=0, H1: L1!=0, L2!= 0, L3!=0, L4=0
# H0: H1: L1!=0, L2!= 0, L3!=0, L4=0, H1: all are not 0
p.asym(rho,18,5,4,tstat = "Wilks")

ncxy

# 繪圖(篩檢過資料的)
xx<-scale(nX,scale=T,center=T) 
yy<-scale(nY,scale=T,center=T) 
cor(xx,yy)
scorex<-xx%*%ncxy$xcoef[,1] 
scorey<-yy%*%ncxy$ycoef[,1] 
plot(scorex,scorey,type="n")
text(scorex,scorey,row.names(newdata),cex=.6)

library(ggplot2)
temp <- data.frame(scorex,scorey)
tmp <- ggplot(temp)
tmp + geom_point(mapping = aes(x = scorex, y = scorey))+
  geom_text(mapping = aes(x = scorex, y = scorey+0.015,label = rownames(temp)))

# 用未刪減過的資料做看看
X <- data[,1:5]
Y <- data[,6:9]
cxy <- cancor(scale(X,scale = T, center = T),scale(Y,scale = T, center = T))
rho <- cxy$cor

# 用顯著水準為0.05看
# 檢定要用幾個Canonical variates
# H0: L1=L2=L3=L4=0, H1: L1!=0,L2=L3=L4=0
# H0:  L1!=0,L2=L3=L4=0, H1: L1!=0,L2!=0, L3=L4=0
# H0: L1!=0,L2!=0, L3=L4=0, H1: L1!=0, L2!= 0, L3!=0, L4=0
# H0: H1: L1!=0, L2!= 0, L3!=0, L4=0, H1: all are not 0

p.asym(rho,26,5,4,tstat = "Wilks")
# 只有第一個拒絕H0

# 
# 繪圖(未篩檢過資料的)
xx<-scale(X,scale=T,center=T) 
yy<-scale(Y,scale=T,center=T) 
cor(xx,yy)
scorex<-xx%*%cxy$xcoef[,1] 
scorey<-yy%*%cxy$ycoef[,1] 
plot(scorex,scorey,type="n")
text(scorex,scorey,row.names(data),cex=.6)


temp <- data.frame(scorex,scorey)
tmp <- ggplot(temp)
tmp + geom_point(mapping = aes(x = scorex, y = scorey))+
  geom_text(mapping = aes(x = scorex, y = scorey+0.015,label = rownames(temp)))
