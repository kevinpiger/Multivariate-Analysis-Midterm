options(scipen = 999)
# Question 01 -------------------------------------------------------------
library(corrplot)
temp <- read.csv("~/Desktop/多變量mid/EU_Jobs.csv")
head(temp)
job <- temp[,-1]
rownames(job) <- temp[,1]

# 兩兩比較
pairs(job) 
corrplot(cor(job),tl.srt = 45)
corrplot.mixed(cor(job))

library(stats) 
# 組成分分析 
pca.job<-princomp(scale(job,scale=TRUE,center=TRUE),cor=T ) 
pca.job
# 選擇相關係數矩陣可以把變異程度差距太大的影響消掉
summary(pca.job)
vars <- pca.job$sdev^2
props <- vars / sum(vars)    
cumulative.props <- cumsum(props)  # 累加前n個元素的值
cumulative.props
plot(cumulative.props,ylab = "cum.variance", xlab = "components")
abline(h=0.7, col="blue") 
loadings(pca.job)
print(pca.job$loadings,digits = 4, cutoff = 0) # 顯示空格的資料

# 看eigen
screeplot(pca.job) # 看誰的解釋的變異量多寡
pcs.job <- predict(pca.job) 
eigen <- eigen(cor(job)) 
plot(eigen$values,type="h")
plot(pcs.job[,c(1,2)],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs.job[,c(1,2)],row.names(job)) 

biplot(pca.job,scale=1) 

sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  # run PCA
  pc.out<-princomp(x,cor=cor,...)
  # the proportion of variance of each PC
  pve=(pc.out$sdev^2/m)[1:m]
  # a matrix with R rows and m columns that contains
  # the proportion of variance explained by each pc
  # for each randomization replicate.
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  for(i in 1:R){
    # permutation each column
    x.perm<-apply(x,2,sample)
    # run PCA
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    # the proportion of variance of each PC.perm
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
  }
  # calcalute the p-values
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  return(list(pve=pve,pval=pval))
}

library(RCurl)
sign.pc(job,cor=T)
