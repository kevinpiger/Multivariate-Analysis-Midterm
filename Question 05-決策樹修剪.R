# Question05 決策樹修剪

# 決策樹 
library(rpart)
model <- rpart(formula = Wage~.,
               data = Wage_train)
model
# 畫圖
library(rpart.plot)
rpart.plot(model,faclen = 0,fallen.leaves = TRUE, shadow.col = gray)

require(partykit)   
rparty.tree <- as.party(model) # 轉換cart決策樹
rparty.tree # 輸出各節點的細部資訊
plot(rparty.tree) 

# 預測
pred <- predict(model, newdata=Wage_test, type="class")

# 用table看預測的情況
table(real=Wage_test$Wage, predict=pred)

# 計算預測準確率 = 對角線的數量/總數量
confus.matrix <- table(real=Wage_test$Wage, predict=pred)
sum(diag(confus.matrix))/sum(confus.matrix) # 對角線的數量/總數量

printcp(model) # 先觀察未修剪的樹，CP欄位代表樹的成本複雜度參數
plotcp(model)  # 畫圖觀察未修剪的樹
# 利用能使決策樹具有最小誤差的CP來修剪樹
prunetree.model <- prune(model, cp = model$cptable[which.min(model$cptable[,"xerror"]),"CP"]) 

# 修剪過後的樹
prunetree_pred <- predict(prunetree.model, newdata=Wage_test, type="class")

# 用table看預測的情況
table(real=Wage_test$Wage, predict=prunetree_pred)

# 準確率
prunetree_confus.matrix <- table(real=Wage_test$Wage, predict=prunetree_pred)
sum(diag(prunetree_confus.matrix))/sum(prunetree_confus.matrix) # 對角線的數量/總數量

