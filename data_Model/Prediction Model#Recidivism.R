#第一次建模預計使用最基本的 Regression analysis
#Multiple regression for 再犯次數(Count)

DVASdata <- read.csv("imputed_age_DVAS.csv")

#Multiple regression for 再犯次數(Count)
model3<-glm(formula = Count ~ . - ACTIONID - Count - X1.4.5.6 - 高危機.死亡 - 
    lng - lat - district - town - 施暴武器說明, family = gaussian(link = "identity"), 
    data = DVASdata, na.action = na.exclude)

summary(model3)

   #以有顯著之項目再跑一個model
model4<-glm(formula = Count ~ X2 + X8 + 家暴因素.個性.生活習慣不合 + 
    家暴因素.疑似或罹患精神疾病 + 暴力型態.精神暴力 + 
    MAIMED + OCCUPATION, family = gaussian(link = "identity"), 
    data = DVASdata, na.action = na.exclude)

#寫出預測分數
y1 <- predict.glm(model4,type = "response")
   
#由於Count=1的通報單涵蓋了所有可能性，因此嘗試改以Count=2-7的單子建模
newdata <- subset(DVASdata, Count > 1 )

model6<-glm(formula = Count ~ . - ACTIONID - Count - X1.4.5.6 - 高危機.死亡 - 
    lng - lat - district - town - 施暴武器說明, family = gaussian(link = "identity"), 
    data = newdata, na.action = na.exclude)
summary(model6)

#以有顯著之項目再跑一個model
model7<-glm(formula = Count ~ X1 + X8 + 家暴因素.照顧壓力  + 
    家暴因素.疑似或罹患精神疾病 + 被害人婚姻狀態 + 受暴持續總月數 + 
    MAIMED + OCCUPATION + AGE, family = gaussian(link = "identity"), 
    data = newdata, na.action = na.exclude)
#寫出預測分數
y4 <- predict.glm(model7,type = "response")
#寫出包含Count=1的預測分數
y5 <- predict.lm(model7,DVASdata,type = "response") ##Error in model.frame.default  factor OCCUPATION has new levels 農林漁牧
#由於Count>1的通報單沒有職業為農林漁牧者，因此抽一個農林漁牧給Count>1，再做一次model
farmerdata <- subset(DVASdata, Count > 1 )
farmerdata <- subset(farmerdata, OCCUPATION == "農林漁牧")
newdata2 <- merge(newdata,farmerdata, all=TRUE)

model8<-glm(formula = Count ~ X1 + X8 + 家暴因素.照顧壓力  + 
    家暴因素.疑似或罹患精神疾病 + 被害人婚姻狀態 + 受暴持續總月數 + 
    MAIMED + OCCUPATION + AGE, family = gaussian(link = "identity"), 
    data = newdata2, na.action = na.exclude)
y6 <- predict.glm(model8,type = "response")
y7 <- predict.lm(model8,DVASdata,type = "response")
