#第一次建模預計使用最基本的 Regression analysis
#Multiple regression for 再犯次數(Count)
#Logistic Regression for 高危機個案(Binary)


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
   

