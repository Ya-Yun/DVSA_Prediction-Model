#第二次建模：Multiple regression for 再犯次數(Count)
#由於上次發現Count=1及=2的單數過多，造成預測分數低估，因此重新分組並resampling後再跑一次回歸
#Resampling group 1, 2, 3+ 各200筆 and Resampling group (1, 2), 3+ 各200筆

library(dplyr)
library(sqldf)
library(ggthemes)
library(ggplot2)
library(plotly)

DVASdata <- read.csv("imputed_age_DVAS.csv")
#Assign to new group
DVASdata$Count_plus <- cut(DVASdata$Count, breaks=c(1,2,3,Inf), right = F, labels = c(1,2,3))
#Resampling
set.seed(123)
dat1 <- DVASdata[which(DVASdata$Count_plus=="1") %>% sample(., 200, replace=TRUE),]
dat2 <- DVASdata[which(DVASdata$Count_plus=="2") %>% sample(., 200, replace=TRUE),]
dat3 <- DVASdata[which(DVASdata$Count_plus=="3") %>% sample(., 200, replace=TRUE),]
out <- rbind(dat1, dat2, dat3)

#Built up a linear regresson model
model1 <- glm(formula = Count_plus ~ . - ACTIONID - Count - X1.4.5.6 - 高危機.死亡 
    - lng - lat - district - town - 施暴武器說明 - Count_plus, family = gaussian(link = "identity"), 
    data = out, na.action = na.exclude)
summary(model1)

   #以有顯著之項目再跑一個model
model4<-glm(formula = Count ~ X2 + X8 + 家暴因素.個性.生活習慣不合 + 
    家暴因素.疑似或罹患精神疾病 + 暴力型態.精神暴力 + 
    MAIMED + OCCUPATION, family = gaussian(link = "identity"), 
    data = DVASdata, na.action = na.exclude)

#寫出預測分數
y1 <- predict.glm(model4,type = "response")
#將預測分數貼回原表單，並觀察預測效果
DVASdata$Predicty1 <- unlist(y1)
plot(DVASdata$Count,DVASdata$Predicty1)
