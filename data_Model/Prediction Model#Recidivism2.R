#第二次建模：Multiple regression for 再犯次數(Count)
#由於上次發現Count=1及=2的單數過多，造成預測分數低估，因此重新分組並resampling後再跑一次回歸
#Resampling group 1, 2, 3+ 各200筆 and Resampling group (1, 2), 3+ 各200筆

library(dplyr)
library(sqldf)
library(ggthemes)
library(ggplot2)
library(plotly)

DVASdata <- read.csv("imputed_age_DVAS.csv")
#Assign to new 3 groups
DVASdata$Count_plus <- cut(DVASdata$Count, breaks=c(1,2,3,Inf), right = F, labels = c(1,2,3))
#Resampling
set.seed(123)
dat1 <- DVASdata[which(DVASdata$Count_plus=="1") %>% sample(., 200, replace=TRUE),]
dat2 <- DVASdata[which(DVASdata$Count_plus=="2") %>% sample(., 200, replace=TRUE),]
dat3 <- DVASdata[which(DVASdata$Count_plus=="3") %>% sample(., 200, replace=TRUE),]
out <- rbind(dat1, dat2, dat3) 
out$Count_plus <- as.numeric(as.character(out$Count_plus)) #class(out$Count_plus) is factor

#Built up a linear regresson model
model1 <- glm(formula = Count_plus ~ . - ACTIONID - Count - X1.4.5.6 - 高危機.死亡 
    - lng - lat - district - town - 施暴武器說明 - Count_plus, family = gaussian(link = "identity"), 
    data = out, na.action = na.exclude)
summary(model1) #r^2 = .222

   #以有顯著之項目再跑一個model
model2<-glm(formula = Count_plus ~ X8 + X9 + X10 + 家暴因素.個性.生活習慣不合 + 家暴因素.性生活不協調 +
    家暴因素.照顧壓力 + 家暴因素.酗酒 + 家暴因素.疑似或罹患精神疾病 + 被害人婚姻狀態 + 暴力型態.經濟暴力 + 
    自殺意念 + 自殺行為 + 求助時間差.小時 + OCCUPATION, family = gaussian(link = "identity"), 
    data = out, na.action = na.exclude) 
summary(model2) #結果r^2更小 = .164

#寫出預測分數
y1 <- predict.glm(model1,type = "response") #選擇model1
#將預測分數貼回原表單，並觀察預測效果
out$Predicty1 <- unlist(y1) # range 0.74~3.23
plot(out$Count_plus,out$Predicty1)


#Assign to new 2 groups
DVASdata$Count_plus2 <- cut(DVASdata$Count, breaks=c(1,3,Inf), right = F, labels = c(1,2))
#Resampling
set.seed(123)
dat4 <- DVASdata[which(DVASdata$Count_plus2=="1") %>% sample(., 200, replace=TRUE),]
dat5 <- DVASdata[which(DVASdata$Count_plus2=="2") %>% sample(., 200, replace=TRUE),]
out2 <- rbind(dat4, dat5) 
out2$Count_plus2 <- as.numeric(as.character(out2$Count_plus2)) #class(out$Count_plus) is factor

#Built up a linear regresson model
model3 <- glm(formula = Count_plus2 ~ . - ACTIONID - Count - X1.4.5.6 - 高危機.死亡 
    - lng - lat - district - town - 施暴武器說明 - Count_plus - Count_plus2, family = gaussian(link = "identity"), 
    data = out2, na.action = na.exclude)
summary(model3) #r^2 = .342

   #以有顯著之項目再跑一個model
model4<-glm(formula = Count_plus ~ X1 + X2 + X12 + 家暴因素.不良嗜好.賭博.出入不正當場所 + 
    家暴因素.子女教養問題 + 家暴因素.財務支配或借貸問題 + 被害人婚姻狀態 + 暴力型態.精神暴力 + 
    自殺意念 + 自殺行為 + OCCUPATION + EDUCATION, family = gaussian(link = "identity"), 
    data = out2, na.action = na.exclude) 
summary(model4) #結果r^2一樣更小 = .274

#寫出預測分數
y2 <- predict.glm(model3,type = "response") #選擇model3
#將預測分數貼回原表單，並觀察預測效果
out2$Predicty1 <- unlist(y2) # range 0.56~2.17
plot(out$Count_plus2,out$Predicty1) #此模型看起是目前最具有預測力的！


#聽說我的寫法很怪...所以改成一般常用寫法，以免遭遇error。 ##比如 y3 <- predict.lm(model3,DVASdata,type = "response") 就出不來了！
out3 <- out2 %>% select(-ACTIONID, -Count, -X1.4.5.6,
                         -高危機.死亡, -lng, -lat, -district,
                         -town, -施暴武器說明, -Count_plus, -OCCUPATION) #OCCUPATION選擇拿掉，因為有些職業的人很少，抽樣會沒抽到。
m3 <- glm(formula = Count_plus2 ~ . , family = gaussian(link = "identity"), 
    data = out3, na.action = na.exclude)
summary(m3) #r^2 =.302 ##拿掉職業還是有.3

DVASdata2 <- DVASdata %>% select(-ACTIONID, -Count, -X1.4.5.6,
                                  -高危機.死亡, -lng, -lat, -district,
                                  -town, -施暴武器說明, -Count_plus, -OCCUPATION) 
#倒回去看看有沒有overfitting..
y4 <- predict.glm(m3, newdata = DVASdata2, type = "response")
DVASdata$Predicty4 <- unlist(y4)

#畫出來
plot(DVASdata$Count_plus2,DVASdata2$Predicty4) #boxplot
plot(as.numeric(DVASdata$Count_plus2),DVASdata2$Predicty4) #row plot
plot(as.numeric(DVASdata$Count),DVASdata2$Predicty4)

