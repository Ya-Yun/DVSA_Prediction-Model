library(dplyr)
library(sqldf)
DVASdata <- read.csv("final_DVAS data.csv") #imputed_age_DVAS.csv
DVASdata$Count_plus2 <- cut(DVASdata$Count, breaks=c(1,3,Inf), right = F, labels = c(1,2))
#Resampling
set.seed(123)
dat1 <- DVASdata[which(DVASdata$Count_plus2=="1") %>% sample(., 200, replace=TRUE),]
dat2 <- DVASdata[which(DVASdata$Count_plus2=="2") %>% sample(., 200, replace=TRUE),]
out2 <- rbind(dat1, dat2) 
out2$Count_plus2 <- as.numeric(as.character(out2$Count_plus2))

#bulid up a model(model1)
out3 <- out2 %>% select(-ACTIONID, -Count, -X1.4.5.6,
                        -高危機.死亡, -lng, -lat, -district,
                        -town, -施暴武器說明, -OCCUPATION) #OCCUPATION選擇拿掉，因為有些職業的人很少，抽樣會沒抽到。
m1 <- glm(formula = Count_plus2 ~ . , family = gaussian(link = "identity"), 
          data = out3, na.action = na.exclude)
summary(m1) #r^2 = .324

# give all data a predictive score
DVASdata2 <- DVASdata %>% select(-ACTIONID, -Count, -X1.4.5.6,
                                -高危機.死亡, -lng, -lat, -district,
                                -town, -施暴武器說明, -OCCUPATION) 

y1 <- predict.glm(m1, newdata = DVASdata2, type = "response")
DVASdata$Predicty1 <- unlist(y1)

#plot the score
plot(as.numeric(DVASdata$Count_plus2),DVASdata$Predicty1)
#3217筆為高再犯風險組，3072筆為第一次進案(if cut point set as 1.2)

##Second model (model2)
# cut point set as 1.2
DVASdata_1.2 <- subset(DVASdata, Predicty1 > 1.2) 
DVASdata_1.2$Count_1.2 <- cut(DVASdata_1.2$Count, breaks=c(1,3,Inf), right = F, labels = c(1,2))
#喪偶只有三個人(3/16)，Education不識字只有2人(2/22)..要建模的data一直抽不到，所以刪掉
DVASdata_1.2 <- subset(DVASdata_1.2, 被害人婚姻狀態 != "喪偶") 
DVASdata_1.2 <- subset(DVASdata_1.2, EDUCATION != "不識字") 
#Resampling
set.seed(123)
dat3 <- DVASdata_1.2[which(DVASdata_1.2$Count_1.2=="1") %>% sample(., 200, replace=TRUE),]
dat4 <- DVASdata_1.2[which(DVASdata_1.2$Count_1.2=="2") %>% sample(., 200, replace=TRUE),]
out5 <- rbind(dat3, dat4) 
out5$Count_1.2 <- as.numeric(as.character(out5$Count_1.2))

out6 <- out5 %>% select(-ACTIONID, -Count, -X1.4.5.6,
                        -高危機.死亡, -lng, -lat, -district,
                        -town, -施暴武器說明, -Count_plus2, 
                        -OCCUPATION, -Predicty1) 

m2 <- glm(formula = Count_1.2 ~ . ,
          family = gaussian(link = "identity"),
          data = out6, na.action = na.exclude) # r^2=.243
summary(m2)

#give subset data a predictive score
DVASdata_1.2.2 <- DVASdata_1.2 %>% select(-ACTIONID, -Count, -X1.4.5.6,
                                        -高危機.死亡, -lng, -lat, -district,
                                        -town, -施暴武器說明, -Count_plus2, 
                                        -OCCUPATION, -Predicty1) 

y2 <- predict.glm(m2, newdata = DVASdata_1.2.2, type = "response")
DVASdata_1.2$Predicty2 <- unlist(y2)
plot(as.numeric(DVASdata_1.2$Count_plus2),DVASdata_1.2$Predicty2)

## Third model (model3)
## cut point set as 1.11
DVASdata_1.11 <- subset(DVASdata_1.2, Predicty2 > 1.11) 
DVASdata_1.11$Count_1.11 <- cut(DVASdata_1.11$Count, breaks=c(1,3,Inf), right = F, labels = c(1,2))
#Resampling
set.seed(123)
dat5 <- DVASdata_1.11[which(DVASdata_1.11$Count_1.11=="1") %>% sample(., 200, replace=TRUE),]
dat6 <- DVASdata_1.11[which(DVASdata_1.11$Count_1.11=="2") %>% sample(., 200, replace=TRUE),]
out7 <- rbind(dat5, dat6) 
out7$Count_1.11 <- as.numeric(as.character(out7$Count_1.11))

out8 <- out7 %>% select(-ACTIONID, -Count, -X1.4.5.6,
                        -高危機.死亡, -lng, -lat, -district,
                        -town, -施暴武器說明, -OCCUPATION, -Count_plus2, 
                        -Count_1.2, -Predicty1, -Predicty2) 

m3 <- glm(formula = Count_1.11 ~ . ,
          family = gaussian(link = "identity"),
          data = out8, na.action = na.exclude) # r^2=.234
summary(m3)

#give subset data a predictive score
DVASdata_1.11.2 <- DVASdata_1.11 %>% select(-ACTIONID, -Count, -X1.4.5.6,
                                            -高危機.死亡, -lng, -lat, -district,
                                            -town, -施暴武器說明, -OCCUPATION, -Count_plus2, 
                                            -Count_1.2, -Predicty1, -Predicty2) 
y3 <- predict.glm(m3, newdata = DVASdata_1.11.2, type = "response")
DVASdata_1.11$Predicty3 <- unlist(y3)
plot(as.numeric(DVASdata_1.11$Count_plus2),DVASdata_1.11$Predicty3)

#2809筆為高再犯風險組，2666筆為第一次進案(-405 compare with model1)
