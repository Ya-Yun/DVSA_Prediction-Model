library(dplyr)

dat <- read.csv("地圖資料.csv")

#計算同一完整地址出現次數
dat.count <- dat %>% group_by(個案類型, 居住完整地址, lng, lat) %>% summarise(count=n())

#只取發生兩次以上(含兩次)的親密關係暴力
dat.count.repAddress <- subset(dat.count, count > 1 & 個案類型 == "婚姻關係暴力")

#刪除地址不完整之資料


write.csv(dat.count.repAddress, "repAddress.csv", row.names = FALSE)
