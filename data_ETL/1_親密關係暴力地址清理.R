library(dplyr)

dat <- read.csv("地圖資料.csv")

#計算同一地址出現次數
dat.count <- dat %>% group_by(個案編號, 個案類型, 居住完整地址, lng, lat) %>% summarise(count=n()) 
#雖然目前其他表單沒撈到「個案編號」(CA夾號)，但先拿出來以免之後重撈有此筆資資料，此為 Primary key

 dat.count$個案類型 <- as.character(dat.count$個案類型)
 dat.count$居住完整地址 <- as.character(dat.count$居住完整地址)

#刪除不完整地址
count_clean <- subset(dat.count, nchar(居住完整地址, type = "chars")>10) #正常地址至少有11個字，如：台北市北投區石牌路1號
count_clean <- count_clean[-grep("\\不詳",count_clean$居住完整地址),] #有些地址會寫：台北市羅斯福路**餘不詳
count_clean <- count_clean[-grep("\\*",count_clean$居住完整地址),]  #或是只有：台北市北投區石牌路**

#取親密關係暴力，包含：婚姻關係暴力、離婚關係暴力、同居關係暴力
count.W <- count_clean[grep("\\關係暴力",count_clean$個案類型),]
#取發生兩次以上(含兩次)的親密關係暴力地址，做為再犯評估 Alternate key
count.repAddress <- subset(count.W, count > 1)


write.csv(count_clean, "All_Address_clean", row.names = FALSE)
write.csv(count.W, "W_Address", row.names = FALSE)
write.csv(count.repAddress, "W_repAddress.csv", row.names = FALSE)
