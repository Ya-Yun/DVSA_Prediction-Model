library(dplyr)

dat <- read.csv("地圖資料.csv") #8977

#計算同一地址出現次數
dat.count <- dat %>% group_by(個案類型, 居住完整地址, lng, lat) %>% summarise(count=n()) #8277

 dat.count$個案類型 <- as.character(dat.count$個案類型)
 dat.count$居住完整地址 <- as.character(dat.count$居住完整地址)

#刪除不完整地址
count_clean <- subset(dat.count, nchar(居住完整地址, type = "chars")>10) #正常地址至少有11個字，如：台北市北投區石牌路1號 #8198
count_clean <- count_clean[-grep("\\不",count_clean$居住完整地址),] #有些地址會寫：台北市羅斯福路**餘不詳(我不知道為什麼"\\不詳"不能用) #8172
count_clean <- count_clean[-grep("\\*",count_clean$居住完整地址),]  #或是只有：台北市北投區石牌路** #8165
count_clean <- count_clean[-grep("\\社會局",count_clean$居住完整地址),]  #接受社會局安置 #8157
count_clean <- count_clean[-grep("\\疑似",count_clean$居住完整地址),]  #地址存疑 #8155

#取親密關係暴力，包含：婚姻關係暴力、離婚關係暴力、同居關係暴力
count.W <- count_clean[grep("\\關係暴力",count_clean$個案類型),] #4226
#取發生兩次以上(含兩次)的親密關係暴力地址，做為再犯評估 Alternate key
count.repAddress <- subset(count.W, count > 1) #317


write.csv(count_clean, "All_Address_clean.csv", row.names = FALSE) 
write.csv(count.W, "W_Address.csv", row.names = FALSE)
write.csv(count.repAddress, "W_repAddress.csv", row.names = FALSE)
