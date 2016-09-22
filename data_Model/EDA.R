library(dplyr)
library(ggthemes)
library(ggplot2)
library(plotly)

EDA <- read.csv("DVAS建模用_工作表3.csv")

#觀察再犯受害者與高危機個案是否有重疊性
A <- ggplot(EDA, aes(x=高危機個案.F.G.,y=Count.處遇中,group =高危機個案.F.G. )) + geom_boxplot() 
    + geom_point(color="red", size=1.5, alpha=0.2) + scale_y_continuous(name="Count")
    + scale_x_continuous(name="Risk", breaks = c(0,1)) + labs(title = "Count vs. High Risk Cases")

#觀察再犯受害者與致死案件是否有重疊性
B <- ggplot(EDA, aes(x=通報時是否已死亡.1.0,y=Count.處遇中,group =通報時是否已死亡.1.0 )) + geom_boxplot()
    + scale_y_continuous(name="Count") + scale_x_continuous(name="Dead", breaks = c(0,1))
    + labs(title = "Count vs. Fatal Cases")

#TIPVDA量表分數分佈情形 ##分佈強度
C <- qplot(TIPVDA, data = EDA, geom = "density")  #qplot(TIPVDA, data = EDA, geom = "histogram")

#觀察TIPVDA總分與再犯受害者關聯 ##盒型圖 ##點圖
D <- ggplot(EDA, aes(y=TIPVDA,x=Count.處遇中,group=Count.處遇中)) + geom_boxplot() 
    + scale_x_continuous(name="Counts", breaks = c(0,1,2,3,4,5,6,7))+ labs(title = "TIPVDA vs. Counts")
d <- ggplot(EDA, aes(y=TIPVDA,x=Count.處遇中,group=Count.處遇中)) + geom_point(color="red", size=2, alpha=0.5) 
    + scale_x_continuous(name="Counts", breaks = c(0,1,2,3,4,5,6,7)) + labs(title = "TIPVDA vs. Counts")

#TIPVDA 1.4.5.6 題與致命行為有關，觀察此四題皆勾選者與再犯受害者關聯
E <- ggplot(EDA, aes(y=Count.處遇中,x=X1.4.5.6.4, group = X1.4.5.6.4)) + geom_boxplot() + geom_point(color="blue", size=2, alpha=.05)
    + scale_x_continuous(name="1.4.5.6", breaks = c(0,1)) + labs(title = "TIPVDA #1.4.5.6 vs. Counts", y ="Counts")

#TIPVDA 1.4.5.6 題與死亡受害者關聯 ##四題皆勾選 ##四題勾的題數
F <- ggplot(EDA, aes(x=通報時是否已死亡.1.0,y=X1.4.5.6.4)) + geom_jitter(color="red", size=1.5, alpha=0.4) 
    + scale_y_continuous(name="#1.4.5.6", breaks = c(0,1))+scale_x_continuous(name="Dead", breaks = c(0,1))
    + labs(title = "TIPVDA #1.4.5.6 vs. Fatal Cases")
f <- ggplot(EDA, aes(x=通報時是否已死亡.1.0, y = X1.4.5.6, group = 通報時是否已死亡.1.0)) + geom_jitter(color="red", size=1.5, alpha=0.2) 
    + geom_boxplot(alpha=0.7) + scale_y_continuous(name="#1.4.5.6") + scale_x_continuous(name="Dead", breaks = c(0,1))
    + labs(title = "TIPVDA #1.4.5.6 vs. Fatal Cases")
    
#各題TIPVDA與再犯與否關聯性 ##以第一題為例
p1 <- plot_ly(
    x = c("Counts = 1", "Counts > 1"),
    y = c(0.74, 0.75),
    name = "No",
    type = "bar")
p2 <- add_trace(p1,
                x = c("Counts = 1", "Counts > 1"),
                y = c(0.26, 0.25),
                name = "Yes",
                type = "bar")
p3 <- layout(p2, barmode = "stack",xaxis = list(title = "X1"), yaxis = list( title = "Proportion"))
p3

#觀察通報人員種類是否影響TIPVDA分數
TIPVDA <- read.csv("TIPVDA_complete.csv") #完整填完TIPVDA的通報單
s <- read.csv("merge_TIPVDA_cir.csv")

merge <- sqldf(
    "select TIPVDA.*, s.通報人員種類
    from TIPVDA left join s on TIPVDA.ACTIONID = s.ACTIONID")
merge$通報人員種類 <- as.character(merge$通報人員種類)
merge$通報人員種類[merge$通報人員種類 == "警察人員"] <- "Police"
merge$通報人員種類[merge$通報人員種類 == "醫護人員"] <- "Hospital Staff" 
merge$通報人員種類[merge$通報人員種類 == "社會工作人員"] <- "Social Worker" 
merge$通報人員種類[merge$通報人員種類 == "教育人員"] <- "Educators"
merge$通報人員種類[merge$通報人員種類 == "村里幹事"] <- "village Officers"
merge$通報人員種類[merge$通報人員種類 == "司(軍)法人員"] <- "Judicial Officers" 
merge$通報人員種類[merge$通報人員種類 == "其他"] <- "Others" 
merge$通報人員種類 <- as.factor(merge$通報人員種類)

merge %>%
    group_by(通報人員種類) %>%
    dplyr::summarise(mean = mean(TIPVDA),
                     sd = sd(TIPVDA)) %>%
    mutate(cv = (sd/mean)*100)
    
G <- ggplot(merge, aes(y=TIPVDA,x=通報人員種類,group=通報人員種類)) + geom_boxplot()
    + labs(title = "TIPVDA vs. Occupation", x = "Occupation")
G
