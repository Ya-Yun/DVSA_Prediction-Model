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
B <- <- ggplot(EDA, aes(x=通報時是否已死亡.1.0,y=Count.處遇中,group =通報時是否已死亡.1.0 )) + geom_boxplot()
    + scale_y_continuous(name="Count") + scale_x_continuous(name="Dead", breaks = c(0,1))
    + labs(title = "Count vs. Fatal Cases")

