library(dplyr)
library(sqldf)

#合併重複地址次數與通報單，以地址做為 Primary key
repAddress <- read.csv("W_Address.csv")
circular <- read.csv("通報表被害人相對人資料.csv")

#在重複地址後方填入通報單號(ACTIONID)及其他predictive index。由於通報表缺漏案件類型，此舉能篩出僅為親密關係暴力者
merge <- sqldf(
    "select repAddress.*, circular.MAIMED, OTHERMAIMED, ACTIONID, VDTYPE, SEXID, BDATE, IDTYPE, OTHERIDTYPE, OCCUPATION, OTHEROCCUPATION, EDUCATION
    from repAddress left join circular on repAddress.居住完整地址 = circular.居住完整地址"
    )

#合併TIPVDA表與含地址次數之通報單，以通報單號做為 Primary key

#TIPVDA表只由被害人填寫，因此將通報單被害人及加害人分開
victim_cir <- subset(merge, merge$VDTYPE == "被害人")
offender_cir <- subset(merge, merge$VDTYPE == "加害人")
    
tipvda <- read.csv("含TIPVDA表之通報資訊.csv")

#在TIPVDA表後填入地址次數及其他通報單predictive index
merge2 <- sqldf(
    "select tipvda.*, victim_cir.MAIMED, OTHERMAIMED, ACTIONID, VDTYPE, SEXID, BDATE, IDTYPE, OTHERIDTYPE, OCCUPATION, OTHEROCCUPATION, EDUCATION
    from repAddress left join circular on tipvda.ACTIONID = victim_cir.ACTIONID"
    ) 
    #Error in sqliteSendQuery(con, statement, bind.data) : error in statement: no such table: tipvda

write.csv(merge, "merge_ActionID_repAddr.csv", row.names = FALSE)
write.csv(victim_cir, "victim_cir.csv", row.names = FALSE)
write.csv(offender_cir, "offender_cir.csv", row.names = FALSE)
