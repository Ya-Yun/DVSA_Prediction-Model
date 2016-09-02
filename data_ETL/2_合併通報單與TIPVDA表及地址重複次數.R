library(dplyr)
library(sqldf)

#合併重複地址次數與通報單，以地址做為 Primary key
repAddress <- read.csv("W_Address.csv") #4226
circular <- read.csv("通報表被害人相對人資料.csv") #14552

#在重複地址後方填入通報單號(ACTIONID)及其他predictive index。由於通報表缺漏案件類型，此舉能篩出僅為親密關係暴力者
merge <- sqldf(
    "select repAddress.*, circular.MAIMED, OTHERMAIMED, ACTIONID, VDTYPE, SEXID, BDATE, IDTYPE, OTHERIDTYPE, OCCUPATION, OTHEROCCUPATION, EDUCATION
    from repAddress left join circular on repAddress.居住完整地址 = circular.居住完整地址"
    ) #8560

#合併TIPVDA表與含地址次數之通報單，以通報單號做為 Primary key

    #TIPVDA表只由被害人填寫，因此將通報單被害人及加害人分開
    victim_cir <- subset(merge, merge$VDTYPE == "被害人") #5042
    offender_cir <- subset(merge, merge$VDTYPE == "加害人") #3100

    #將加害人資料改名並到被害人資料後面
    S1 <- names(offender_cir)
    S2 <- "off"
    S3 <- paste(S2, S1, sep = "_")
    colnames(offender_cir) <- S3
    
    vic_off_cir <- sqldf(
     "select victim_cir.*, offender_cir.off_VDTYPE, off_MAIMED, off_OTHERMAIMED, off_SEXID, off_BDATE, off_IDTYPE, off_OTHERIDTYPE, off_OCCUPATION, off_OTHEROCCUPATION, off_EDUCATION
     from victim_cir left join offender_cir on victim_cir.ACTIONID = offender_cir.off_ACTIONID"
     ) #5300

tipvda <- read.csv("含TIPVDA表之通報資訊.csv")

#在TIPVDA表後填入地址次數及其他通報單predictive index
    #merge2 <- sqldf(
    #    "select tipvda.*, vic_off_cir.*
    #    from tipvda left join vic_off_cir on tipvda.ACTIONID = vic_off_cir.ACTIONID"
    #    ) 
        #Error in sqliteSendQuery(con, statement, bind.data) : error in statement: no such table: tipvda
        #Install RH2 package and run require(RH2) before running the sqldf query
        #sqliteSendQuery error was be solved, but I met a new one...
        #Error in .verify.JDBC.result(s, "Unable to execute JDBC statement ", statement); Table "tipvda" not found; SQL statement:
merge2 <-merge(tipvda, vic_off_cir, by.x = "ACTIONID", by.y = "ACTIONID") #5300


write.csv(merge, "merge_ActionID_repAddr.csv", row.names = FALSE)
write.csv(victim_cir, "victim_cir.csv", row.names = FALSE)
write.csv(offender_cir, "offender_cir.csv", row.names = FALSE)
write.csv(vic_off_cir, "vic_off_cir.csv", row.names = FALSE)
write.csv(merge2, "merge_TIPVDA_cir.csv", row.names = FALSE)
