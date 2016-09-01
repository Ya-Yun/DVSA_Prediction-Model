#合併重複地址與通報單，以通報單號做為 Primary key

library(dplyr)
library(sqldf)

repAddress <- read.csv("repAddress.csv")
circular <- read.csv("通報表被害人相對人資料.csv")

#在重複地址後方填入通報單號(ACTIONID)及其他predictive index
merge <- sqldf(
    "select repAddress.*, circular.MAIMED, ACTIONID, VDTYPE, SEXID, IDTYPE, OTHERIDTYPE, OCCUPATION, OTHEROCCUPATION, EDUCATION
    from repAddress left join circular on repAddress.居住完整地址 = circular.居住完整地址"
    )
    
#得出重複發生親密關係暴力之通報單號
#此單號將連結TIPVDA量表及其他通報單資訊


write.csv(merge, "merge_ActionID_repAddr.csv", row.names = FALSE)
