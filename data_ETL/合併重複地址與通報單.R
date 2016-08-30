#合併重複地址與通報單，以通報單號做為primary key (替代身分證號)

library(dplyr)
library(sqldf)

#在重複地址後填上通報單號(ACTIONID)及其他predictive index
merge <- sqldf(
    "select repAdress.*, circular.MAIMED, ACTIONID, VDTYPE, SEXID, IDTYPE, OTHERIDTYPE, OCCUPATION, OTHEROCCUPATION, EDUCATION
    from repAdress left join circular on repAdress.居住完整地址 = circular.居住完整地址"
    )
    
#得出重複發生親密關係暴力之通報單號
#此單號將連結TIPVDA量表及其他通報單資訊
