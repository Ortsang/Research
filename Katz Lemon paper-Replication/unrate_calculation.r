library(data.table);
library(xlsx)
library(psych);
setwd("D:/Drives/OneDrive - Cuny GradCenter/!PhD Class/3rd year/1st sem/Labor DJ/replication project/data");
data = fread("DW8486.csv")
IND.cross = read_excel("indcross_8486.xlsx")
data = merge(data,indcross_8486,by = c("DWIND"="IND"),all.x = T)
data$num = 1
data0 = data
data = subset(data,OCC>0&OCC<900)
data = subset(data,CLASSWKR==13|CLASSWKR==14|CLASSWKR==21)

table1 = as.data.frame(data[IND!=0,list(
  INDPOP = sum(WTFINL)
), by=list(IND)])

table2 = as.data.frame(data[IND!=0&(UNION>=2),list(
  INDPOP = sum(WTFINL)
), by=list(IND)])

table1 = merge(table1, table2, by=c("IND"), all.x = T)

table1$unrate = table1$INDPOP.y/table1$INDPOP.x
table = data.frame(cbind(table1$IND,table1$unrate))
names(table)=c("IND","unrate")
indcross_8486 = merge(indcross_8486,table,by=c("IND"),all.x = T)

writeWorksheetToFile(data = indcross_8486, file = "indcross_8486_unrate.xlsx", sheet="sheet1")
write.xlsx(indcross_8486,file = "indcross_8486_unrate.xlsx",sheetName = "sheet1")
