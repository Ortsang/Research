library(data.table);
library(psych);
library(readxl);
library(dummies);
library(censReg);
library(DWreg)
#library(xlsx)
library(openxlsx);
library(bit64)

setwd("D:/Drives/OneDrive - Cuny GradCenter/!PhD Class/3rd year/1st sem/Labor DJ/replication project/data");
setwd("~/OneDrive - Cuny GradCenter/!PhD Class/3rd year/1st sem/Labor DJ/replication project/data")
data0 = fread("DW8493.csv")
##layoff == 2 if slack of work, elimination or shift of position (DWREAS==03|UH_WHYLEFT_1==05);
##layoff == 1 if plant closed (DWREAS==01);
data = data0
data$layoff = with(data,
                   ifelse(DWREAS==03|UH_WHYLFT_1==05,2,
                          ifelse(DWREAS==01,1,0)))

data$layoff = with(data,
                   ifelse(DWREAS==03|DWREAS==04|DWREAS==02|UH_WHYLFT_1==05,2,
                          ifelse(DWREAS==01,1,0)
                   ))

##subset 8486
#data = subset(data, YEAR==1984|YEAR==1986)
##EDUC
##have restrictions on AGE,SEX,
##DWCLASS - private sector == 02|== 03 (==04? selfemployed)
##DWOCC - non agricultral job, non construction job (!= 869,Construction laborers&!=054, agricultural)
##DWFULLTIME - fulltime == 02
data = subset(data,AGE>=20 & AGE<=61 & SEX==1)
data = subset(data,DWCLASS == 02| DWCLASS == 03|DWCLASS == 04)
data = subset(data,DWOCC != 054 & DWOCC != 869 & DWIND > 31 & DWIND != 60)
data = subset(data,DWFULLTIME == 02)
data = subset(data,DWWEEKL<9999)
data = subset(data,layoff != 0)
data = subset(data,DWWEEKC<9990)
data$notice[is.na(data$notice)]=NA
data$UH_WKSUN_1[data$UH_WKSUN_1<0]=NA
data$DWWKSUN[data$DWWKSUN>=90]=NA
##not easy to get the number 
data$DWYEARS[data$DWYEARS>=90]=NA

data$tenure = data$DWYEARS

data$yoschool = with(data,
                     ifelse(EDUC==001|EDUC==002,0,
                            ifelse(EDUC==011,1,
                                   ifelse(EDUC==012,2,
                                          ifelse(EDUC==013,3,
                                                 ifelse(EDUC==014,4,
                                   ifelse(EDUC==021,5,
                                          ifelse(EDUC==022,6,
                                                 ifelse(EDUC==031,7,
                                                          ifelse(EDUC==032,8,
                                   ifelse(EDUC==040,9,
                                          ifelse(EDUC==050,10,                            
                                                 ifelse(EDUC==060,11,
                                                     ifelse(EDUC==070|EDUC==072|EDUC==073,12,
                                   ifelse(EDUC==080,13,
                                                 ifelse(EDUC==090,14,
                                                     ifelse(EDUC==100,15,
                                                           ifelse(EDUC==110,16,
                                   ifelse(EDUC==121,17,
                                                 ifelse(EDUC==122,18,0))))))))))))))))))))
data$yearofjobl = with(data,
                       ifelse(DWLASTWRK==00,data$YEAR,
                              ifelse(DWLASTWRK==01,data$YEAR-1,
                                     ifelse(DWLASTWRK==02,data$YEAR-2,
                                            ifelse(DWLASTWRK==03,data$YEAR-3,
                                                   ifelse(DWLASTWRK==04,data$YEAR-4,data$YEAR-5))))))
data$experience = data$AGE-data$yoschool-6-(data$YEAR-data$yearofjobl);
data$experience[data$experience<0]=0
data$experience2 = data$experience^2
data = subset(data,DWWEEKC!=0&DWWEEKL!=0);
data$DWWEEKC[data$YEAR==1986] = data$DWWEEKC[data$YEAR==1986]/1.047
data$lweekl = log(data$DWWEEKL);
data$lweekc = log(data$DWWEEKC);
data$changele = log(data$DWWEEKL/data$DWWEEKC);
data$layoff1 = data$layoff-1
data$nounem = with(data,ifelse(DWWKSUN==0,1,0))
data$notice = with(data,ifelse(DWNOTICE==5,1,0))
indcross = read.csv("IND.cross_unrate.csv")
occcross = read_excel("occcross_8486.xlsx")
data = merge(data,indcross,by=c("DWIND"="IND"),all.x = T)
data = merge(data,occcross,by=c("DWOCC"="OCC"),all.x = T)
#data$notice[is.na(data$notice)]=0
data$manufa = with(data,ifelse(DWIND>=100&DWIND<500,1,0))
data1 = data.table(cbind(data$layoff1,data$tenure,data$changele,data$lweekl,data$lweekc,data$DWWKSUN,
                         data$nounem,data$notice,data$yoschool,data$experience,data$manufa,data$WWRK))
names(data1) = c("layoff","ptenure","clweeke","lpearn","lcearn","wjobless","noune",
                 "adnotif","schooling","experience","manuf","whiteco")

#table1
output.table1 = cbind(as.data.frame(describe(data1))[,1:4],as.data.frame(describe(data1[layoff==1]))[,1:4],as.data.frame(describe(data1[layoff==0]))[,1:4])
#table2
output.table2.1 = as.data.frame(describe(data1[layoff==0&whiteco==1]))[,1:4]
output.table2.2 = as.data.frame(describe(data1[layoff==1&whiteco==1]))[,1:4]
output.table2.3 = as.data.frame(describe(data1[layoff==0&whiteco==0]))[,1:4]
output.table2.4 = as.data.frame(describe(data1[layoff==1&whiteco==0]))[,1:4]
output.table2 = cbind(output.table2.1,output.table2.2,output.table2.3,output.table2.4)
#table3
#running dependent vairables wage, 
##wage change (log(cuurentwage/previouswage)), prewage log(previous wage), after wage
#spline function - 
##previous tenure (dummy in 1,2,3,6), education, advance notification
##year-of-displacesment dummies, seven previous industry dummies, eight occupation dummies,
##experience and its square, marriage dummy, nonwhite dummy, region dummy
data$tenurei = with(data,
                    ifelse(tenure<=1|is.na(tenure), 1,
                       ifelse(tenure>1&tenure<=2, 2,
                          ifelse(tenure>2&tenure<=3,3,6))))
data = cbind(data, dummy(data$tenurei,sep = "_tenure_"))
data = cbind(data, dummy(data$yearofjobl,sep = "_lj_"))
data$dwindi = data$DWIND%/%100
data$dwindi[data$dwindi<=1]=1
data = subset(data,data$dwindi<9)
data = cbind(data, dummy(data$dwindi,sep = "_ind_"))
data$dwocci = data$DWOCC%/%100
data = cbind(data, dummy(data$dwocci,sep = "_occ_"))
data$marriage = with(data,ifelse(MARST==1|MARST==2,1,0))
data$nonwhite = with(data,ifelse(RACE!=100,1,0))
data$highun = with(data,ifelse(unrate>=median(data$unrate,na.rm = T),1,0)) #0.04606501 is the median of Union rate
data$stated = with(data,
                   ifelse(STATEFIP==09|(STATEFIP>=17&STATEFIP<=19)|STATEFIP==23|STATEFIP==25|
                            STATEFIP==26|STATEFIP==27|STATEFIP==31|STATEFIP==33|STATEFIP==34|
                            STATEFIP==36|STATEFIP==38|STATEFIP==39|STATEFIP==42|STATEFIP==44|
                            STATEFIP==46|STATEFIP==50|STATEFIP==55,1,
                          ifelse(STATEFIP==01|STATEFIP==05|STATEFIP==10|STATEFIP==12|STATEFIP==13|
                                 STATEFIP==21|STATEFIP==22|STATEFIP==24|STATEFIP==28|STATEFIP==29|
                                 STATEFIP==37|STATEFIP==40|STATEFIP==45|STATEFIP==47|STATEFIP==48|
                                 STATEFIP==51|STATEFIP==54,2,3)))
data = cbind(data, dummy(data$stated,sep = "_region_"))

func.spline = ("tenurei+yoschool+data_lj_1979+data_lj_1980+data_lj_1981+
               data_lj_1982+data_lj_1983+data_lj_1984+data_lj_1985+data_ind_1+data_ind_2+data_ind_3+data_ind_4+data_ind_5+data_ind_6+data_ind_7+
               data_occ_0+data_occ_1+data_occ_2+data_occ_3+data_occ_4+data_occ_5+data_occ_6+data_occ_7+
               marriage+nonwhite+data_region_1+data_region_2+notice+experience+experience2")
output.table3 = data.frame(estimate=c(NA),se=c(NA),t=c(NA),p=c(NA))
output.table3[1,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table3[2,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table3[3,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table3[4,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table3[5,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table3[6,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table3[7,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table3[8,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table3[9,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table3[10,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (unrate<=median(data$unrate,na.rm = T))))$coefficients[2,1:4]
output.table3[11,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data,subset = (unrate<=median(data$unrate,na.rm = T))))$coefficients[2,1:4]
output.table3[12,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (unrate<=median(data$unrate,na.rm = T))))$coefficients[2,1:4]
output.table3[13,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (unrate>median(data$unrate,na.rm = T))))$coefficients[2,1:4]
output.table3[14,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data,subset = (unrate>median(data$unrate,na.rm = T))))$coefficients[2,1:4]
output.table3[15,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (unrate>median(data$unrate,na.rm = T))))$coefficients[2,1:4]



##table 4
data$lowten = with(data,ifelse(tenure<2,1,0))
data$higten = with(data,ifelse(tenure>=2,1,0))
data$lfXlow = data$layoff1*data$lowten
data$lfXhig = data$layoff1*data$higten
output.table4 = data.frame(estimate=c(NA),se=c(NA),t=c(NA),p=c(NA))
output.table4[1,] = summary(lm(paste("changele~lfXlow+DWLASTWRK+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table4[2,] = summary(lm(paste("lweekl~lfXlow+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table4[3,] = summary(lm(paste("lweekc~lfXlow+DWLASTWRK+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table4[4,] = summary(lm(paste("changele~lfXhig+DWLASTWRK+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table4[5,] = summary(lm(paste("lweekl~lfXhig+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table4[6,] = summary(lm(paste("lweekc~lfXhig+DWLASTWRK+",func.spline,sep = ""),data = data))$coefficients[2,1:4]
output.table4[7,] = summary(lm(paste("changele~lfXlow+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table4[8,] = summary(lm(paste("lweekl~lfXlow+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table4[9,] = summary(lm(paste("lweekc~lfXlow+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table4[10,] = summary(lm(paste("changele~lfXhig+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table4[11,] = summary(lm(paste("lweekl~lfXhig+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table4[12,] = summary(lm(paste("lweekc~lfXhig+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==1)))$coefficients[2,1:4]
output.table4[13,] = summary(lm(paste("changele~lfXlow+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table4[14,] = summary(lm(paste("lweekl~lfXlow+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table4[15,] = summary(lm(paste("lweekc~lfXlow+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table4[16,] = summary(lm(paste("changele~lfXhig+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table4[17,] = summary(lm(paste("lweekl~lfXhig+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]
output.table4[18,] = summary(lm(paste("lweekc~lfXhig+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (WWRK==0)))$coefficients[2,1:4]

##table5
data$cemp = with(data,ifelse((EMPSTAT>=10&EMPSTAT<=12),1,0))
output.table5 = data.frame(estimate=c(NA),se=c(NA),t=c(NA),p=c(NA))
output.table5[1,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1)))$coefficients[2,1:4]
output.table5[2,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1)))$coefficients[2,1:4]
output.table5[3,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1)))$coefficients[2,1:4]
output.table5[4,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1&WWRK==1)))$coefficients[2,1:4]
output.table5[5,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1&WWRK==1)))$coefficients[2,1:4]
output.table5[6,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1&WWRK==1)))$coefficients[2,1:4]
output.table5[7,] = summary(lm(paste("changele~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1&WWRK==0)))$coefficients[2,1:4]
output.table5[8,] = summary(lm(paste("lweekl~layoff1+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1&WWRK==0)))$coefficients[2,1:4]
output.table5[9,] = summary(lm(paste("lweekc~layoff1+DWLASTWRK+",func.spline,sep = ""),data = data,subset = (DWLASTWRK>=2&cemp==1&WWRK==0)))$coefficients[2,1:4]
##layoff1, DWYEARS, lweekc-lweekl, lweekl, lweekc
##DWWKSUN, DWNOTICE, 

##table6
data$DWWKSUN[data$DWWKSUN==0] = NA
data$llaydur = log(data$DWWKSUN)
data$lfXhiun = data$layoff1*data$highun
data$lfXwite = data$layoff1*data$WWRK
data$lfXunfr = data$layoff1*data$unrate
func.spline.table6 = "yoschool+notice+data_lj_1979+data_lj_1980+data_lj_1981+data_lj_1982+data_lj_1983+data_lj_1984+data_lj_1985+
               data_ind_1+data_ind_2+data_ind_3+data_ind_4+data_ind_5+data_ind_6+data_ind_7+data_ind_8+
               data_occ_0+data_occ_1+data_occ_2+data_occ_3+data_occ_4+data_occ_5+data_occ_6+data_occ_7+data_occ_8+
               experience+experience2+marriage+nonwhite+data_region_1+data_region_2+data_region_3"
output.table6 = data.frame(estimate=c(NA),se=c(NA),t=c(NA),p=c(NA))
output.table6 = dw.reg(paste("DWWKSUN~layoff1+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$survreg[c(2:5,38),1:4]
output.table6 = rbind(output.table6,dw.reg(paste("DWWKSUN~layoff1+lfXwite+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$survreg[c(2:6,40),1:4])  
output.table6 = rbind(output.table6,dw.reg(paste("DWWKSUN~layoff1+lfXhiun+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$survreg[c(2:6,40),1:4])  
output.table6 = rbind(output.table6,dw.reg(paste("DWWKSUN~layoff1+lfXunfr+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$survreg[c(2:6,40),1:4])  
output.table6.1 = dw.reg(paste("DWWKSUN~layoff1+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$loglik
output.table6.1 = data.frame(rbind(output.table6.1,dw.reg(paste("DWWKSUN~layoff1+lfXwite+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$loglik))
output.table6.1 = rbind(output.table6.1,dw.reg(paste("DWWKSUN~layoff1+lfXhiun+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$loglik)
output.table6.1 = rbind(output.table6.1,dw.reg(paste("DWWKSUN~layoff1+lfXunfr+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)$loglik)


## Create a blank workbook
wb <- createWorkbook()
addWorksheet(wb, "table1")
addWorksheet(wb, "table2")
addWorksheet(wb, "table3")
addWorksheet(wb, "table4")
addWorksheet(wb, "table5")
addWorksheet(wb, "table6")
addWorksheet(wb, "table6.1")
writeData(wb, "table1", output.table1)
writeData(wb, "table2", output.table2)
writeData(wb, "table3", output.table3)
writeData(wb, "table4", output.table4)
writeData(wb, "table5", output.table5)
writeData(wb, "table6", output.table6)
writeData(wb, "table6.1", output.table6.1)

saveWorkbook(wb, file = "Katz Lemon f4.xlsx", overwrite = TRUE)
##note: 
##DWYEAR times 100
##notice na turns to 0
#a = dw.reg(paste("DWWKSUN~layoff1+lfXwite+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)
#    dw.reg(paste("DWWKSUN~layoff1+unrate+tenure+lweekl+",func.spline.table6,sep = ""),data = data, para.q2 = T)

##output
#write.xlsx(output.table1,file = "Katz_lemon tables.xlsx",sheetName = "table1")
#write.xlsx(output.table2,file = "Katz_lemon tables.xlsx",sheetName = "table2",append=T)
#write.xlsx(output.table3,file = "Katz_lemon tables.xlsx",sheetName = "table3",append=T)
#write.xlsx(output.table4,file = "Katz_lemon tables.xlsx",sheetName = "table4",append=T)
#write.xlsx(output.table5,file = "Katz_lemon tables.xlsx",sheetName = "table5",append=T)
#write.xlsx(output.table6,file = "Katz_lemon tables.xlsx",sheetName = "table6",append=T)
#write.xlsx(output.table6.1,file = "Katz_lemon tables.xlsx",sheetName = "table6.1",append=T)

