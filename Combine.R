require(data.table)
source_dir=readline("Enter the directory for the R Scripts (Replace backslashes with a frontslash): ")

source(paste(source_dir,"/PeopleSoft_Analysis.R",sep=""))
source(paste(source_dir,"/Aura_analysis_2.R",sep=""))
YZPER037_AAPEEO=merge(YZPER037_AAPEEO,Emp_Avg_hours,by="Emplid")
YZPER037_AAPEEO=merge(YZPER037_AAPEEO,Num_clients_Emp,by="Emplid")


source(paste(source_dir,"/YZPER003_ActiveWithinRange.R",sep=""))
YZPER037_AAPEEO=merge(YZPER037_AAPEEO,Merge_YZPER003_ActiveWithinRange,by="Emplid")
remove(Merge_YZPER003_ActiveWithinRange)

source(paste(source_dir,"/YZPER032_Last3SalaryIncreases.R",sep=""))
YZPER037_AAPEEO=merge(YZPER037_AAPEEO,Merge_YZPER032_Last3SalaryIncreases,by="Emplid")
remove(Merge_YZPER032_Last3SalaryIncreases)

source(paste(source_dir,"/YZPER009_ActivewithYrsofSvc.R",sep=""))
YZPER037_AAPEEO=merge(YZPER037_AAPEEO,Merge_YZPER009_ActivewithYrsofSvc,by="Emplid")
remove(Merge_YZPER009_ActivewithYrsofSvc)
str(YZPER037_AAPEEO)

Overall_Attrition=round((nrow(YZPER037_AAPEEO[Action =="TER" & `Action Reason` %in% c("RES","TER","MUT","TMP"),])/nrow(YZPER037_AAPEEO))*100,2)
Reg_Emp_Attrition=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="R" & Action =="TER" & `Action Reason` %in% c("RES","TER","MUT","TMP"),])/nrow(YZPER037_AAPEEO))*100,2)
Temp_Emp_Attrition=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="T" & Action =="TER" & `Action Reason` %in% c("RES","TER","MUT","TMP"),])/nrow(YZPER037_AAPEEO))*100,2)
Termination=round((nrow(YZPER037_AAPEEO[Action =="TER" & `Action Reason` %in% c("TER"),])/nrow(YZPER037_AAPEEO))*100,2)
Reg_Emp_Terminations=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="R" & Action =="TER" & `Action Reason` %in% c("TER"),])/nrow(YZPER037_AAPEEO))*100,2)
Temp_Emp_Terminations=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="T" & Action =="TER" & `Action Reason` %in% c("TER"),])/nrow(YZPER037_AAPEEO))*100,2)
Resignation=round((nrow(YZPER037_AAPEEO[Action =="TER" & `Action Reason` %in% c("RES"),])/nrow(YZPER037_AAPEEO))*100,2)
Reg_Emp_Resignation=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="R" & Action =="TER" & `Action Reason` %in% c("RES"),])/nrow(YZPER037_AAPEEO))*100,2)
Temp_Emp_Resignation=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="T" & Action =="TER" & `Action Reason` %in% c("RES"),])/nrow(YZPER037_AAPEEO))*100,2)
Mutual_Churn=round((nrow(YZPER037_AAPEEO[Action =="TER" & `Action Reason` %in% c("MUT","TMP"),])/nrow(YZPER037_AAPEEO))*100,2)
Reg_Mutual_Churn=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="R" & Action =="TER" & `Action Reason` %in% c("MUT","TMP"),])/nrow(YZPER037_AAPEEO))*100,2)
Temp_Mutual_Churn=round((nrow(YZPER037_AAPEEO[`Reg/Temp`=="T" & Action =="TER" & `Action Reason` %in% c("MUT","TMP"),])/nrow(YZPER037_AAPEEO))*100,2)
Emp_Aura_Merge=YZPER037_AAPEEO[,.(Emplid,Action,`Action Reason`,Empl_Status,`Reg/Temp`,`P/F`)]
gh=aura_chicago
gh$Emplid=as.integer(gh$Emplid)
gh=merge(aura_chicago,Emp_Aura_Merge,by="")


write.csv(gh,file =paste(source_dir,"/Aura_YZPER037_AAPEEO.csv",sep=""),row.names=FALSE, na="")

YZPER037_AAPEEO[Action =="TER" & `Action Reason` %in% c("TER"),]$Years_RehQuit


#source("C:/Users/prithvinatha/Documents/YZPER006_ActivewithSalary.R")

#######################

mod=YZPER037_AAPEEO
str(mod$Hike_Reason_1)
mod$Hourly_Rate=mod$Annual_Rt/2080
mod$Hike_PCT_1=ifelse(mod$Hike_Reason_1 %in% c("Merit","Merit/Promotion"),mod$Hike_PCT_1,0)
#mod$Hike_Date1=ifelse(mod$Hike_Reason_1 %in% c("Merit","Merit/Promotion"),mod$Hike_Date1,"")
ref_date <- as.Date("05/31/17", "%m/%d/%y")
mod$Hike_1Year=ifelse(mod$Empl_Status=="A" & ((as.numeric(difftime(ref_date,mod$Hike_Date1,units = "weeks"))/52.25)< 0.5),1,
                      ifelse(mod$Empl_Status=="T" & ((as.numeric(difftime(mod$`Term Dt`,mod$Hike_Date1,units = "weeks"))/52.25)< 0.5),1,0))
mod=mod[,.(Gender,`P/F`,Grade,`Reg/Temp`,Age,Std_Hours,Empl_Status,Mar_Status,Avg_hrs_week,Num_clients,Avg_Hrs_Client,Hike_PCT_1,Years_BQuit,Hourly_Rate,Hike_Last6Months)]
#mod$Hike_PCT_2=ifelse(mod$Hike_Reason_2 %in% c("Merit","Merit/Promotion"),mod$Hike_PCT_2,0)
mod$Empl_Status=ifelse(mod$Empl_Status=="A",0,1)
colnames(mod)[7]<-"Churned"
mod$Churned=as.factor(mod$Churned)
mod$Hike_Last6Months=as.factor(mod$Hike_Last6Months)
#install.packages("caTools")
library(caTools)
set.seed(100)
split=sample.split(mod$Churned,SplitRatio=0.75)
projdataTrain=subset(mod,split==TRUE)
projdataTest=subset(mod,split==FALSE)
str(projdataTrain)
library(rpart)
library(rpart.plot)
library(tree)
library(rattle)
library(RColorBrewer)
rattle( useGtkBuilder = TRUE)
r.ctrl = rpart.control(minsplit=4, minbucket = 3, cp = 0, xval = 20)
modelCT = rpart(projdataTrain$Churned ~ ., data=projdataTrain,control = r.ctrl)
prp(modelCT)
fancyRpartPlot(modelCT)
printcp(modelCT)
plotcp(modelCT)
#prune tree
mytree<-prune(modelCT,cp=0.12,"CP")
fancyRpartPlot(mytree, uniform=TRUE)

########################



write.csv(YZPER037_AAPEEO,file =paste(source_dir,"/YZPER037_AAPEEO.csv",sep=""),row.names=FALSE, na="")
