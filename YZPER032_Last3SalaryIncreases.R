
#reading YZPER032_Last3SalaryIncreases file
file_path=readline("Please enter the full Path for the YZPER032_Last3SalaryIncreases report csv file(Replace backslashes with a frontslash): ")
YZPER032_Last3SalaryIncreases=fread(file_path)
remove(file_path)
colnames(YZPER032_Last3SalaryIncreases)[14]="Hike_Amt_1"
colnames(YZPER032_Last3SalaryIncreases)[15]="Hike_PCT_1"
colnames(YZPER032_Last3SalaryIncreases)[16]="Hike_Date1"
colnames(YZPER032_Last3SalaryIncreases)[17]="Hike_Reason_1"

colnames(YZPER032_Last3SalaryIncreases)[18]="Hike_Amt_2"
colnames(YZPER032_Last3SalaryIncreases)[19]="Hike_PCT_2"
colnames(YZPER032_Last3SalaryIncreases)[20]="Hike_Date2"
colnames(YZPER032_Last3SalaryIncreases)[21]="Hike_Reason_2"

colnames(YZPER032_Last3SalaryIncreases)[22]="Hike_Amt_3"
colnames(YZPER032_Last3SalaryIncreases)[23]="Hike_PCT_3"
colnames(YZPER032_Last3SalaryIncreases)[24]="Hike_Date3"
colnames(YZPER032_Last3SalaryIncreases)[25]="Hike_Reason_3"
date_Convert<-function(date_col){
  as.Date(date_col,"%m/%d/%Y")
}
YZPER032_Last3SalaryIncreases[,c("Hike_Date1","Hike_Date2","Hike_Date3"):=lapply(.SD,date_Convert),.SDcols=c("Hike_Date1","Hike_Date2","Hike_Date3")]


#YZPER032_Last3SalaryIncreases[,c("Hike_Amt_1","Hike_Amt_2","Hike_Amt_3","Hike_PCT_1","Hike_PCT_2","Hike_PCT_3"):=lapply(.SD,as.integer),.SDcols=c("Hike_Amt_1","Hike_Amt_2","Hike_Amt_3","Hike_PCT_1","Hike_PCT_2","Hike_PCT_3")]
YZPER032_Last3SalaryIncreases[,c("Hike_Reason_1","Hike_Reason_2","Hike_Reason_3"):=lapply(.SD,as.factor),.SDcols=c("Hike_Reason_1","Hike_Reason_2","Hike_Reason_3")]
str(YZPER032_Last3SalaryIncreases)
Merge_YZPER032_Last3SalaryIncreases=YZPER032_Last3SalaryIncreases[,.(Emplid,Hike_Amt_1, 
                                             Hike_PCT_1,Hike_Date1,Hike_Reason_1,Hike_Amt_2,Hike_PCT_2,Hike_Date2,Hike_Reason_2,Hike_Amt_3,Hike_PCT_3,
                                             Hike_Date3,Hike_Reason_3)]


