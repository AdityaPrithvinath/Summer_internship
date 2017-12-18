#getwd()
#file_path=file.choose()
file_path=readline("Please enter the full Path for the YZPER037_AAPEEO report csv file(Replace backslashes with a frontslash): ")
YZPER037_AAPEEO=fread(file_path)
remove(file_path)

# Checking structure of the dataset

str(YZPER037_AAPEEO)

#Columns with Missing values
colnames(YZPER037_AAPEEO)[colSums(is.na(YZPER037_AAPEEO)) > 0]
#Rows with All NAs
YZPER037_AAPEEO[rowSums(is.na(YZPER037_AAPEEO)) !=ncol(YZPER037_AAPEEO),]
YZPER037_AAPEEO=YZPER037_AAPEEO[-231,]
YZPER037_AAPEEO[,`Curr EEO1CODE`:=NULL]
#Checking number of missing values in each column
colSums(is.na(YZPER037_AAPEEO))
YZPER037_AAPEEO$No_PayHike=ifelse((is.na(YZPER037_AAPEEO$`Last Salary Increase`)),1,0)
YZPER037_AAPEEO$`Last Salary Increase`=ifelse((is.na(YZPER037_AAPEEO$`Last Salary Increase`)),0,YZPER037_AAPEEO$`Last Salary Increase`)
YZPER037_AAPEEO$Prev_Comprate=ifelse((is.na(YZPER037_AAPEEO$Prev_Comprate)),0,YZPER037_AAPEEO$Prev_Comprate)
YZPER037_AAPEEO$Prev_Salary=ifelse(is.na(YZPER037_AAPEEO$Prev_Salary),0,YZPER037_AAPEEO$Prev_Salary)
colnames(YZPER037_AAPEEO)
#Columns to be converted to date
date_vars<-c("EffDt","Sal Inc EffDt","Hire Dt","Rehire Dt","Term Dt","Birth Dt","Pos_Entry_Dt")
#Character to date conversion function
date_Convert<-function(date_col){
  as.Date(date_col,"%m/%d/%Y")
}
#Converting to date
YZPER037_AAPEEO[,(date_vars):=lapply(.SD,date_Convert),.SDcols = date_vars]

#Converting columns to factors
factor<-c("Lob","Deptid","Dept Name","Ethnicity","Curr Position_Title","Military Status Desc","Gender","Ethn Descr","Jobcode","Job_Title","P/F","Grade","FLSA","Billing Rate","Reg/Temp","Job Family","Curr Pos_Nbr","Military Status Code","Officer_CD","Officer_Descr","Empl_Status","City","State","Postal","Mar_Status","Supervisor Title","Disabled","Disabled Vet","No_PayHike")
YZPER037_AAPEEO[,(factor):=lapply(.SD,as.factor),.SDcols = factor]

#Creating a new dataset for usage in aura analysis-benchmarking as baseline to actual hours clocked through tenure
EmpStd_Hours=YZPER037_AAPEEO[,.(Emplid,Std_Hours)]








