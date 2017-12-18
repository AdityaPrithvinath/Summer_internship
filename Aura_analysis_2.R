#getwd()
#file_path=file.choose()
file_path=readline("Enter the path of the aura report csv for sheet 1(Replace Backslash with a frontslash) : ")
require(ggplot2)
install.packages("plotly")
require(plotly)
aura_chicago=fread(file_path)
remove(file_path)
file_path=readline("Enter the path of the aura report csv for sheet 2(Replace Backslash with a frontslash) : ")
#file_path_2 = file.choose()
aura_chicago_2=fread(file_path)
aura_chicago=rbind(aura_chicago,aura_chicago_2)
remove(aura_chicago_2)
aura_chicago[,V12:=NULL]
str(aura_chicago)
aura_chicago$Week=as.Date(aura_chicago$Weekend,"%b%d/%y")
aura_chicago[,Weekend:=NULL]

#Number of Weeks of Data Captured in the report
first_week=head(unique(aura_chicago$Week),n = 1L)
last_week=tail(unique(aura_chicago$Week),n= 1L)
print(paste0("Date Range for data captured is ", first_week," to ",last_week))



aura_chicago$Emp=substr(aura_chicago$EmplId,7,12)
aura_chicago[,EmplId:=NULL]
colnames(aura_chicago)[11]="Emplid"
#aura_chicago$Emplid=as.integer(aura_chicago$Emplid)
colnames(aura_chicago)[8]="Billable_Effort"
#Subsetting to exclude vacation time from the data
Work_Effort<-aura_chicago[Non_Client=="",]

#Subsetting for non billable effort logged
Vacation_Hours<-aura_chicago[Non_Client!="",]
#Filtering vacation data to exclude Office Holidays
Vacation_Hours=Vacation_Hours[Non_Client != "HOLIDAY HOLIDAY" & Non_Client != "CLOSED EARLY OR EMERGENCY CLOSING OF OFFICE",]
#table(Vacation_Hours$Non_Client)
Vacation_Hours$Non_Client=as.factor(Vacation_Hours$Non_Client)
#Types of vacation
levels(Vacation_Hours$Non_Client)

#Plotting vacation information
#Vacation_Hours[,.(Hours_Vacation=sum(Hours)),by=.(Emplid)]

#Creating Month and Year variable to be used while aggregating.
Vacation_Hours$month=month(cut(Vacation_Hours$Week,breaks="month"))
Vacation_Hours$year=year(cut(Vacation_Hours$Week,breaks="year"))

#Different Non-Billable Effort logged by month and year
vacation_time=Vacation_Hours[order(month,year),.(Total_Leave_Hours=sum(Hours)),by=.(Non_Client,month,year)]

#Subsetting only for Vacation
vacation_time=vacation_time[Non_Client=="VACDAY VACATION",]
vacation_time$year=as.factor(vacation_time$year)
#temp3$month= factor(temp3$month,levels=month.abb)
#Plotting overall Vacation Hours by months and year
p=ggplot(data = vacation_time,aes(x=month.abb[month], y=Total_Leave_Hours, colour=year, group=year))  + geom_point( size=2, shape=21, fill="white") +  scale_x_discrete(limits = month.abb) +geom_line() +   xlab("") + ylab("Vacation Hours Per Year")
ggplotly(p)


#temp3$date=as.Date(paste(month.abb[temp3$month] ,temp3$year , sep = "-")  , format = "%b-01-%Y" )

#Subset the training time by Employee
Training_time=Vacation_Hours[Non_Client == "TRAIN TRAINING",.(Training_Hours=sum(Hours)),by=.(Emplid)]
p=ggplot(data=Training_time,aes(x=Emplid ,y=Training_Hours))+geom_point()+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())
ggplotly(p)

# Computing Average Work hours clocked by each employee
Emp_Avg_hours<-Work_Effort[,.(Avg_hours=sum(Hours)),by=.(Emplid,Week)]
Emp_Avg_hours=Emp_Avg_hours[,.(Avg_hrs_week=mean(Avg_hours)),by=.(Emplid)]
Emp_Avg_hours$Emplid=as.integer(Emp_Avg_hours$Emplid)
#Emp_Avg_hours$Avg_hrs_week=as.integer(Emp_Avg_hours$Avg_hrs_week)
#plotting Average Hours Density graph
plot(density(Emp_Avg_hours$Avg_hrs_week))


Work_Effort$Emplid=as.integer(Work_Effort$Emplid)
temp=Work_Effort[order(Emplid,Week),.(Sum_hrs=sum(Hours)),by=.(Week,Emplid)]
temp$month=as.Date(cut(temp$Week,breaks="month"))
temp$year=year(cut(temp$Week,breaks="year"))
temp$month=month(temp$month)
temp=merge(temp,EmpStd_Hours,by="Emplid")
temp2=temp[,.(Avg_Hrs_Month=mean(Sum_hrs)),by=.(Emplid,month,year)]
temp2=merge(temp2,EmpStd_Hours,by="Emplid")
temp2=subset(temp2,Emplid =="047941")

#temp$month=as.factor(temp$month)
#temp$year=as.factor(temp$year)

str(temp)
#plot-work effort by Emplid 
Work_emp_plot=function(dt,employee){
  dt=subset(dt, Emplid == employee)
  p=ggplot(dt, aes(x=Week, y=Sum_hrs)) + geom_line() + scale_x_date(date_breaks = "3 month", date_labels = "%b-%y") +geom_hline(yintercept = dt$Std_Hours ) +  xlab("") + ylab("Total Hours Clocked per Week")
  ggplotly(p)
}
Work_emp_plot(temp,620365)

#aura_chicago[Client_Name != "" & Billable_Effort == "N",Client_Name]
Num_clients_Emp<-Work_Effort[,.(Num_clients=length(unique(Client_Name))),by=.(Emplid)]

temp=Work_Effort[,.(Total_hrs_Client=sum(Hours)),by=.(Emplid,Client_Name)][,.(Avg_Hrs_Client=mean(Total_hrs_Client)),by=.(Emplid)]
Num_clients_Emp=merge(Num_clients_Emp,temp,by="Emplid")
Num_clients_Emp$Emplid=as.integer(Num_clients_Emp$Emplid)
#Num_clients_Emp$Avg_Hrs_Client=as.integer(Num_clients_Emp$Avg_Hrs_Client)
remove(temp)
#head(aura_chicago[])



write.csv(aura_chicago,file = paste(source_dir,"/Aura_R_Report.csv",sep=""),row.names=FALSE, na="")
