#file_path=file.choose()
file_path=readline("Please enter the full Path for the YZPER006_ActivewithSalary report csv file(Replace backslash with a frontslash): ")
YZPER006_ActivewithSalary=fread(file_path)
remove(file_path)
str(YZPER006_ActivewithSalary)
YZPER006_ActivewithSalary$`Job Title`=as.factor(YZPER006_ActivewithSalary$`Job Title`)
colnames(YZPER006_ActivewithSalary)[11]<-"Job_Title"

Merge_table=YZPER006_ActivewithSalary[,.(Grade,Min,Mid,Max)]
Merge_table=Merge_table[unique(Merge_table$Grade),]

