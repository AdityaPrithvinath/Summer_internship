file_path=readline("Please enter the full Path for the YZPER003_ActiveWithinRange report csv file(Replace backslashes with a frontslash): ")
YZPER003_ActiveWithinRange=fread(file_path)
remove(file_path)
str(YZPER003_ActiveWithinRange)
colnames(YZPER003_ActiveWithinRange)[4]="Emplid"
#intersect(YZPER003_ActiveWithinRange$Emplid,YZPER037_AAPEEO$Emplid)
YZPER003_ActiveWithinRange[,c("Action","Action Reason"):=lapply(.SD,as.factor),.SDcols=c("Action","Action Reason")]
Merge_YZPER003_ActiveWithinRange=YZPER003_ActiveWithinRange[,.(Emplid,Action,`Action Reason`)]
