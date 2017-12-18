file_path=readline("Please enter the full Path for the YZPER009_ActivewithYrsofSvc report csv file(Replace backslashes with a frontslash): ")
YZPER009_ActivewithYrsofSvc=fread(file_path)
remove(file_path)
str(YZPER009_ActivewithYrsofSvc)
colnames(YZPER009_ActivewithYrsofSvc)
colnames(YZPER009_ActivewithYrsofSvc)[11]<-"Years_BQuit"
colnames(YZPER009_ActivewithYrsofSvc)[13]<-"Years_RehQuit"
#YZPER009_ActivewithYrsofSvc$Years_BQuit=as.integer(YZPER009_ActivewithYrsofSvc$Years_BQuit)
#YZPER009_ActivewithYrsofSvc$Years_RehQuit=as.integer(YZPER009_ActivewithYrsofSvc$Years_RehQuit)
Merge_YZPER009_ActivewithYrsofSvc=YZPER009_ActivewithYrsofSvc[,.(Emplid,Years_BQuit,Years_RehQuit)]
