getwd()
file_path<-file.choose()
#install.packages("data.table")
require(data.table)
survey_data<-fread(file_path)
str(survey_data)

colnames(survey_data)

# survey_data[,testind:=ifelse(testind =='',0,1),with=FALSE]
survey_data[,25:34][(survey_data[,25:34])==""] <- 0
survey_data[,25:34][(survey_data[,25:34])!=0] <- 1



factor_coerce<-c("Age","Gender","Rel_Status","Children","City","Donate","Donation_Mode","Motivating_fct_1","Motivating_fct_2","Motivating_fct_3","Motivating_fct_4","Motivating_fct_5","Motivating_fct_6",
          "Friends/Family","Social Media","Advertisement","Internet Search")

survey_data[,(factor_coerce):=lapply(.SD, as.factor),.SDcols=factor_coerce]



# for (col in c("Motivating_fct_1","Motivating_fct_2","Motivating_fct_3","Motivating_fct_4","Motivating_fct_5","Motivating_fct_6",
#              "Friends/Family","Social Media","Advertisement","Internet Search")){
#   survey_data[,col:=ifelse(col == "", 0, 1)]
# }
attach(survey_data)
# survey_data[,Motivating_fct_1:=ifelse(Motivating_fct_1=="",0,1)]  
# survey_data[,Motivating_fct_2:=ifelse(Motivating_fct_2=="",0,1)]  
# survey_data[,Motivating_fct_3:=ifelse(Motivating_fct_3=="",0,1)]  
# survey_data[,Motivating_fct_4:=ifelse(Motivating_fct_4=="",0,1)]  
# survey_data[,Motivating_fct_5:=ifelse(Motivating_fct_5=="",0,1)]  
# survey_data[,Motivating_fct_6:=ifelse(Motivating_fct_6=="",0,1)]  
# survey_data[,`Friends/Family`:=ifelse(`Friends/Family`=="",0,1)]
# survey_data[,`Social Media`:=ifelse(`Social Media`=="",0,1)]
# survey_data[,Advertisement:=ifelse(Advertisement=="",0,1)]
# survey_data[,`Internet Search`:=ifelse(`Internet Search`=="",0,1)]

survey_data[,8:12][is.na(survey_data[,8:12])==TRUE]<-0

survey_data$Education<-round(Education/(Education+`Health Care`+Environment+`Animal Welfare`+`Human Rights`),2)
survey_data$`Health Care`<-round(`Health Care`/(Education+`Health Care`+Environment+`Animal Welfare`+`Human Rights`),2)
survey_data$Environment<-round(Environment/(Education+`Health Care`+Environment+`Animal Welfare`+`Human Rights`),2)
survey_data$`Animal Welfare`<-round(`Animal Welfare`/(Education+`Health Care`+Environment+`Animal Welfare`+`Human Rights`),2)
survey_data$`Human Rights`<-round(`Human Rights`/(Education+`Health Care`+Environment+`Animal Welfare`+`Human Rights`),2)

survey_data[,13:18][is.na(survey_data[,13:18])==TRUE]<-0
ff<-survey_data[9,]
round(ff$Socially_Drv_challenge/(ff$Socially_Drv_challenge+ff$Checkout_Charity+ff$Crowdsourcing+ff$Charity_Run+ff$`Volunteer Work`+ff$`Peer-to-Peer`),2)

survey_data$Socially_Drv_challenge<-round(Socially_Drv_challenge/(Socially_Drv_challenge + Checkout_Charity + Crowdsourcing + Charity_Run + `Volunteer Work` + `Peer-to-Peer`),2)
survey_data[is.na(Socially_Drv_challenge)==TRUE,]<-0
survey_data$Checkout_Charity<-round(Checkout_Charity/(Socially_Drv_challenge+Checkout_Charity+Crowdsourcing+Charity_Run+`Volunteer Work`+`Peer-to-Peer`),2)
survey_data$Crowdsourcing<-round(Crowdsourcing/(Socially_Drv_challenge+Checkout_Charity+Crowdsourcing+Charity_Run+`Volunteer Work`+`Peer-to-Peer`),2)
survey_data$Charity_Run<-round(Charity_Run/(Socially_Drv_challenge+Checkout_Charity+Crowdsourcing+Charity_Run+`Volunteer Work`+`Peer-to-Peer`),2)
survey_data$`Volunteer Work`<-round(`Volunteer Work`/(Socially_Drv_challenge+Checkout_Charity+Crowdsourcing+Charity_Run+`Volunteer Work`+`Peer-to-Peer`),2)
survey_data$`Peer-to-Peer`<-round(`Peer-to-Peer`/(Socially_Drv_challenge+Checkout_Charity+Crowdsourcing+Charity_Run+`Volunteer Work`+`Peer-to-Peer`),2)
# testind<-survey_data[,grep("Motivating_fct_", colnames(survey_data))]



complete<-complete.cases(survey_data)
complete<-survey_data[complete]
str(complete)

complete[, Household_Income:=replace(Household_Income,(Household_Income == 0),as.integer(median(Household_Income))), by=.(Age,Gender)]

complete[,Scaled_Income:=((Household_Income-mean(Household_Income))/sd(Household_Income))]

colnames(complete)
cluster_data<-complete[,c("Education","Health Care","Environment","Animal Welfare","Human Rights","Socially_Drv_challenge","Checkout_Charity",
                          "Crowdsourcing","Charity_Run","Volunteer Work","Peer-to-Peer","Motivating_fct_1","Motivating_fct_2",
                          "Motivating_fct_3","Motivating_fct_4","Motivating_fct_5","Motivating_fct_6",    
                          "Friends/Family","Social Media","Advertisement","Internet Search","Scaled_Income")]
require(cluster)
d<-daisy(cluster_data,metric = "gower")
H_clus<-hclust(d,method = "ward.D2")
plot(H_clus)
rect.hclust(H_clus, k=4, border=2:5) 
Gower_groups <- cutree(H_clus, k=4)

clusplot(cluster_data, Gower_groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Respondent segments')

clus_survey<-hclust(dist(complete[,c("Education","Health Care","Environment","Animal Welfare","Human Rights","Socially_Drv_challenge","Checkout_Charity",
                                     "Crowdsourcing","Charity_Run","Volunteer Work","Peer-to-Peer","Motivating_fct_1","Motivating_fct_2",
                                     "Motivating_fct_3","Motivating_fct_4","Motivating_fct_5","Motivating_fct_6",    
                                     "Friends/Family","Social Media","Advertisement","Internet Search","Scaled_Income")]),method = "ward.D")
clus_survey_completeDist<-hclust(dist(complete[,c("Education","Health Care","Environment","Animal Welfare","Human Rights","Socially_Drv_challenge","Checkout_Charity",
                                     "Crowdsourcing","Charity_Run","Volunteer Work","Peer-to-Peer","Motivating_fct_1","Motivating_fct_2",
                                     "Motivating_fct_3","Motivating_fct_4","Motivating_fct_5","Motivating_fct_6",    
                                     "Friends/Family","Social Media","Advertisement","Internet Search","Scaled_Income")]),method = "complete")



dend1<-as.dendrogram(clus_survey)
dend2<-as.dendrogram(clus_survey_completeDist)
#install.packages("sos")
require("sos")
findFn("tanglegram")

install.packages("dendextend")
require(dendextend)

tanglegram(dend1,dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#findFn("fviz_nbclust")
install.packages("factoextra")
require(factoextra)
fviz_nbclust(complete,FUN=hcut, method="wss")


plot(clus_survey)
plot(clus_survey_completeDist)
final_cluster<-cutree(clus_survey,4)

plot(clus_survey, cex = 0.6)
rect.hclust(clus_survey, k = 4, border = 2:5)


complete[,Cluster:= Gower_groups]

complete[,.(Mean_Salary=mean(Household_Income)),by=.(Cluster)]
table(complete$Cluster,complete$Gender)
table(complete$Cluster,complete$Children)
table(complete$Cluster,complete$Age)
complete[,.(Education=mean(Education),Health_Care=mean(`Health Care`),Environment=mean(Environment),Animal_Welfare=mean(`Animal Welfare`),Human_Rights=mean(`Human Rights`)),by=.(Cluster)]

complete[,.(Challenge=mean(Socially_Drv_challenge),Checkout_Charity=mean(Checkout_Charity),Crowdsourcing=mean(Crowdsourcing),Charity_Run=mean(Charity_Run),Volunteer=mean(`Volunteer Work`),Peer_to_Peer=mean(`Peer-to-Peer`)),by=.(Cluster)]

complete[,.(Facebook=sum(Facebook),Instagram=sum(Instagram),Twitter=sum(Twitter),Snapchat=sum(Snapchat),Pinterest=sum(Pinterest)),by=.(Cluster)]

#findFn("cast")
install.packages("reshape")
require(reshape)
Cluster_1<-complete[Cluster==1,]
Cluster_2<-complete[Cluster==2,]
Cluster_3<-complete[Cluster==3,]
Cluster_4<-complete[Cluster==4,]
cast(Cluster_1, Age~Gender+City,mean,value="Household_Income" )
cast(Cluster_2, Age~Gender+City,mean,value="Household_Income" )
cast(Cluster_3, Age~Gender+City,mean,value="Household_Income" )
cast(Cluster_4, Age~Gender+City,mean,value="Household_Income" )

anova_1<- aov(complete$Donate ~ complete$Age,data = complete)
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(anova_1) # diagnostic plots
summary(anova_1)
TukeyHSD(anova_1)
chi_Age_DMode<-table(complete$Age,complete$Donation_Mode)
chisq_Age_DMode<-chisq.test(chi_Age_DMode)
remove(chisq_Age_DMode)
  table(Gower_groups,complete$Motivating_fct_1)
aggregate(complete[,c("Household_Income")],list(final_cluster),median)

sum(complete$Socially_Drv_challenge)/nrow(complete)
sum(complete$Checkout_Charity)/nrow(complete)
sum(complete$Crowdsourcing)/nrow(complete)
sum(complete$Charity_Run)/nrow(complete)
sum(complete$`Volunteer Work`)/nrow(complete)
sum(complete$`Peer-to-Peer`)/nrow(complete)
table(survey_data$Donation_Mode)
sum(survey_data$Motivating_fct_1)


table(survey_data$Motivating_fct_1)
table(survey_data$Motivating_fct_2)
table(survey_data$Motivating_fct_3)
table(survey_data$Motivating_fct_4)
table(survey_data$Motivating_fct_5)
table(survey_data$Motivating_fct_6)


install.packages("wordcloud")
library(tm)
library(SnowballC)
library(wordcloud)
Quest <- read.csv('C:/Users/prithvinatha/Documents/Outback_Survey/Modified_Survey_Doc_1.csv', stringsAsFactors = FALSE)

qCorpus <- Corpus(VectorSource(Quest$Open.Ended.Response))

qCorpus <- tm_map(qCorpus, PlainTextDocument)
qCorpus <- tm_map(qCorpus, removePunctuation)
qCorpus <- tm_map(qCorpus, removeWords, stopwords('english'))

qCorpus <- tm_map(qCorpus, stemDocument)

qCorpus <- Corpus(VectorSource(qCorpus))
wordcloud(qCorpus, max.words = 50, random.order = FALSE)

qCorpus <- tm_map(qCorpus, removeWords, c('the', 'this', stopwords('english')))
