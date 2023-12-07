
#============== Function to estimate ageing error matrix
get_ageage<-function(norpac_species,recage,nages_D){

# Read in, subset data to species and region
ae_DAT<-read.csv(paste(path_DAT,"/Other Data/reader_tester.csv",sep=""))
ae_DAT<-subset(ae_DAT,ae_DAT$Species==norpac_species)
ae_DAT<-subset(ae_DAT,ae_DAT$Region=="GOA")

# Send reader-tester agreement data to ADMB and estimate ageing error SDs
r<-ae_DAT$Read_Age
t<-ae_DAT$Test_Age
f<-ae_DAT$Final_Age
DAT<-as.data.frame(cbind(r,t,f))
x_r<-which(DAT$r<0)
x_t<-which(DAT$t<0)
x_f<-which(DAT$f<0)
if(length(c(x_r,x_t,x_f))>0){DAT<-DAT[-c(x_r,x_t,x_f),]}
Test_Age<-DAT$t
Read_Age<-DAT$r
z_tab<-table(Test_Age,Read_Age)
z<-cbind(as.numeric(rownames(z_tab)),seq(as.numeric(colnames(z_tab))[1],as.numeric(colnames(z_tab))[1],length.out=length(rownames(z_tab))),z_tab[,1])
colnames(z)<-c("Test_Age","Read_Age","Freq")
for(c in 2:length(z_tab[1,])){
t<-cbind(as.numeric(rownames(z_tab)),seq(as.numeric(colnames(z_tab))[c],as.numeric(colnames(z_tab))[c],length.out=length(rownames(z_tab))),z_tab[,c])
z<-rbind(z,t)}
rownames(z)<-seq(1:length(z[,1]))
z<-as.data.frame(z)
z$AGREE<-z$Test_Age==z$Read_Age
ape<-(1-(tapply(z$Freq[z$AGREE==FALSE],z$Test_Age[z$AGREE==FALSE],sum))/tapply(z$Freq,z$Test_Age,sum))*100
ss<-tapply(z$Freq,z$Test_Age,sum)
perc_ag<-data.frame(cbind(ape,ss))
age<-seq(as.numeric(rownames(perc_ag)[1]),as.numeric(rownames(perc_ag)[length(perc_ag[,1])]))
Data<-matrix(nrow=length(age),ncol=3)
colnames(Data)<-c("a","ape","ss")
Data[,1]<-age
for(a in 1:length(age)){
r<-which(as.numeric(rownames(perc_ag))==age[a])
if(length(r)==1){
Data[a,2]<-perc_ag$ape[r]/100
Data[a,3]<-perc_ag$ss[r]}
if(length(r)==0){
Data[a,2]<-(-9)
Data[a,3]<-(-9)}}
DATs<-c("#Number of obs",length(age),"#Age vector",Data[,1],"#Percent agreement vector",Data[,2],"#Sample size vector",Data[,3])
write.table(DATs,file=paste(path_MDL,"/SD A@A/ageage.DAT",sep=""),,quote=F,,,,,row.names=F,col.names=F)
setwd(paste(path_MDL,"/SD A@A",sep=""))
shell("ageage.EXE")
STD<-read.delim(paste(path_MDL,"/SD A@A/ageage.STD",sep=""),sep="")

# Calculate ageing error matrix out to age=100
age_sd<-cbind(age,STD$value[3:(length(age)+2)])
colnames(age_sd)<-c("Age","SD")
SDs<-lm(age_sd[,2]~age_sd[,1])

ages<-seq(recage,100)
ages_sd<-cbind(ages,coef(SDs)[1]+coef(SDs)[2]*ages)
ae_mtx_100<-matrix(nrow=length(ages),ncol=length(ages))
colnames(ae_mtx_100)<-ages
rownames(ae_mtx_100)<-ages
for(j in 1:length(ages)){
ae_mtx_100[j,1]<-pnorm(ages[1]+0.5,ages[j],ages_sd[which(ages_sd[,1]==ages[j]),2])}
for(i in 2:(length(ages)-1)){
for(j in 1:length(ages)){
ae_mtx_100[j,i]<-pnorm(ages[i]+0.5,ages[j],ages_sd[which(ages_sd[,1]==ages[j]),2])-pnorm(ages[i-1]+0.5,ages[j],ages_sd[which(ages_sd[,1]==ages[j]),2])}}
for(j in 1:length(ages)){
ae_mtx_100[j,length(ages)]<-1-sum(ae_mtx_100[j,1:(length(ages)-1)])}
write.csv(ae_mtx_100,paste(path_UD,"/Raw Assessment Data/ageing error/POP_ae_mtx_100.csv",sep=""))
write.csv(age_sd,paste(path_UD,"/Raw Assessment Data/ageing error/POP_age_SD.csv",sep=""))

# Compute ageing error matrix for model
ae_Mdl<-matrix(nrow=length(ages),ncol=nages_D)
ae_Mdl[,1:(nages_D-1)]<-as.matrix(ae_mtx_100[,1:(nages_D-1)])
ae_Mdl[,nages_D]<-rowSums(ae_mtx_100[,nages_D:length(ages)])
ae_Mdl<-round(ae_Mdl,digits=4)
r<-which(ae_Mdl[,nages_D]>=0.999)[1]
ae_Mdl<-ae_Mdl[1:r,]

ae_Mdl}

