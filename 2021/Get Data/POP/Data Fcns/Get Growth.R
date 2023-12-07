
#============== Functions to estimate growth statistics

#~~~~~~~~~~~~~~~~~~~ Function to compute size-age transition matrix for 60z
get_szaa_sixties<-function(nages_M,ages_M,lenbins){

# Read in parameter values
params_60z<-read.csv(paste(path_DAT,"/Other Data/SizeAge1.csv",sep=""))

# Compute size-age transition matrix
Lbar<-params_60z$Linf*(1-exp(-params_60z$k*(ages_M-params_60z$t0)))
Lbar[nages_M]<-0.5*(Lbar[nages_M]+params_60z$Linf)
SD_Lbar<-params_60z$a*log(ages_M)+params_60z$b
SzA_60z<-matrix(nrow=nages_M,ncol=length(lenbins))
for(j in 1:nages_M){
SzA_60z[j,1]<-pnorm(lenbins[1]+0.5,Lbar[j],SD_Lbar[j])
for(i in 2:(length(lenbins)-1)){
SzA_60z[j,i]<-pnorm(lenbins[i]+0.5,Lbar[j],SD_Lbar[j])-pnorm(lenbins[i-1]+0.5,Lbar[j],SD_Lbar[j])}}
SzA_60z[,length(lenbins)]<-1-rowSums(SzA_60z,na.rm=TRUE)
SzA_60z<-round(SzA_60z,digits=4)

SzA_60z}

#~~~~~~~~~~~~~~~~~~~ Function to compute current size-age transition matrix
get_szaa_current<-function(username,password,species_code,nages_M,ages_M,lenbins){

### Get age and length data
library(RODBC)
channel=odbcConnect("afsc",uid=username,pwd=password,believeNRows=FALSE)
# Age data
age_data<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species_code,") AND ((RACEBASE.SPECIMEN.REGION)='GOA') AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# Remove redundant columns
age_data<-age_data[,-which(substr(names(age_data),nchar(names(age_data))-1,nchar(names(age_data)))==".1")]
age_data<-age_data[,-which(substr(names(age_data),nchar(names(age_data))-1,nchar(names(age_data)))==".2")]
# Subset to data after 1990
age_data<-subset(age_data,age_data$YEAR>=1990)
# Remove NA's from ages
r<-which(is.na(age_data$AGE)==TRUE)
if(length(r)>0){
age_data<-age_data[-r,]}
# Length data
length_data<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='GOA') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species_code,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# Remove redundant columns
length_data<-length_data[,-which(substr(names(length_data),nchar(names(length_data))-1,nchar(names(length_data)))==".1")]
length_data<-length_data[,-which(substr(names(length_data),nchar(names(length_data))-1,nchar(names(length_data)))==".2")]
# Subset to data after 1990
length_data<-subset(length_data,length_data$YEAR>=1990)
# Write data
write.csv(age_data,paste(path_UD,"/Raw Assessment Data/growth/AL_data.csv",sep=""))
write.csv(length_data,paste(path_UD,"/Raw Assessment Data/growth/Lfreq_data.csv",sep=""))

### Compute observed length-at-age statistics
# Get parameters
ages<-sort(unique(age_data$AGE))
nages<-length(ages)
lengths<-sort(unique(age_data$LENGTH))
nlengths<-length(lengths)
# Subset to ages with >1 obs
n_a<-table(age_data$AGE)
r<-which(n_a<2)
if(length(r)>0){
n_a<-n_a[-r]}
ages<-as.numeric(names(n_a))
nages<-length(ages)
age_data_1<-NULL
for(a in 1:nages){
t<-subset(age_data,age_data$AGE==ages[a])
age_data_1<-rbind(age_data_1,t)}
lengths<-sort(unique(age_data_1$LENGTH))
nlengths<-length(lengths)
# Get Age-length key together
n_al<-table(age_data_1$AGE,age_data_1$LENGTH)
n_l<-colSums(n_al)
N_l<-matrix(nrow=nlengths)
rownames(N_l)<-lengths
for(l in 1:nlengths){
N_l[l,1]<-sum(subset(length_data$FREQUENCY,length_data$LENGTH==lengths[l]))}
N_al<-matrix(0,nrow=nages,ncol=nlengths)
rownames(N_al)<-ages
colnames(N_al)<-lengths
for(l in 1:nlengths){
N_al[,l]<-n_al[,l]/n_l[l]*N_l[l]}
# Get/compile observed length-at-age statistics
Age<-ages
SS<-vector(length=nages)
Lbar<-vector(length=nages)
SD_Lbar<-vector(length=nages)
for(a in 1:nages){
SS[a]<-length(subset(age_data_1$LENGTH,age_data_1$AGE==ages[a]))
Lbar[a]<-sum(N_al[a,]*lengths)/sum(N_al[a,])*0.1
SD_Lbar[a]<-sqrt(1/(sum(N_al[a,])-1)*sum(N_al[a,]*(lengths/10-Lbar[a])^2))}
LaA_stats<-as.data.frame(cbind(Age,SS,Lbar,SD_Lbar))
r<-which(LaA_stats$SD_Lbar<=0.01)
if(length(r)>0){
LaA_stats<-LaA_stats[-r,]}
# Write data
write.csv(LaA_stats,paste(path_UD,"/Raw Assessment Data/growth/LaA_stats.csv",sep=""))

### Estimate length-at-age statistics
# Estimate mean length
DAT<-c("# Data file for LVB model of mean length",
"# Number of ages (nages)",
length(LaA_stats$Age),
"# Ages with observed mean length (ages)",
paste(LaA_stats$Age,collapse=" "),
"# Observed mean length (Lbar_obs)",
paste(LaA_stats$Lbar,collapse=" "),
"# SD in Observed mean length (Lbar_obs)",
paste(LaA_stats$SD_Lbar,collapse=" "))
write.table(DAT,file=paste(path_MDL,"/Lbar@A/LVB.DAT",sep=""),,quote=F,,,,,row.names=F,col.names=F)
setwd(paste(path_MDL,"/Lbar@A",sep=""))
shell("LVB.EXE")
REP_Lbar<-readLines(paste(path_MDL,"/Lbar@A/LVB.REP",sep=""),warn=FALSE)
Linf<-as.numeric(strsplit(REP_Lbar[grep("Linf",REP_Lbar)[1]]," ")[[1]][2])
k<-as.numeric(strsplit(REP_Lbar[grep("k",REP_Lbar)[1]]," ")[[1]][2])
t0<-as.numeric(strsplit(REP_Lbar[grep("t0",REP_Lbar)[1]]," ")[[1]][2])
# Estimate SD in mean length
DATs<-c("# Data file for logarithmic model of SD in mean length",
"#Number of obs",
length(LaA_stats$Age),
"#Age vector",
paste(LaA_stats$Age,collapse=" "),
"#SD Length vector",
paste(LaA_stats$SD_Lbar,collapse=" "),
"#Sample size vector",
paste(LaA_stats$SS,collapse=" "))
write.table(DATs,file=paste(path_MDL,"/SD Sz@A/SD.DAT",sep=""),,quote=F,,,,,row.names=F,col.names=F)
setwd(paste(path_MDL,"/SD Sz@A",sep=""))
shell("SD.EXE")
STD_SD<-read.delim(paste(path_MDL,"/SD Sz@A/SD.STD",sep=""),sep="")
a<-STD_SD$value[1]
b<-STD_SD$value[2]
Lbar_params<-cbind(Linf,k,t0,a,b)
write.csv(Lbar_params,paste(path_UD,"/Raw Assessment Data/growth/Lbar_params.csv",sep=""))

# Compute current size-age transition matrix
Lbar<-Linf*(1-exp(-k*(ages_M-t0)))
Lbar[nages_M]<-0.5*(Lbar[nages_M]+Linf)
SD_Lbar<-a*log(ages_M)+b
SzA_curr<-matrix(nrow=nages_M,ncol=length(lenbins))
for(j in 1:nages_M){
SzA_curr[j,1]<-pnorm(lenbins[1]+0.5,Lbar[j],SD_Lbar[j])
for(i in 2:(length(lenbins)-1)){
SzA_curr[j,i]<-pnorm(lenbins[i]+0.5,Lbar[j],SD_Lbar[j])-pnorm(lenbins[i-1]+0.5,Lbar[j],SD_Lbar[j])}}
SzA_curr[,length(lenbins)]<-1-rowSums(SzA_curr,na.rm=TRUE)
SzA_curr<-round(SzA_curr,digits=4)

SzA_curr}


#~~~~~~~~~~~~~~~~~~~ Function to compute current mean weight
get_waa<-function(nages_M,ages_M){

# Read in data
age_data_raw<-read.csv(paste(path_UD,"/Raw Assessment Data/growth/AL_data.csv",sep=""))
length_data_raw<-read.csv(paste(path_UD,"/Raw Assessment Data/growth/Lfreq_data.csv",sep=""))

# Get parameters
ages<-sort(unique(age_data_raw$AGE))
nages<-length(ages)
lengths<-sort(unique(age_data_raw$LENGTH))
nlengths<-length(lengths)
# Subset to ages with >1 obs
n_a<-table(age_data_raw$AGE)
r<-which(n_a<2)
if(length(r)>0){
n_a<-n_a[-r]}
ages<-as.numeric(names(n_a))
nages<-length(ages)
age_data_1<-NULL
for(a in 1:nages){
t<-subset(age_data_raw,age_data_raw$AGE==ages[a])
age_data_1<-rbind(age_data_1,t)}
# Get Age-length key together
n_al<-table(age_data_1$AGE,age_data_1$LENGTH)
n_l<-colSums(n_al)
r<-which(n_l<2)
if(length(r)>0){
n_l<-n_l[-r]
n_al<-n_al[,-r]}
lengths<-as.numeric(names(n_l))
nlengths<-length(lengths)
N_l<-matrix(nrow=nlengths)
rownames(N_l)<-lengths
for(l in 1:nlengths){
N_l[l,1]<-sum(subset(length_data_raw$FREQUENCY,length_data_raw$LENGTH==lengths[l]))}
N_al<-matrix(0,nrow=nages,ncol=nlengths)
rownames(N_al)<-ages
colnames(N_al)<-lengths
for(l in 1:nlengths){
N_al[,l]<-n_al[,l]/n_l[l]*N_l[l]}
# Get mean weight and r age-length key together
Wbar_la<-matrix(NA,nrow=nages,ncol=nlengths)
rownames(Wbar_la)<-ages
colnames(Wbar_la)<-lengths
r_la<-matrix(NA,nrow=nages,ncol=nlengths)
rownames(r_la)<-ages
colnames(r_la)<-lengths
V_Wbar_la<-matrix(NA,nrow=nages,ncol=nlengths)
rownames(V_Wbar_la)<-ages
colnames(V_Wbar_la)<-lengths
V_r_la<-matrix(NA,nrow=nages,ncol=nlengths)
rownames(V_r_la)<-ages
colnames(V_r_la)<-lengths
theta_la<-matrix(NA,nrow=nages,ncol=nlengths)
rownames(theta_la)<-ages
colnames(theta_la)<-lengths
theta_a<-vector(length=nages)
alpha_l<-vector(length=nlengths)
for(a in 1:nages){
for(l in 1:nlengths){
awl_data<-subset(age_data_1,age_data_1$AGE==ages[a] & age_data_1$LENGTH==lengths[l])
if(length(awl_data$WEIGHT)>0){
Wbar_la[a,l]<-mean(awl_data$WEIGHT,na.rm=TRUE)
if(length(awl_data$WEIGHT)>1){
V_Wbar_la[a,l]<-var(awl_data$WEIGHT,na.rm=TRUE)/length(awl_data$WEIGHT)}}
alpha_l[l]<-N_l[l]/sum(N_l)
theta_la[a,l]<-n_al[a,l]/sum(n_al[,l])
r_la[a,l]<-alpha_l[l]*theta_la[a,l]}
theta_a[a]<-sum(r_la[a,])}
L<-sum(N_l)
A_l<-colSums(n_al)
for(a in 1:nages){
for(l in 1:nlengths){
V_r_la[a,l]<-alpha_l[l]^2*theta_la[a,l]*(1-theta_la[a,l])/(A_l[l]-1)+alpha_l[l]*(theta_la[a,l]-theta_a[a])^2/L}}

# Get/compile weight-at-age statistics
Age<-ages
SS<-vector(length=nages)
Wbar<-vector(length=nages)
SD_Wbar<-vector(length=nages)
for(a in 1:nages){
SS[a]<-length(subset(age_data_1$WEIGHT,age_data_1$AGE==ages[a]))
Wbar[a]<-sum(r_la[a,]*Wbar_la[a,],na.rm=TRUE)/sum(r_la[a,])
SD_Wbar[a]<-sqrt(sum(r_la[a,]^2*V_Wbar_la[a,]+(Wbar_la[a,]-Wbar[a])^2*V_r_la[a,],na.rm=TRUE)/theta_a[a]^2)*sqrt(length(subset(age_data_1$WEIGHT,age_data_1$AGE==ages[a])))}
WaA_stats<-as.data.frame(cbind(Age,SS,Wbar,SD_Wbar))
r<-which(WaA_stats$SD_Wbar==0)
WaA_stats<-WaA_stats[-r,]
r<-which(WaA_stats$SS<30)
WaA_stats<-WaA_stats[-r,]
# Write data
write.csv(WaA_stats,paste(path_UD,"/Raw Assessment Data/growth/WaA_stats.csv",sep=""))

# Get/compile weight-at-length statistics
lw_data<-cbind(age_data_raw$LENGTH,age_data_raw$WEIGHT)
r<-which(is.na(lw_data[,2])==TRUE)
if(length(r)>0){
lw_data<-lw_data[-r,]}
lengths<-sort(unique(lw_data[,1]))
lw_mdl_data<-matrix(nrow=length(lengths),ncol=3)
colnames(lw_mdl_data)<-c("Length","Wbar","SD_Wbar")
lw_mdl_data[,1]<-lengths
for(l in 1:length(lengths)){
q<-subset(lw_data[,2],lw_data[,1]==lengths[l])
lw_mdl_data[l,2]<-mean(q)
lw_mdl_data[l,3]<-sd(q)}
r<-which(is.na(lw_mdl_data[,3])==TRUE)
if(length(r)>0){
lw_mdl_data<-lw_mdl_data[-r,]}
r<-which(lw_mdl_data[,3]==0)
if(length(r)>0){
lw_mdl_data<-lw_mdl_data[-r,]}
# Write data
write.csv(lw_mdl_data,paste(path_UD,"/Raw Assessment Data/growth/WaL_stats.csv",sep=""))

# Run allometric model
DAT<-c("# Data file for allometric model of mean weight by length",
"# Number of lengths (nlengths)",
length(lw_mdl_data[,1]),
"# Lengths with observed mean weight (lengths)",
paste(lw_mdl_data[,1],collapse=" "),
"# Observed mean weight (Wbar_obs)",
paste(lw_mdl_data[,2],collapse=" "),
"# SD in Observed mean weight (SD_Wbar)",
paste(lw_mdl_data[,3],collapse=" "))
write.table(DAT,file=paste(path_MDL,"/Wbar@A/ALLO.DAT",sep=""),,quote=F,,,,,row.names=F,col.names=F)
setwd(paste(path_MDL,"/Wbar@A",sep=""))
shell("ALLO.EXE")
PAR<-readLines(paste(path_MDL,"/Wbar@A/allo.par",sep=""),warn=FALSE)
beta_lw<-as.numeric(strsplit(PAR[grep("beta",PAR)+1]," ")[[1]])

# Run LVBmodel and estimate mean weight
PIN<-c("# Parameter starting values for LVB model of mean weight","# Winf","800","# k","0.1","# t0","0","# beta",as.character(beta_lw))
write.table(PIN,file=paste(path_MDL,"/Wbar@A/LVB.PIN",sep=""),,quote=F,,,,,row.names=F,col.names=F)
DAT<-c("# Data file for LVB model of mean weight",
"# Number of ages (nages)",
length(WaA_stats$Age),
"# Ages with observed mean weight (ages)",
paste(WaA_stats$Age,collapse=" "),
"# Observed mean weight (Wbar_obs)",
paste(WaA_stats$Wbar,collapse=" "),
"# SD in Observed mean weight (Wbar_obs)",
paste(WaA_stats$SD_Wbar,collapse=" "))
write.table(DAT,file=paste(path_MDL,"/Wbar@A/LVB.DAT",sep=""),,quote=F,,,,,row.names=F,col.names=F)
shell("LVB.EXE")
REP<-readLines(paste(path_MDL,"/Wbar@A/lvb.rep",sep=""),warn=FALSE)
Winf<-as.numeric(strsplit(REP[grep("Winf",REP)[1]]," ")[[1]][2])
k<-as.numeric(strsplit(REP[grep("k",REP)[1]]," ")[[1]][2])
t0<-as.numeric(strsplit(REP[grep("t0",REP)[1]]," ")[[1]][2])
Wbar<-Winf*(1-exp(-k*(ages_M-t0)))^beta_lw
Wbar[nages_M]<-0.5*(Wbar[nages_M]+Winf)
Wbar<-round(Wbar,digits=1)
Wbar_params<-cbind(Winf,k,t0,beta_lw)
write.csv(Wbar_params,paste(path_UD,"/Raw Assessment Data/growth/Wbar_params.csv",sep=""))

Wbar}