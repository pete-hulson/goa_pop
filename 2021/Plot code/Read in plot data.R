
#####################################################################################################
######### Get directories - Read in plot data
#####################################################################################################

install.packages("extrafont")
library(extrafont)
library(ggplot2)
windowsFonts(A = windowsFont("Times New Roman"))

path<-getwd()

# Define end year of model
endyr<-2021

pathM<-paste(path,"/Pref model",sep="")

REP<-readLines(paste(pathM,"/Model_20_1.rep",sep=""),warn=FALSE)
REP_20<-readLines(paste(pathM,"/Model_20_1_20.rep",sep=""),warn=FALSE)
CTL<-readLines(paste(pathM,"/goa_pop_",endyr,".ctl",sep=""),warn=FALSE)

t<-strsplit(REP[grep("Year",REP)[1]]," ")
t<-subset(t[[1]],t[[1]]!="")
yrs<-as.numeric(t[2:length(t)])

sb<-REP[grep("Bottom Trawl Survey Biomass",REP):(grep("Alternative Survey Biomass",REP)-2)]
t<-strsplit(sb[2]," ")
t<-subset(t[[1]],t[[1]]!="")
yrs_srv1_biom<-as.numeric(t[2:length(t)])

t<-strsplit(REP[grep("Age",REP)[1]]," ")
t<-subset(t[[1]],t[[1]]!="")
ages<-as.numeric(t[2:length(t)])
styr_rec<-yrs[1]-length(ages)+2

filen<-file(paste(pathM,"/Model_20_1.psv",sep=""),"rb")
nopar<-readBin(filen,what=integer(),n=1)
mcmc<-readBin(filen,what=numeric(),n=nopar*5000)
mcmc_params<-matrix(mcmc,byrow=TRUE,ncol=nopar)
mcmc_params<-mcmc_params[501:length(mcmc_params[,1]),]
STD<-read.delim(paste(pathM,"/Model_20_1.STD",sep=""),sep="")
colnames(mcmc_params)<-STD$name[1:length(mcmc_params[1,])]
mcmc_params<-as.data.frame(mcmc_params)

mcmc_other<-read.delim(paste(pathM,"/evalout.prj",sep=""),sep="",header=FALSE)
mcmc_other<-mcmc_other[501:length(mcmc_other[,1]),]
colnames(mcmc_other)<-c("sigr","q_srv1","q_srv2","F40","natmort","ABC","obj_fun",paste("tot_biom_",yrs,sep=""),paste("log_rec_dev_",seq(styr_rec,yrs[length(yrs)]),sep=""),paste("spawn_biom_",yrs,sep=""),"log_mean_rec",paste("spawn_biom_proj_",yrs[length(yrs)]+seq(1,15),sep=""),paste("pred_catch_proj_",yrs[length(yrs)]+seq(1,15),sep=""),paste("rec_proj_",yrs[length(yrs)]+seq(1,10),sep=""),paste("tot_biom_proj_",yrs[length(yrs)]+1,sep=""),paste("tot_biom_proj_",yrs[length(yrs)]+2,sep=""),paste("srv1_biom_",yrs_srv1_biom,sep=""))
mcmc_other<-as.data.frame(mcmc_other)

# Catch
pred_catch<-strsplit(REP[grep("Pred_Catch",REP)]," ")
r1<-which(pred_catch[[1]]=="Pred_Catch")
r2<-which(pred_catch[[1]]=="Pred_catch_later")
r3<-which(pred_catch[[1]]=="")
pred_catch<-as.numeric(pred_catch[[1]][-c(r1,r2,r3)])
obs_catch<-strsplit(REP[grep("Obs_Catch",REP)]," ")
r1<-which(obs_catch[[1]]=="Obs_Catch")
r2<-which(obs_catch[[1]]=="Obs_Catch_Later")
r3<-which(obs_catch[[1]]=="")
obs_catch<-as.numeric(obs_catch[[1]][-c(r1,r2,r3)])

# Fish size composition
q_obs<-REP[grep("Obs_P_fish_size",REP):(grep("Pred_P_fish_size",REP)-2)]
q_pred<-REP[grep("Pred_P_fish_size",REP):(grep("yrs_fish_size",REP)-2)]
nyrs<-length(q_pred)-1
lenbins<-strsplit(q_obs[1]," ")
lenbins<-as.numeric(lenbins[[1]][2:length(lenbins[[1]])])
osc_fish<-matrix(nrow=nyrs,ncol=(length(lenbins)+1))
colnames(osc_fish)<-c("Year",lenbins)
esc_fish<-matrix(nrow=nyrs,ncol=(length(lenbins)+1))
colnames(esc_fish)<-c("Year",lenbins)
for(y in 1:nyrs){
t<-strsplit(q_obs[y+1]," ")
t<-subset(t[[1]],t[[1]]!="")
t<-as.numeric(t[1:(length(lenbins)+1)])
osc_fish[y,]<-t
t<-strsplit(q_pred[y+1]," ")
t<-subset(t[[1]],t[[1]]!="")
t<-as.numeric(t[1:(length(lenbins)+1)])
esc_fish[y,]<-t}

# Fish age composition
q_obs<-REP[grep("Obs_P_fish_age",REP):(grep("Pred_P_fish_age",REP)-2)]
q_pred<-REP[grep("Pred_P_fish_age",REP):(grep("yrs_fish_age",REP)-2)]
nyrs<-length(q_pred)-1
ages<-strsplit(q_obs[1]," ")
ages<-as.numeric(ages[[1]][2:length(ages[[1]])])
oac_fish<-matrix(nrow=nyrs,ncol=(length(ages)+1))
colnames(oac_fish)<-c("Year",ages)
eac_fish<-matrix(nrow=nyrs,ncol=(length(ages)+1))
colnames(eac_fish)<-c("Year",ages)
for(y in 1:nyrs){
t<-strsplit(q_obs[y+1]," ")
t<-subset(t[[1]],t[[1]]!="")
t<-as.numeric(t[1:(length(ages)+1)])
oac_fish[y,]<-t
t<-strsplit(q_pred[y+1]," ")
t<-subset(t[[1]],t[[1]]!="")
t<-as.numeric(t[1:(length(ages)+1)])
eac_fish[y,]<-t}
ages<-ages[1:(length(oac_fish[1,])-1)]

# Survey biomass
sb<-REP[grep("Bottom Trawl Survey Biomass",REP):(grep("Alternative Survey Biomass",REP)-2)]
t<-strsplit(sb[2]," ")
t<-subset(t[[1]],t[[1]]!="")
yrs_srv1_biom<-as.numeric(t[2:length(t)])
t<-strsplit(sb[3]," ")
t<-subset(t[[1]],t[[1]]!="")
pred_srv1_biom<-as.numeric(t[2:length(t)])
t<-strsplit(sb[4]," ")
t<-subset(t[[1]],t[[1]]!="")
obs_srv1_biom<-as.numeric(t[2:length(t)])
t<-strsplit(sb[5]," ")
t<-subset(t[[1]],t[[1]]!="")
se_srv1_biom<-as.numeric(t[2:length(t)])
uci_srv1_biom<-obs_srv1_biom+1.96*se_srv1_biom
lci_srv1_biom<-obs_srv1_biom-1.96*se_srv1_biom
r<-which(lci_srv1_biom<0)
lci_srv1_biom[r]<-0
sb_20<-REP_20[grep("Survey Biomass",REP_20)[1]:(grep("Survey Biomass",REP_20)[2]-2)]
t<-strsplit(sb_20[2]," ")
t<-subset(t[[1]],t[[1]]!="")
yrs_srv1_biom_20<-as.numeric(t[2:length(t)])
t<-strsplit(sb_20[3]," ")
t<-subset(t[[1]],t[[1]]!="")
pred_srv1_biom_20<-as.numeric(t[2:length(t)])

# Survey age composition
q_obs<-REP[grep("Obs_P_srv1_age",REP):(grep("Pred_P_srv1_age",REP)-2)]
q_pred<-REP[grep("Pred_P_srv1_age",REP):(grep("yrs_srv1_age",REP)-2)]
nyrs<-length(q_pred)-1
ages<-strsplit(q_obs[1]," ")
ages<-as.numeric(ages[[1]][2:length(ages[[1]])])
oac_srv1<-matrix(nrow=nyrs,ncol=(length(ages)+1))
colnames(oac_srv1)<-c("Year",ages)
eac_srv1<-matrix(nrow=nyrs,ncol=(length(ages)+1))
colnames(eac_srv1)<-c("Year",ages)
for(y in 1:nyrs){
t<-strsplit(q_obs[y+1]," ")
t<-subset(t[[1]],t[[1]]!="")
t<-as.numeric(t[1:(length(ages)+1)])
oac_srv1[y,]<-t
t<-strsplit(q_pred[y+1]," ")
t<-subset(t[[1]],t[[1]]!="")
t<-as.numeric(t[1:(length(ages)+1)])
eac_srv1[y,]<-t}
ages<-ages[1:(length(oac_srv1[1,])-1)]

# Survey size composition
q_obs<-REP[grep("Obs_P_srv1_size",REP):(grep("Pred_P_srv1_size",REP)-2)]
nyrs<-length(q_obs)-1
lenbins<-strsplit(q_obs[1]," ")
lenbins<-as.numeric(lenbins[[1]][2:length(lenbins[[1]])])
osc_srv1<-matrix(nrow=nyrs,ncol=(length(lenbins)+1))
colnames(osc_srv1)<-c("Year",lenbins)
for(y in 1:nyrs){
t<-strsplit(q_obs[y+1]," ")
t<-subset(t[[1]],t[[1]]!="")
t<-as.numeric(t[1:(length(lenbins)+1)])
osc_srv1[y,]<-t}

# Total biomass
t<-strsplit(REP[grep("Tot_biom",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
tot_biom<-as.numeric(t[2:length(t)])
t<-strsplit(REP_20[grep("Tot_biom",REP_20)]," ")
t<-subset(t[[1]],t[[1]]!="")
tot_biom_20<-as.numeric(t[2:length(t)])

# Spawning biomass
t<-strsplit(REP[grep("SpBiom",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
sp_biom<-as.numeric(t[2:length(t)])
t<-strsplit(REP_20[grep("SpBiom",REP_20)]," ")
t<-subset(t[[1]],t[[1]]!="")
sp_biom_20<-as.numeric(t[2:length(t)])

# F
t<-strsplit(REP[grep("Fully_selected_F",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
F<-as.numeric(t[2:length(t)])

# Selectivity
t<-strsplit(REP[grep("Fishery_Selectivity_1967-1976",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
fish_sel_1<-as.numeric(t[2:length(t)])
t<-strsplit(REP[grep("Fishery_Selectivity_1977-1995",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
fish_sel_2<-as.numeric(t[2:length(t)])
t<-strsplit(REP[grep("Fishery_Selectivity_1996-2006",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
fish_sel_3<-as.numeric(t[2:length(t)])
t<-strsplit(REP[grep(paste("Fishery_Selectivity_2007-",endyr,sep=""),REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
fish_sel_4<-as.numeric(t[2:length(t)])
t<-strsplit(REP[grep("Bottom_Trawl_Survey_Selectivity",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
srv1_sel<-as.numeric(t[2:length(t)])

# Maturity
t<-strsplit(REP[grep("Maturity",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
mat<-as.numeric(t[2:length(t)])

# Recruitment
t<-strsplit(REP[grep("Recruitment",REP)]," ")
t<-subset(t[[1]],t[[1]]!="")
pred_rec<-as.numeric(t[2:length(t)])

N<-REP_20[grep("Numbers",REP_20):(grep("Obs_P_fish_age",REP_20)-2)]
t<-NA
for(y in 1:length(yrs)){
ts<-as.numeric(strsplit(N[y+1]," ")[[1]][3])
t<-c(t,ts)}
pred_rec_20<-subset(t,is.na(t)==FALSE)

# Yeildratio
t<-strsplit(CTL[grep("yieldratio",CTL)],"\t")
yeildratio<-as.numeric(t[[1]][1])







