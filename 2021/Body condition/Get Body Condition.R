#~~~~~~~~~~~~~~~~~~~ Set Directories
#C:\Users\maia.kapur\Work\assessments\2023\GOA_POP\2021\Body condition
path<-getwd()
path_DAT<-paste(path,"/Data",sep="")
path_MDL<-paste(path,"/Wbar@L",sep="")
path_RES<-paste(path,"/Results",sep="")

# Read in data
age_data_raw<-read.csv(paste(path_DAT,"/AL_data.csv",sep=""))
years<-sort(unique(age_data_raw$YEAR))
WbarL_params<-matrix(nrow=length(years)+1,ncol=2)
colnames(WbarL_params)<-c("alpha","beta")
rownames(WbarL_params)<-c("All",years)

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
write.csv(lw_mdl_data,paste(path_RES,"/WaL_stats_All.csv",sep=""))

# Get data for plotting
plot_raw_lw<-matrix(nrow=length(lw_mdl_data[,1]),ncol=length(years)+2)
colnames(plot_raw_lw)<-c("Length","All_Wbar",years)
plot_raw_lw[,1]<-lw_mdl_data[,1]/10
plot_raw_lw[,2]<-lw_mdl_data[,2]

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
write.table(DAT,file=paste(path_MDL,"/ALLO.DAT",sep=""),quote=F,row.names=F,col.names=F)
setwd(path_MDL)
shell("ALLO.EXE")
PAR<-readLines(paste(path_MDL,"/allo.par",sep=""),warn=FALSE)
alpha_lw<-as.numeric(strsplit(PAR[grep("alpha",PAR)+1]," ")[[1]])
beta_lw<-as.numeric(strsplit(PAR[grep("beta",PAR)+1]," ")[[1]])
WbarL_params[1,1]<-alpha_lw
WbarL_params[1,2]<-beta_lw

for(y in 1:length(years)){
  # Get/compile weight-at-length statistics
  age_data_raw_y<-subset(age_data_raw,age_data_raw$YEAR==years[y])
  lw_data<-cbind(age_data_raw_y$LENGTH,age_data_raw_y$WEIGHT)
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
  write.csv(lw_mdl_data,paste(path_RES,"/WaL_stats_",years[y],".csv",sep=""))
  
  # Get data for plotting
  for(i in 1:length(lw_mdl_data[,1])){
    plot_raw_lw[which(lw_mdl_data[i,1]/10==plot_raw_lw[,1]),y+2]<-lw_mdl_data[i,2]
  }

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
  write.table(DAT,file=paste(path_MDL,"/ALLO.DAT",sep=""),quote=F,row.names=F,col.names=F)
  setwd(path_MDL)
  shell("ALLO.EXE")
  PAR<-readLines(paste(path_MDL,"/allo.par",sep=""),warn=FALSE)
  alpha_lw<-as.numeric(strsplit(PAR[grep("alpha",PAR)+1]," ")[[1]])
  beta_lw<-as.numeric(strsplit(PAR[grep("beta",PAR)+1]," ")[[1]])
  WbarL_params[y+1,1]<-alpha_lw
  WbarL_params[y+1,2]<-beta_lw
}
  
  
# Get residuals for observed data
LW_resids_obs<-matrix(nrow=length(years),ncol=3)
colnames(LW_resids_obs)<-c("Year","Resid_avg","SD_Resid")
LW_resids_obs[,1]<-years
LW_resid_raw_obs<-matrix(nrow=length(plot_raw_lw[,1]),ncol=length(years))
rownames(LW_resid_raw_obs)<-plot_raw_lw[,1]
colnames(LW_resid_raw_obs)<-years
for(y in 1:length(years)){
  LW_resid_raw_obs[,y]<-plot_raw_lw[,y+2]-plot_raw_lw[,2]
  LW_resids_obs[y,2]<-mean(LW_resid_raw_obs[,y],na.rm=TRUE)
  LW_resids_obs[y,3]<-sd(LW_resid_raw_obs[,y],na.rm=TRUE)
}

# Get residuals for modeled data

LW_resids_mdl<-matrix(nrow=length(years),ncol=3)
colnames(LW_resids_mdl)<-c("Year","Resid_avg","SD_Resid")
LW_resids_mdl[,1]<-years
LW_resid_raw_mdl<-matrix(nrow=length(plot_raw_lw[,1]),ncol=length(years))
rownames(LW_resid_raw_mdl)<-plot_raw_lw[,1]
colnames(LW_resid_raw_mdl)<-years
for(y in 1:length(years)){
  LW_resid_raw_mdl[,y]<-WbarL_params[y+1,1]*(plot_raw_lw[,1]*10)^WbarL_params[y+1,2]-WbarL_params[1,1]*(plot_raw_lw[,1]*10)^WbarL_params[1,2]
  LW_resids_mdl[y,2]<-mean(LW_resid_raw_mdl[,y],na.rm=TRUE)
  LW_resids_mdl[y,3]<-sd(LW_resid_raw_mdl[,y],na.rm=TRUE)
}


png(paste0(path,'/waa_results.png'))
plot(plot_raw_lw[,1],WbarL_params[1,1]*(plot_raw_lw[,1]*10)^WbarL_params[1,2],
type="l", xlab = 'Length cm', ylab = 'Wt gm')
lines(plot_raw_lw[,1],WbarL_params[2,1]*(plot_raw_lw[,1]*10)^WbarL_params[2,2],col="red")
#lines(plot_raw_lw[,1],plot_raw_lw[,2],col="blue")


legend('topleft', legend = c('All years', '1990'), col = c('black','red'), lty = 1)
dev.off()











  
  
