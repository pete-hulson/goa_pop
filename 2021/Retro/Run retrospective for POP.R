
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#/\/\/\/\/\/\/\/\ Set up directories
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#setwd("C:/Retro")
path<-getwd()
#path<-"C:/Users/Pete.Hulson/Desktop/NOAA/Stock Assessments/Tier 3/2020/POP/Retro"

#### Set species and model year
species<-"POP"
modelyear<-2021
MDL_name<-"model_20_1"


##### Set up some paths
pathD<-paste(path,"/Data",sep="")
pathR<-paste(path,"/Results",sep="")
pathM<-paste(path,"/Model",sep="")

# Get current data files
CTL<-read.delim(paste(pathD,"/goa_",species,"_",modelyear,".ctl",sep=""),header=FALSE)
DAT<-readLines(paste(pathD,"/goa_",species,"_",modelyear,".dat",sep=""),warn=FALSE)

Sec_st<-grep("#-",DAT,fixed=TRUE)
Sec_end<-grep("#!",DAT)

st_end<-matrix(NA,nrow=length(Sec_st),ncol=2)
st_end[,1]<-Sec_st
st_end[,2]<-Sec_end

mcmcon<-"YES"
mcmcruns<-500000  # Could change these, but I like 5000 as a manageable number to deal with
mcmcsave<-mcmcruns/5000
#mcmcruns<-10000  # Could change these, but I like 5000 as a manageable number to deal with
#mcmcsave<-mcmcruns/5000



#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#/\/\/\/\/\/\/\/\ Set up some model dimensions
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
styr<-as.numeric(DAT[Sec_st[2]-3]) # start of model (example 1961 for POP)
nages<-as.numeric(DAT[Sec_st[2]+3]) # number of age bins
nlens<-as.numeric(DAT[Sec_st[2]+5]) # number of length bins
numretros<-10 # number of retrospective years

# Set up some results files
RES_SB<-matrix(nrow=length(seq(styr,modelyear)),ncol=numretros)
rownames(RES_SB)<-seq(styr,modelyear)
colnames(RES_SB)<-seq(modelyear-numretros+1,modelyear)
RES_Rec<-matrix(nrow=length(seq(styr,modelyear)),ncol=numretros)
rownames(RES_Rec)<-seq(styr,modelyear)
colnames(RES_Rec)<-seq(modelyear-numretros+1,modelyear)

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#/\/\/\/\/\/\/\/\ Run retrospective loop
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
T_start<-Sys.time() #Timer start

for(y in 1:numretros){

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#/\/\/\/\/\/\/\/\ Concatenate DAT file
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

# Set endyr
yrs_retro<-seq(modelyear-numretros+1,modelyear)
endyr<-yrs_retro[y]
nyrs<-endyr-styr+1
DAT_retro<-c(DAT[st_end[1,1]:st_end[1,2]],as.character(endyr),DAT[st_end[2,1]:st_end[2,2]])

# Fishery catch
DAT_retro<-c(DAT_retro,paste(scan(text=DAT[Sec_st[3]-1])[1:nyrs],collapse=" "),DAT[st_end[3,1]:st_end[3,2]])
# Trawl survey biomass
 BTSb_yrs<-length(which(scan(text=DAT[Sec_st[5]-1])<(endyr+1)))
DAT_retro<-c(
DAT_retro,
as.character(BTSb_yrs),
DAT[st_end[4,1]:st_end[4,2]],
paste(scan(text=DAT[Sec_st[5]-1])[1:BTSb_yrs],collapse=" "),
DAT[st_end[5,1]:st_end[5,2]],
paste(scan(text=DAT[Sec_st[6]-1])[1:BTSb_yrs],collapse=" "),
DAT[st_end[6,1]:st_end[6,2]],
paste(scan(text=DAT[Sec_st[7]-1])[1:BTSb_yrs],collapse=" "),
DAT[st_end[7,1]:st_end[7,2]],
paste(scan(text=DAT[Sec_st[8]-1])[1:BTSb_yrs],collapse=" "),
DAT[st_end[8,1]:st_end[8,2]],
paste(scan(text=DAT[Sec_st[9]-1])[1:BTSb_yrs],collapse=" "),
DAT[st_end[9,1]:st_end[9,2]])

# Fish age comp
FAC_yr_max<-length(scan(text=DAT[Sec_st[11]+1]))
FAC_yrs<-length(which(scan(text=DAT[Sec_st[11]-1])<(endyr)))
DAT_retro<-c(DAT_retro,
as.character(FAC_yrs),
DAT[st_end[10,1]:st_end[10,2]],
paste(scan(text=DAT[Sec_st[11]-1])[1:FAC_yrs],collapse=" "),
DAT[st_end[11,1]:st_end[11,2]],
paste(scan(text=DAT[Sec_st[12]-1])[1:FAC_yrs],collapse=" "),
DAT[st_end[12,1]:st_end[12,2]],
paste(scan(text=DAT[Sec_st[13]-1])[1:FAC_yrs],collapse=" "),
DAT[st_end[13,1]:st_end[13,2]],
paste(scan(text=DAT[Sec_st[14]-1])[1:FAC_yrs],collapse=" "),
DAT[st_end[14,1]:st_end[14,2]])
for(i in 1:FAC_yrs)   DAT_retro<-c(DAT_retro,paste(scan(text=DAT[Sec_st[15]-FAC_yr_max+i-1]) ,collapse = " "))
DAT_retro<-c(DAT_retro,DAT[st_end[15,1]:st_end[15,2]])

# Survey age comp
SAC_yr_max<-length(scan(text=DAT[Sec_st[17]+1]))
SAC_yrs<-length(which(scan(text=DAT[Sec_st[17]-1])<(endyr)))
DAT_retro<-c(DAT_retro,
as.character(SAC_yrs),
DAT[st_end[16,1]:st_end[16,2]],
paste(scan(text=DAT[Sec_st[17]-1])[1:SAC_yrs],collapse=" "),
DAT[st_end[17,1]:st_end[17,2]],
paste(scan(text=DAT[Sec_st[18]-1])[1:SAC_yrs],collapse=" "),
DAT[st_end[18,1]:st_end[18,2]],
paste(scan(text=DAT[Sec_st[19]-1])[1:SAC_yrs],collapse=" "),
DAT[st_end[19,1]:st_end[19,2]],
paste(scan(text=DAT[Sec_st[20]-1])[1:SAC_yrs],collapse=" "),
DAT[st_end[20,1]:st_end[20,2]])
for(i in 1:SAC_yrs)   DAT_retro<-c(DAT_retro,paste(scan(text=DAT[Sec_st[21]-SAC_yr_max+i-1]) ,collapse = " "))
DAT_retro<-c(DAT_retro,DAT[st_end[21,1]:st_end[21,2]])

# Fish size comp
FSC_yrs<-length(which(scan(text=DAT[Sec_st[23]-1])<(endyr)))
DAT_retro<-c(DAT_retro,
as.character(FSC_yrs),
DAT[st_end[22,1]:st_end[22,2]],
paste(scan(text=DAT[Sec_st[23]-1])[1:FSC_yrs],collapse=" "),
DAT[st_end[23,1]:st_end[23,2]],
paste(scan(text=DAT[Sec_st[24]-1])[1:FSC_yrs],collapse=" "),
DAT[st_end[24,1]:st_end[24,2]],
paste(scan(text=DAT[Sec_st[25]-1])[1:FSC_yrs],collapse=" "),
DAT[st_end[25,1]:st_end[25,2]],
paste(scan(text=DAT[Sec_st[26]-1])[1:FSC_yrs],collapse=" "),
DAT[st_end[26,1]:st_end[26,2]])
for(i in 1:FSC_yrs)   DAT_retro<-c(DAT_retro,paste(scan(text=DAT[Sec_st[27]-FSC_yrs-1+i]) ,collapse = " "))
DAT_retro<-c(DAT_retro,DAT[st_end[27,1]:st_end[27,2]])

# Survey size comp
SSC_yrs<-length(which(scan(text=DAT[Sec_st[29]-1])<(endyr+1)))
DAT_retro<-c(DAT_retro,
as.character(SSC_yrs),
DAT[st_end[28,1]:st_end[28,2]],
paste(scan(text=DAT[Sec_st[29]-1])[1:SSC_yrs],collapse=" "),
DAT[st_end[29,1]:st_end[29,2]],
paste(scan(text=DAT[Sec_st[30]-1])[1:SSC_yrs],collapse=" "),
DAT[st_end[30,1]:st_end[30,2]],
paste(scan(text=DAT[Sec_st[31]-1])[1:SSC_yrs],collapse=" "),
DAT[st_end[31,1]:st_end[31,2]],
paste(scan(text=DAT[Sec_st[32]-1])[1:SSC_yrs],collapse=" "),
DAT[st_end[32,1]:st_end[32,2]])
for(i in 1:SSC_yrs)   DAT_retro<-c(DAT_retro,paste(scan(text=DAT[Sec_st[33]-SSC_yrs-1+i]) ,collapse = " "))
DAT_retro<-c(DAT_retro,DAT[st_end[33,1]:st_end[33,2]])

# Write data and control file
write.table(DAT_retro,file=paste(pathM,"/goa_",species,"_",endyr,".dat",sep=""),quote=F,row.names=F,col.names=F)
CTL_retro<-as.matrix(CTL)
CTL_retro[2,1]<-paste("goa_",species,"_",endyr,".dat",sep="")
CTL_retro[4,1]<-as.character(endyr)
write.table(CTL_retro,file=paste(pathM,"/goa_",species,"_",modelyear,".ctl",sep=""),quote=F,row.names=F,col.names=F)



#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#/\/\/\/\/\/\/\/\ Run model
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

## set your number of MCMC runs at the top of the program... 
setwd(pathM)
shell(paste(MDL_name,'.exe ','-nox'))

# Copy files from base run
file.copy(from=paste(pathM,"/",MDL_name,".STD",sep=""),to=paste(pathR,"/std_",endyr,".std",sep=""),overwrite=T)
file.copy(from=paste(pathM,"/report.rep",sep=""),to=paste(pathR,"/rep_",endyr,".rep",sep=""),overwrite=T)
file.copy(from=paste(pathM,"/",MDL_name,".par",sep=""),to=paste(pathR,"/par_",endyr,".par",sep=""),overwrite=T)
file.copy(from=paste(pathM,"/proj.dat",sep=""),to=paste(pathR,"/prj_",endyr,".prj",sep=""),overwrite=T)

# Compile SSB/recruitment results
STD<-read.delim(paste(pathM,"/",MDL_name,".STD",sep=""),sep="")
RES_SB[1:nyrs,y]<-STD$value[which(STD$name=="spawn_biom")]
RES_Rec[1:nyrs,y]<-STD$value[which(STD$name=="pred_rec")]

if(mcmcon=="YES"){
  # Freeze sigr for MCMC run
  sigr<-STD$value[which(STD$name=="sigr")]
  CTL_retro[which(CTL_retro[,2]=="# sigrprior"),1]<-sigr
  CTL_retro[which(CTL_retro[,2]=="# ph_sigr"),1]<-(-2)
  write.table(CTL_retro,file=paste(pathM,"/goa_",species,"_",modelyear,".ctl",sep=""),quote=F,row.names=F,col.names=F)
  shell(paste(MDL_name,'.exe',' -mcmc ',mcmcruns,'-mcsave ',mcmcsave))
  shell(paste(MDL_name,'.exe',' -mceval'))
  file.copy(from=paste(pathM,"/evalout.prj",sep=""),to=paste(pathR,"/",species,"_mcmc_",endyr,".std",sep=""),overwrite=T)
}



#---------------------------------------------
# End of retrospective model running loop
#---------------------------------------------
}

write.csv(RES_SB,paste(pathR,"/RES_SB.csv",sep=""))
write.csv(RES_Rec,paste(pathR,"/RES_Rec.csv",sep=""))


T_end<-Sys.time()

T_end-T_start
