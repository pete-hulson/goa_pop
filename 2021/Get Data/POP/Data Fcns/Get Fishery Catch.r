
#============== Get total groundfish catch
# Read in .csv file obtained from AKFIN Groundfish Total Catch report (1991-Present for POP)
# Pre-1990 values set at old catch numbers, read in as .csv

get_FC<-function(species,group_code,username,password){

library(RODBC)

# Set up connection
channel=odbcConnect("akfin",uid=username,pwd=password,believeNRows=FALSE)

# Get data
catch_data<-sqlQuery(channel,paste("SELECT COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR, COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_NAME, COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE, COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR, COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA, COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA, COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED FROM COUNCIL.COMPREHENSIVE_BLEND_CA WHERE COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA = 'GOA' AND COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ",endyr," AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = '",group_code,"'",sep=""),believeNRows=FALSE)
obs_data<-sqlQuery(channel,paste("SELECT NORPAC.DEBRIEFED_SPCOMP_MV.YEAR, NORPAC.DEBRIEFED_SPCOMP_MV.HAUL_DATE, NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES, NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA, NORPAC.DEBRIEFED_SPCOMP_MV.EXTRAPOLATED_WEIGHT FROM NORPAC.DEBRIEFED_SPCOMP_MV INNER JOIN NORPAC.DEBRIEFED_HAUL_MV ON NORPAC.DEBRIEFED_SPCOMP_MV.JOIN_KEY = NORPAC.DEBRIEFED_HAUL_MV.JOIN_KEY WHERE NORPAC.DEBRIEFED_SPCOMP_MV.YEAR BETWEEN ",endyr-3," AND ",endyr-1," AND NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA = 'GOA' AND NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES = ",species,sep=""),believeNRows=FALSE)
Old_C<-read.csv(paste(path_DAT,"/Fishery Data/Old C.csv",sep=""),header=TRUE)
write.csv(catch_data,paste(path_UD,"/Raw Assessment Data/catch/catch_data.csv",sep=""))
write.csv(obs_data,paste(path_UD,"/Raw Assessment Data/catch/obs_catch_data.csv",sep=""))

# Compute catch from 1991 to current year
yr<-seq(1991,endyr)
C<-matrix(0,nrow=length(yr),ncol=1)
for(y in 1:(length(yr)-1)){
Y_C<-subset(catch_data$WEIGHT_POSTED,catch_data$YEAR==yr[y])
C[y,1]<-round(sum(Y_C))}
Y_C<-subset(catch_data,catch_data$YEAR==yr[length(yr)])
Y_C<-subset(Y_C,as.Date(Y_C$WEEK_END_DATE)<=paste(yr[length(yr)],"-10-07",sep=""))
C[length(yr),1]<-round(sum(Y_C$WEIGHT_POSTED))

# Estimate catch in final year to end of year
yr<-seq(endyr-3,endyr-1)
Endyr_C<-matrix(nrow=length(yr),ncol=2)
colnames(Endyr_C)<-c("Oct_C","Total_C")
rownames(Endyr_C)<-yr
for(y in 1:length(yr)){
Data<-subset(obs_data,obs_data$YEAR==yr[y])
Data_pre<-subset(Data,as.Date(Data$HAUL_DATE)<=paste(yr[y],substr(max(as.Date(Y_C$WEEK_END_DATE)),5,10),sep=""))
Endyr_C[y,1]<-sum(Data_pre$EXTRAPOLATED_WEIGHT)
Endyr_C[y,2]<-sum(Data$EXTRAPOLATED_WEIGHT)}
Endyr_ratio<-1+(sum(Endyr_C[,2])-sum(Endyr_C[,1]))/sum(Endyr_C[,1])
C[length(C[,1]),1]<-round(C[length(C[,1]),1]*Endyr_ratio)

# Compile catch data
C<-c(Old_C[,2],C)

# Estimate yeild ratio
yld_rat<-mean(C[(length(C)-3):(length(C)-1)]/TAC)

c(yld_rat,C)}


