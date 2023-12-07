
#============== Get Fishery age composition data

get_FAC<-function(species,username,password,plus_age,nages){

library(RODBC)

# Set up connection
channel=odbcConnect("akfin",uid=username,pwd=password,believeNRows=FALSE)

# Get data
age_data<-sqlQuery(channel,paste("SELECT NORPAC.DEBRIEFED_AGE_MV.YEAR, NORPAC.DEBRIEFED_AGE_MV.FMP_AREA, NORPAC.DEBRIEFED_AGE_MV.SPECIES, NORPAC.DEBRIEFED_AGE_MV.LENGTH, NORPAC.DEBRIEFED_AGE_MV.WEIGHT, NORPAC.DEBRIEFED_AGE_MV.AGE, NORPAC.DEBRIEFED_AGE_MV.PERFORMANCE FROM NORPAC.DEBRIEFED_AGE_MV WHERE NORPAC.DEBRIEFED_AGE_MV.FMP_AREA = 'GOA' AND NORPAC.DEBRIEFED_AGE_MV.SPECIES = ",species,sep=""),believeNRows=FALSE)
HAUL_JOIN<-sqlQuery(channel,paste("SELECT TO_CHAR(NORPAC.DEBRIEFED_AGE_MV.HAUL_JOIN) AS HAUL_JOIN FROM NORPAC.DEBRIEFED_AGE_MV WHERE NORPAC.DEBRIEFED_AGE_MV.FMP_AREA = 'GOA' AND NORPAC.DEBRIEFED_AGE_MV.SPECIES = ",species,sep=""),as.is=TRUE,believeNRows=FALSE)
PORT_JOIN<-sqlQuery(channel,paste("SELECT TO_CHAR(NORPAC.DEBRIEFED_AGE_MV.PORT_JOIN) AS PORT_JOIN FROM NORPAC.DEBRIEFED_AGE_MV WHERE NORPAC.DEBRIEFED_AGE_MV.FMP_AREA = 'GOA' AND NORPAC.DEBRIEFED_AGE_MV.SPECIES = ",species,sep=""),as.is=TRUE,believeNRows=FALSE)
age_data<-cbind(age_data,HAUL_JOIN,PORT_JOIN)
r<-which(age_data$SPECIMEN_TYPE==3)
if(length(r)>0){
age_data<-age_data[-r,]}
write.csv(age_data,paste(path_UD,"/Raw Assessment Data/fish age comp/fishery age data.csv",sep=""))

length_data<-sqlQuery(channel,paste("SELECT NORPAC.DEBRIEFED_LENGTH_MV.YEAR, NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA, NORPAC.DEBRIEFED_LENGTH_MV.SPECIES, NORPAC.DEBRIEFED_LENGTH_MV.LENGTH, NORPAC.DEBRIEFED_LENGTH_MV.FREQUENCY, NORPAC.DEBRIEFED_LENGTH_MV.PERFORMANCE FROM NORPAC.DEBRIEFED_LENGTH_MV WHERE NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA = 'GOA' AND NORPAC.DEBRIEFED_LENGTH_MV.SPECIES = ",species,sep=""),believeNRows=FALSE)
HAUL_JOIN<-sqlQuery(channel,paste("SELECT TO_CHAR(NORPAC.DEBRIEFED_LENGTH_MV.HAUL_JOIN) AS HAUL_JOIN FROM NORPAC.DEBRIEFED_LENGTH_MV WHERE NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA = 'GOA' AND NORPAC.DEBRIEFED_LENGTH_MV.SPECIES = ",species,sep=""),as.is=TRUE,believeNRows=FALSE)
PORT_JOIN<-sqlQuery(channel,paste("SELECT TO_CHAR(NORPAC.DEBRIEFED_LENGTH_MV.PORT_JOIN) AS PORT_JOIN FROM NORPAC.DEBRIEFED_LENGTH_MV WHERE NORPAC.DEBRIEFED_LENGTH_MV.FMP_AREA = 'GOA' AND NORPAC.DEBRIEFED_LENGTH_MV.SPECIES = ",species,sep=""),as.is=TRUE,believeNRows=FALSE)
length_data<-cbind(length_data,HAUL_JOIN,PORT_JOIN)
write.csv(length_data,paste(path_UD,"/Raw Assessment Data/fish age comp/fishery length data.csv",sep=""))

# Get data formatted/subsetted
RAW_DATA<-subset(age_data,age_data$PERFORMANCE!="NA")
RAW_DATA<-subset(RAW_DATA,RAW_DATA$AGE!="NA")
yr<-sort(unique(RAW_DATA$YEAR))
yr<-subset(yr,yr>=1990 & yr!=2007)
AC<-matrix(0,nrow=length(yr),ncol=nages)
RES<-matrix(nrow=length(yr),ncol=nages+4)
colnames(RES)<-c("Year","n_s","n_h","AA_Index",seq(recage,plus_age))
RES[,1]<-yr
RES[,4]<-1
length_data<-subset(length_data,length_data$PERFORMANCE!="NA")

# Calculate age comps
for(y in 1:length(yr)){
  # Subset to year data
  age_data_y<-subset(RAW_DATA,RAW_DATA$YEAR==yr[y])
  length_data_y<-subset(length_data,length_data$YEAR==yr[y])
  # Get number of ages by length and age
  age_num <- tapply(age_data_y$AGE, list(age_data_y$LENGTH, age_data_y$AGE), length)
  age_num[is.na(age_num)] <- 0
  # Turn these into fractions
  age_frac <- age_num/apply(age_num, 1, sum)
  # Get number at length
  length_num <- tapply(length_data_y$FREQUENCY, list(length_data_y$LENGTH), sum)
  # Match up lengths from length freq in ALK, and lengths in ALK in length freq
  length_num <- length_num[which(as.numeric(names(length_num)) %in% as.numeric(rownames(age_frac)))]
  age_frac <- age_frac[which(as.numeric(rownames(age_frac)) %in% as.numeric(names(length_num))),]
  # Turn #'s at length into fractions
  length_frac <- length_num/sum(length_num)
  # Estimate age comp
  ac_y <- colSums(as.vector(length_frac)*age_frac)
  # Put in results matrix
  ages<-seq(recage,plus_age)
  fac <- ac_y[match(ages,as.numeric(names(ac_y)))]
  fac[length(ages)] <- sum(ac_y[which(as.numeric(names(ac_y)) >= ages[length(ages)])])
  fac[is.na(fac)] <- 0
  RES[y,5:(nages+4)]<-fac
}

# Calculate number of samples and hauls
for(y in 1:length(yr)){
  SH<-subset(RAW_DATA,RAW_DATA$YEAR==yr[y])
  SH<-subset(SH,SH$AGE!="NA")
  RES[y,2]<-length(SH$AGE)
  hj<-as.matrix(subset(SH$HAUL_JOIN,SH$HAUL_JOIN!=""))
  pj<-as.matrix(subset(SH$PORT_JOIN,SH$PORT_JOIN!=""))
  RES[y,3]<-length(unique(hj))+length(unique(pj))}

RES}


