
#============== Get trawl survey age composition
get_TSAC<-function(species,username,password,nages,plus_age){

library(RODBC)

# Set up connection
channel=odbcConnect("afsc",uid=username,pwd=password,believeNRows=FALSE)

# Get data
data_1<-sqlQuery(channel,paste("SELECT GOA.AGECOMP_TOTAL.SURVEY, GOA.AGECOMP_TOTAL.SURVEY_YEAR, GOA.AGECOMP_TOTAL.SPECIES_CODE, GOA.AGECOMP_TOTAL.SEX, GOA.AGECOMP_TOTAL.AGE, GOA.AGECOMP_TOTAL.AGEPOP, GOA.AGECOMP_TOTAL.MEAN_LENGTH, GOA.AGECOMP_TOTAL.STANDARD_DEVIATION FROM GOA.AGECOMP_TOTAL WHERE (((GOA.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
data_2<-sqlQuery(channel,paste("SELECT RACEBASE.SPECIMEN.CRUISE, GOA.BIENNIAL_SURVEYS.YEAR, RACEBASE.SPECIMEN.HAULJOIN, RACEBASE.SPECIMEN.CRUISEJOIN, RACEBASE.SPECIMEN.SPECIES_CODE, RACEBASE.SPECIMEN.SPECIMENID, RACEBASE.SPECIMEN.LENGTH, RACEBASE.SPECIMEN.SEX, RACEBASE.SPECIMEN.AGE, RACEBASE.SPECIMEN.WEIGHT, RACEBASE.SPECIMEN.REGION, RACEBASE.HAUL.PERFORMANCE FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='GOA') AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
write.csv(data_1,paste(path_UD,"/Raw Assessment Data/trawl survey age comp/GOA_AGECOMP_TOTAL.csv",sep=""))
write.csv(data_2,paste(path_UD,"/Raw Assessment Data/trawl survey age comp/Age specimen data.csv",sep=""))

# Get survey years and set up age comp and total results matrix
yr<-sort(unique(data_1$SURVEY_YEAR))
yr<-yr[3:length(yr)]
AC<-matrix(0,nrow=length(yr),ncol=nages)
RES<-matrix(nrow=length(yr),ncol=nages+4)
colnames(RES)<-c("Year","n_s","n_h","AA_Index",seq(recage,plus_age))
RES[,1]<-yr
RES[,4]<-1

# Calculate age comps
for(y in 1:length(yr)){
Y_AC<-matrix(nrow=length(subset(data_1$AGE,data_1$SURVEY_YEAR==yr[y])),ncol=2)
Y_AC[,1]<-subset(data_1$AGE,data_1$SURVEY_YEAR==yr[y])
Y_AC[,2]<-subset(data_1$AGEPOP,data_1$SURVEY_YEAR==yr[y])
Y_AC<-subset(Y_AC,Y_AC[,1]>0)
Y_AC<-subset(Y_AC,Y_AC[,1]>=recage)
for(a in 1:(nages-1)){
if(length(subset(Y_AC[,2],Y_AC[,1]==(a+1)))>0){
AC[y,a]<-sum(subset(Y_AC[,2],Y_AC[,1]==(a+1)))/sum(Y_AC[,2])}}
if(length(subset(Y_AC[,2],Y_AC[,1]>=(nages+1)))>0){
AC[y,nages]<-sum(subset(Y_AC[,2],Y_AC[,1]>=(nages+1)))/sum(Y_AC[,2])}}
RES[,5:(nages+4)]<-AC

# Calculate number of samples and hauls
for(y in 1:length(yr)){
SH<-subset(data_2,data_2$YEAR==yr[y])
SH<-subset(SH,SH$AGE!="NA")
RES[y,2]<-length(SH$AGE)
RES[y,3]<-length(unique(SH$HAULJOIN))}

RES}


