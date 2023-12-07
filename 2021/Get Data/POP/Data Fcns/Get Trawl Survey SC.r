
#============== Get trawl survey size composition

get_TSSC<-function(species,username,password,lenbins){

library(RODBC)

# Set up connection
channel=odbcConnect("afsc",uid=username,pwd=password,believeNRows=FALSE)

# Get data
data_1<-sqlQuery(channel,paste("SELECT GOA.SIZECOMP_TOTAL.SURVEY, GOA.SIZECOMP_TOTAL.YEAR, GOA.SIZECOMP_TOTAL.SPECIES_CODE, GOA.SIZECOMP_TOTAL.LENGTH, GOA.SIZECOMP_TOTAL.MALES, GOA.SIZECOMP_TOTAL.FEMALES, GOA.SIZECOMP_TOTAL.UNSEXED, GOA.SIZECOMP_TOTAL.TOTAL FROM GOA.SIZECOMP_TOTAL WHERE (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
data_2<-sqlQuery(channel,paste("SELECT RACEBASE.LENGTH.CRUISEJOIN, RACEBASE.LENGTH.REGION, RACEBASE.LENGTH.SPECIES_CODE, RACEBASE.LENGTH.LENGTH, RACEBASE.LENGTH.FREQUENCY, GOA.BIENNIAL_SURVEYS.YEAR, RACEBASE.HAUL.PERFORMANCE, RACEBASE.HAUL.HAULJOIN FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='GOA') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
write.csv(data_1,paste(path_UD,"/Raw Assessment Data/trawl survey size comp/GOA_SIZECOMP_TOTAL.csv",sep=""))
write.csv(data_2,paste(path_UD,"/Raw Assessment Data/trawl survey size comp/Length freq data.csv",sep=""))

# Get survey years and set up size comp and total results matrix
yr<-unique(data_1$YEAR)
yr<-sort(yr)
SC<-matrix(0,nrow=length(yr),ncol=nlenbins)
RES<-matrix(nrow=length(yr),ncol=nlenbins+4)
colnames(RES)<-c("Year","n_s","n_h","SA_Index",as.character(lenbins))
RES[,1]<-yr
RES[,4]<-2

# Calculate size comps
for(y in 1:length(yr)){
Y_SC<-matrix(nrow=length(subset(data_1$TOTAL,data_1$YEAR==yr[y])),ncol=2)
Y_SC[,1]<-subset(data_1$LENGTH,data_1$YEAR==yr[y])
Y_SC[,2]<-subset(data_1$TOTAL,data_1$YEAR==yr[y])
Y_SC<-subset(Y_SC,Y_SC[,1]/10>=as.numeric(lenbins[1]))
Y_SC<-subset(Y_SC,Y_SC[,1]>0)
for(l in 1:(nlenbins-1)){
if(length(subset(Y_SC[,2],Y_SC[,1]/10==as.numeric(lenbins[l])))>0){
SC[y,l]<-sum(subset(Y_SC[,2],Y_SC[,1]/10==as.numeric(lenbins[l])))/sum(as.numeric(Y_SC[,2]))}}
if(length(subset(Y_SC[,2],Y_SC[,1]/10>=as.numeric(lenbins[nlenbins])))>0){
SC[y,nlenbins]<-sum(subset(Y_SC[,2],Y_SC[,1]/10>=as.numeric(lenbins[nlenbins])))/sum(as.numeric(Y_SC[,2]))}}
RES[,5:(nlenbins+4)]<-SC

# Calculate number of samples and hauls
for(y in 1:length(yr)){
SH<-subset(data_2,data_2$YEAR==yr[y])
SH<-subset(SH,SH$LENGTH!="NA")
RES[y,2]<-sum(SH$FREQUENCY)
RES[y,3]<-length(unique(SH$HAULJOIN))}

RES}


