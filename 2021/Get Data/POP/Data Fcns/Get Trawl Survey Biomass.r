
#============== Get trawl survey biomass
# Sequel call directed to GOA.Biomass_Total table
# 2001 read in as 'set' value due to no sampling in the eastern gulf
# Below code is provided on how this value was originally calculated

get_TSB<-function(species,username,password){

library(RODBC)

# Set up connection
channel=odbcConnect("afsc",uid=username,pwd=password,believeNRows=FALSE)

# Get data
data<-sqlQuery(channel,paste("SELECT * FROM GOA.BIOMASS_TOTAL WHERE (((GOA.BIOMASS_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
SB_2001<-read.csv(paste(path_DAT,"/Survey Data/SB_2001.csv",sep=""),header=TRUE)
write.csv(data,paste(path_UD,"/Raw Assessment Data/trawl survey biom/GOA_BIOMASS_TOTAL.csv",sep=""))

# Arrange data into matrix and get results
yr<-sort(data$YEAR)
yr<-yr[3:length(yr)]
SB_Data<-matrix(nrow=length(yr),ncol=5)
colnames(SB_Data)<-c("Year","Biom","SE","LCI","UCI")
SB_Data[,1]<-yr
for(i in 1:length(yr)){
SB_Data[i,2]<-round(subset(data$TOTAL_BIOMASS,data$YEAR==SB_Data[i,1]))
SB_Data[i,3]<-round(sqrt(subset(data$BIOMASS_VAR,data$YEAR==SB_Data[i,1])))}
SB_Data[,4]<-round(SB_Data[,2]-1.96*SB_Data[,3])
SB_Data[,5]<-round(SB_Data[,2]+1.96*SB_Data[,3])

# Take out LCI less than 0
for(i in 1:length(yr)){
for(j in 1:5){
if(SB_Data[i,j]<0){SB_Data[i,j]<-0}}}

# Replace 2001 Spawning biomass
SB_Data[5,2:5]<-round(as.matrix(SB_2001[1,]))

SB_Data}

## Original code to calculate 2001 biomass

#b2001<-function(year,datafile,sp_code){
# x<-goa_biom_inpfc[goa_biom_inpfc$SPECIES_CODE %in% c(sp_code)& goa_biom_inpfc$YEAR>=1993 & goa_biom_inpfc$YEAR<=1999,]
# biom<-as.data.frame(tapply(x$AREA_BIOMASS,x$SUMMARY_AREA,mean))
# var<-as.data.frame(tapply(x$AREA_BIOMASS,x$SUMMARY_AREA,var))
# fix<-cbind(biom,var)
# names(fix)<-c("AREA_BIOMASS","BIOMASS_VAR")
# fix<-fix[rownames(fix)=="949"|rownames(fix)=="959",]
# fix_var<-(1+1/3)*sum(fix$BIOMASS_VAR)
# x2001<-goa_biom_inpfc[goa_biom_inpfc$SPECIES_CODE %in% c(sp_code)& goa_biom_inpfc$YEAR==2001,]
# x2001<-subset(x2001,select=c("AREA_BIOMASS","BIOMASS_VAR"))
# biom_2001<-sum(x2001$AREA_BIOMASS,fix$AREA_BIOMASS)
# var_2001<-sum(x2001$BIOMASS_VAR,fix_var)
# se_2001<-sqrt(var_2001)
# lci_2001<-biom_2001-1.96*se_2001
# uci_2001<-biom_2001+1.96*se_2001
# tot_2001<-as.data.frame(rbind(biom_2001,var_2001,se_2001,lci_2001,uci_2001))
# colnames(tot_2001)<-rep(" ",length(colnames(tot_2001)))
#}

