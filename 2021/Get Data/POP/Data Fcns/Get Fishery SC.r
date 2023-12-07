
#============== Get fishery size composition

get_FSC<-function(species,nlenbins,len_bins){

# Read in .csv file, this data not updated for POP since not used past 1997
FSC_n<-read.csv(paste(path_DAT,"/Fishery Data/FSC_n_s-h.csv",sep=""),header=FALSE)

old_years<-c(1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977)
new_years<-c(1991,1992,1995,1996,1997)

old_data<-read.csv(paste(path_DAT,"/Fishery Data/Old fish size comp.csv",sep=""))
new_data<-read.csv(paste(path_DAT,"/Fishery Data/norpac_length_report.csv",sep=""))
new_data<-subset(new_data,new_data$Length>=11)
new_data<-subset(new_data,new_data$T.Table=="DOMESTIC_HAUL")

# Get 60s and 70s data together
old_SC<-matrix(nrow=length(old_years),ncol=nlenbins)
rownames(old_SC)<-old_years
colnames(old_SC)<-len_bins
for(y in 1:length(old_years)){
rwz<-which(old_data$Length<=len_bins[1])
old_SC[y,1]<-sum(old_data[rwz,y+1],na.rm=TRUE)
for(l in 2:(nlenbins-1)){
rwz<-which(old_data$Length==len_bins[l])
old_SC[y,l]<-sum(old_data[rwz,y+1],na.rm=TRUE)}
rwz<-which(old_data$Length>=len_bins[nlenbins])
old_SC[y,nlenbins]<-sum(old_data[rwz,y+1],na.rm=TRUE)}

# Get 90s data together
new_SC<-matrix(nrow=length(new_years),ncol=nlenbins)
rownames(new_SC)<-new_years
colnames(new_SC)<-len_bins
for(y in 1:length(new_years)){
data_y<-subset(new_data,new_data$Year==new_years[y])
rwz<-which(data_y$Length<=len_bins[1])
new_SC[y,1]<-sum(data_y$Frequency[rwz],na.rm=TRUE)
for(l in 2:(nlenbins-1)){
rwz<-which(data_y$Length==len_bins[l])
new_SC[y,l]<-sum(data_y$Frequency[rwz],na.rm=TRUE)}
rwz<-which(data_y$Length>=len_bins[nlenbins])
new_SC[y,nlenbins]<-sum(data_y$Frequency[rwz],na.rm=TRUE)}

FSC<-rbind(old_SC,new_SC)
FSC<-FSC/rowSums(FSC)
RES<-matrix(nrow=length(FSC[,1]),ncol=nlenbins+4)
colnames(RES)<-c("Year","n_s","n_h","SA_Index",seq(1,nlenbins))
RES[,1]<-as.matrix(c(old_years,new_years))
RES[,2]<-as.matrix(FSC_n[,1])
RES[,3]<-as.matrix(FSC_n[,1])
RES[,4]<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2) 
RES[,5:(nlenbins+4)]<-as.matrix(FSC)

RES}


