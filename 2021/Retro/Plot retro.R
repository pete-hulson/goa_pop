
# Get directories

path<-getwd()
pathR<-paste(path,"/Results",sep="")

install.packages("extrafont")
library(extrafont)
windowsFonts(A = windowsFont("Times New Roman"))

species<-"POP"
modelyear<-2021
styr<-1961
numretros<-10 # number of retrospective years
nages<-28

# Read in plot data

ssb<-seq(1,(modelyear-styr+1))
ssb_uci<-seq(1,(modelyear-styr+1))
ssb_lci<-seq(1,(modelyear-styr+1))
totbio<-seq(1,(modelyear-styr+1))
i<-0
for (i in modelyear:(modelyear-numretros+1)) {
  z<-paste0(pathR,"/std_",i,".std")
  f <- read.delim(z,sep="")
  ssb1<-subset(f$value,f$name=="spawn_biom")
  length(ssb1)<-modelyear-styr+1
  ssb<-cbind(ssb,ssb1)
  
  mcmc<-(read.table(paste0(pathR,"/",species,"_mcmc_",i,".std"),header=F,sep=""))
  mcmc<-mcmc[(0.2*length(mcmc[,1])):length(mcmc[,1]),] # remove burning
  mcmcnames<-  c("sigr","q1",  "q2",  "f40",  "M", "ABC",  "obj_fun")
  for(j in styr:i) mcmcnames<- c(mcmcnames,paste("totbio",j,sep="")) 
  for(j in (styr-nages+2):i) mcmcnames<- c(mcmcnames,paste("recdev",j,sep="")) 
  for(j in styr:i) mcmcnames<- c(mcmcnames,paste("ssb",j,sep="")) 
  mcmcnames<-c(mcmcnames,"LMR")
  for(j in (i+1):(i+15)) mcmcnames<-c(mcmcnames,paste("ssbproj",j,sep=""))
  for(j in (i+1):(i+15)) mcmcnames<-c(mcmcnames,paste("catchproj",j,sep=""))
  for(j in (i+1):(i+10)) mcmcnames<-c(mcmcnames,paste("recproj",j,sep=""))
  mcmcnames<-c(mcmcnames,paste("totbioproj",i+1,sep=""))
  mcmcnames<-c(mcmcnames,paste("totbioproj",i+2,sep=""))
  names(mcmc)<-mcmcnames
  mcmc_ssb<-mcmc[,which(substr(names(mcmc),1,3)=="ssb")[1]:which(substr(names(mcmc),1,3)=="ssb")[length(seq(styr,i))]]
  
  ssb_uci1<-vector(length=length(mcmc_ssb[1,]))
  ssb_lci1<-vector(length=length(mcmc_ssb[1,]))
  for(j in 1:length(mcmc_ssb[1,])) ssb_uci1[j]<-quantile(mcmc_ssb[,j],0.975)
  for(j in 1:length(mcmc_ssb[1,])) ssb_lci1[j]<-quantile(mcmc_ssb[,j],0.025)
  length(ssb_uci1)<-modelyear-styr+1
  length(ssb_lci1)<-modelyear-styr+1
  ssb_uci<-cbind(ssb_uci,ssb_uci1)
  ssb_lci<-cbind(ssb_lci,ssb_lci1)
}
ssb[,1]<-seq((modelyear-length(ssb[,1])+1),modelyear)
colnames(ssb)<-c("Year",seq(modelyear,modelyear-numretros+1))
ssb_uci[,1]<-seq((modelyear-length(ssb_uci[,1])+1),modelyear)
colnames(ssb_uci)<-c("Year",seq(modelyear,modelyear-numretros+1))
ssb_lci[,1]<-seq((modelyear-length(ssb_lci[,1])+1),modelyear)
colnames(ssb_lci)<-c("Year",seq(modelyear,modelyear-numretros+1))


#### Make some color palletes
colvec <- rainbow(numretros+1, alpha = 0.7)
shadecolvec <- rainbow(numretros+1, alpha = 0.075)
#colvec <- terrain.colors(numretros+1, alpha = 0.7)
#shadecolvec <- terrain.colors(numretros+1, alpha = 0.075)

addpoly <- function(yrvec, lower, upper, shadecol = rgb(0,0, 0, 0.1), col = 1) {
  polygon(x = c(yrvec, rev(yrvec)), y = c(lower, rev(upper)), 
          border = NA, col = shadecol)
  #lines(yrvec, lower, lty = 3, col = col)
  #lines(yrvec, upper, lty = 3, col = col)
}

layout(matrix(c(0,1,2,0),4,1,byrow = TRUE),heights=c(0,1,1,0.2))
par(mar=c(0.1,5,0.1,1),family="A")

plot(styr:modelyear,ssb[,2]/1000,type="l",lwd=3,xaxt="n",ylim=c(0,300),xlab="",ylab="",las=2,cex.axis=1.25,col=colvec[1])
mtext("Spawning biomass (kt)",side=2,line=3.777)
addpoly(styr:modelyear,subset(ssb_lci[,2]/1000,is.na(ssb[,2])==FALSE),subset(ssb_uci[,2]/1000,is.na(ssb[,2])==FALSE),shadecol=shadecolvec[1])

for (i in 2:numretros) {
lines(styr:(modelyear-i+1),subset(ssb[,i+1]/1000,is.na(ssb[,i+1])==FALSE),col=colvec[i],lwd=3)
addpoly(styr:(modelyear-i+1),subset(ssb_lci[,i+1]/1000,is.na(ssb[,i+1])==FALSE),subset(ssb_uci[,i+1]/1000,is.na(ssb[,i+1])==FALSE),shadecol=shadecolvec[i])
}

plot(styr:modelyear,(1-ssb[,2]/ssb[,2])*100,type="l",xaxt="n",ylim=c(-100,100),xlab="",ylab="",las=2,cex.axis=1.25,lwd=3,col=colvec[1])
mtext("Percent difference from terminal year",side=2,line=3.777)
mtext("Year",side=1,line=2.777)
axis(1,cex.axis=1.25)

for (i in 2:numretros) {

lines(styr:(modelyear-i+1),subset((ssb[,i+1]/ssb[,2]-1)*100,is.na(ssb[,i+1])==FALSE),col=colvec[i],lwd=2)

addpoly(styr:(modelyear-i+1),
  subset((ssb_lci[,i+1]/ssb[,2]-1)*100,is.na(ssb[,i+1])==FALSE),
  subset((ssb_uci[,i+1]/ssb[,2]-1)*100,is.na(ssb[,i+1])==FALSE),shadecol=shadecolvec[i])
  
}



