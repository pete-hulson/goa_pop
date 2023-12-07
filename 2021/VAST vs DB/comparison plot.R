
#####################################################################################################
######### Plot fishery and survey selectivity 
#####################################################################################################

install.packages("extrafont")
library(extrafont)
windowsFonts(A = windowsFont("Times New Roman"))

path<-getwd()

biomass<-read.csv(paste0(path,"/Biomass.csv"))
CV<-read.csv(paste0(path,"/CV.csv"))

uci_db<-biomass$D.B+1.96*biomass$D.B*CV$D.B
lci_db<-biomass$D.B-1.96*biomass$D.B*CV$D.B
uci_vast<-biomass$VAST+1.96*biomass$VAST*CV$VAST
lci_vast<-biomass$VAST-1.96*biomass$VAST*CV$VAST





#layout(matrix(c(0,1,2,0), 4, 1, byrow = TRUE),heights=c(0.07,0.5,0.5,0.07))

par(mar=c(3,5,0.1,0.1),family="A")

plot(biomass$Year,biomass$D.B,type="b",pch=16,lty=2,col="orangered",cex=1.5,ylab="",xlab="",las=2,cex.axis=1,ylim=c(0,2500000))
polygon(c(biomass$Year,sort(biomass$Year,decreasing=TRUE)),c(uci_db,rev(lci_db)),col=adjustcolor("orangered",alpha.f=0.25),border=NA)

points(biomass$Year,biomass$VAST,type="b",pch=16,lty=2,col="darkgreen",cex=1.5)
polygon(c(biomass$Year,sort(biomass$Year,decreasing=TRUE)),c(uci_vast,rev(lci_vast)),col=adjustcolor("darkgreen",alpha.f=0.25),border=NA)
mtext("Biomass (t)",side=2,line=4,cex=1.25)

legend(1990,2377700,legend=c("Design-Based","VAST"),col=c("orangered","darkgreen"),pch=c(16,16),bty="n")





par(mar=c(2,7,2,0.1),family="A")

x<-barplot(CV$D.B,col="orangered",las=2,xaxt="n",ylab="",xlab="",cex.axis=1.5)







# library
library(ggplot2)

# create a dataset

Year<-NULL
cv<-NULL
for(y in 1:length(CV$Year)){
  Year<-c(Year,rep(CV$Year[y],2))
  cv<-c(cv,CV[y,2:3])
}

Meth<-c(rep(c("D-B","VAST"),length(CV$Year)))
data <- data.frame(Year,Meth,cv)



specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(data, aes(fill=condition, y=value, x=CV$Year)) + 
  geom_bar(position="dodge", stat="identity")






