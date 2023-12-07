
#####################################################################################################
######### Plot phase plane figure
#####################################################################################################

B35<-as.numeric(REP[(grep("B_35",REP)+1):(grep("F_40",REP)[1]-1)])
y_plot<-c(F,STD$value[which(STD$name=="F40")]*yeildratio,STD$value[which(STD$name=="F40")]*yeildratio)/STD$value[which(STD$name=="F35")]
x_plot<-c(sp_biom,STD$value[which(STD$name=="spawn_biom_proj")][1],STD$value[which(STD$name=="spawn_biom_proj")][2])/B35
Fabc<-0.35/0.4
phasecolor<- c("black","red","darkorange","forestgreen","deepskyblue","blue")
endyr<-yrs[length(yrs)]
years<-c(yrs,endyr+1,endyr+2)

layout(matrix(c(0,1,2,0), 4, 1, byrow = TRUE),heights=c(0.07,0.5,0.5,0.07))

par(mar=c(2,7,0.1,7),family="A")

plot(x_plot,y_plot,ylim=c(0,ceiling(max(y_plot))),xlim=c(0,ceiling(max(x_plot))),type="l",lwd=2,ylab="",cex.axis=1.5,las=2,xaxt="n")
points(x_plot,y_plot,pch=16,cex=2.5,col="white")
axis(1,cex.axis=1.5)
text(x_plot[which(years>=1960 & years<1970)],y_plot[which(years>=1960 & years<1970)],labels=years[which(years>=1960 & years<1970)]-1900,cex=0.75,col=phasecolor[1],font=2)
text(x_plot[which(years>=1970 & years<1980)],y_plot[which(years>=1970 & years<1980)],labels=years[which(years>=1970 & years<1980)]-1900,cex=0.75,col=phasecolor[2],font=2)
text(x_plot[which(years>=1980 & years<1990)],y_plot[which(years>=1980 & years<1990)],labels=years[which(years>=1980 & years<1990)]-1900,cex=0.75,col=phasecolor[3],font=2)
text(x_plot[which(years>=1990 & years<2000)],y_plot[which(years>=1990 & years<2000)],labels=years[which(years>=1990 & years<2000)]-1900,cex=0.75,col=phasecolor[4],font=2)
text(x_plot[which(years>=2000 & years<2010)],y_plot[which(years>=2000 & years<2010)],labels=paste("0",years[which(years>=2000 & years<2010)]-2000,sep=""),cex=0.75,col=phasecolor[5],font=2)
text(x_plot[which(years>=2010 & years<2020)],y_plot[which(years>=2010 & years<2020)],labels=years[which(years>=2010 & years<2020)]-2000,cex=0.75,col=phasecolor[6],font=2)
segments(0.05,0,0.4/0.35,1,col="red",lwd=2.5)
segments(0.4/0.35,1,4,1,col="blue",lwd=2.5)
segments(0.05,0,0.4/0.35,Fabc,col="red",lwd=2.5,lty=2)
segments(0.4/0.35,Fabc,4,Fabc,col="blue",lwd=2.5,lty=2)
legend(0.1,6.5,legend=c(expression(italic(F[OFL])),expression(italic(F[ABC]))),lwd=c(2.5,2.5),col=c("blue","blue"),lty=c(1,2),cex=1.5,bty="n")

par(mar=c(2,7,0.1,7),family="A")

plot(x_plot,y_plot,ylim=c(0,1),xlim=c(0,2.5),type="l",lwd=2,ylab="",cex.axis=1.5,las=2,xaxt="n")
points(x_plot,y_plot,pch=16,cex=2.5,col="white")
axis(1,cex.axis=1.5)
text(x_plot[which(years>=1960 & years<1970)],y_plot[which(years>=1960 & years<1970)],labels=years[which(years>=1960 & years<1970)]-1900,cex=0.75,col=phasecolor[1],font=2)
text(x_plot[which(years>=1970 & years<1980)],y_plot[which(years>=1970 & years<1980)],labels=years[which(years>=1970 & years<1980)]-1900,cex=0.75,col=phasecolor[2],font=2)
text(x_plot[which(years>=1980 & years<1990)],y_plot[which(years>=1980 & years<1990)],labels=years[which(years>=1980 & years<1990)]-1900,cex=0.75,col=phasecolor[3],font=2)
text(x_plot[which(years>=1990 & years<2000)],y_plot[which(years>=1990 & years<2000)],labels=years[which(years>=1990 & years<2000)]-1900,cex=0.75,col=phasecolor[4],font=2)
text(x_plot[which(years>=2000 & years<2010)],y_plot[which(years>=2000 & years<2010)],labels=paste("0",years[which(years>=2000 & years<2010)]-2000,sep=""),cex=0.75,col=phasecolor[5],font=2)
text(x_plot[which(years>=2010 & years<endyr+1)],y_plot[which(years>=2010 & years<endyr+1)],labels=years[which(years>=2010 & years<endyr+1)]-2000,cex=0.75,col=phasecolor[6],font=2)

points(x_plot[which(years==endyr+1)],y_plot[which(years==endyr+1)],pch=16,cex=1,col="red")
text(x_plot[which(years==endyr+1)],y_plot[which(years==endyr+1)],labels=endyr+1,cex=1.5,col="red",font=2,pos=3)
points(x_plot[which(years==endyr+2)],y_plot[which(years==endyr+2)],pch=16,cex=1,col="black")
text(x_plot[which(years==endyr+2)],y_plot[which(years==endyr+2)],labels=endyr+2,cex=1.25,col="black",font=2,pos=2)


segments(0.05,0,0.4/0.35,1,col="red",lwd=2.5)
segments(0.4/0.35,1,4,1,col="blue",lwd=2.5)
segments(0.05,0,0.4/0.35,Fabc,col="red",lwd=2.5,lty=2)
segments(0.4/0.35,Fabc,4,Fabc,col="blue",lwd=2.5,lty=2)
mtext(expression(italic(F/F["35%"])),at=1.2,side=2,line=3.5,cex=1.5)
mtext(expression(italic(SSB/B["35%"])),side=1,line=3.5,cex=1.5)







