
#####################################################################################################
######### Plot phase plane figure
#####################################################################################################



layout(matrix(c(0,0,1,2,3,4,5,6,0,0), 5, 2, byrow = TRUE),heights=c(0.05,0.5,0.5,0.5,0.05))

par(mar=c(4,7,0.1,0.1),family="A")
h<-hist(mcmc_other[,which(colnames(mcmc_other)=="q_srv1")],plot=FALSE,breaks=37)
h$counts<-h$counts/sum(h$counts)
plot(h,main="",ylab="",xlab="",las=2,xaxt="n",cex.axis=1.25,col=c(rainbow(length(h$density[1:which(h$density==max(h$density))]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[1:which(h$density==max(h$density))]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67),rev(rainbow(length(h$density[(which(h$density==max(h$density))+1):length(h$density)]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67))))
axis(1,cex.axis=1.25)
mtext(expression("Trawl Survey Catchability ("*italic(q)*")"),side=1,line=2.7,cex=1.25)
segments(STD$value[which(STD$name=="q_srv1")],0,STD$value[which(STD$name=="q_srv1")],1,lwd=3,col="white")

par(mar=c(4,0.1,0.1,7))
h<-hist(mcmc_other[,which(colnames(mcmc_other)=="ABC")]/1000,plot=FALSE,breaks=37)
h$counts<-h$counts/sum(h$counts)
plot(h,main="",ylab="",xlab="",las=2,xaxt="n",yaxt="n",cex.axis=1.25,col=c(rainbow(length(h$density[1:which(h$density==max(h$density))]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[1:which(h$density==max(h$density))]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67),rev(rainbow(length(h$density[(which(h$density==max(h$density))+1):length(h$density)]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67))))
axis(1,cex.axis=1.25)
axis(4,cex.axis=1.25,las=2)
mtext("Acceptable Biological Catch (ABC, kt)",side=1,line=2.7,cex=1.25)
segments(STD$value[which(STD$name=="ABC")]/1000,0,STD$value[which(STD$name=="ABC")]/1000,1,lwd=3,col="white")

par(mar=c(4,7,0.1,0.1))
h<-hist(mcmc_other[,which(colnames(mcmc_other)=="natmort")],plot=FALSE,breaks=37)
h$counts<-h$counts/sum(h$counts)
plot(h,main="",ylab="",xlab="",las=2,xaxt="n",cex.axis=1.25,col=c(rainbow(length(h$density[1:which(h$density==max(h$density))]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[1:which(h$density==max(h$density))]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67),rev(rainbow(length(h$density[(which(h$density==max(h$density))+1):length(h$density)]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67))))
axis(1,cex.axis=1.25)
mtext(expression("Natural Mortality ("*italic(M)*")"),side=1,line=2.7,cex=1.25)
segments(STD$value[which(STD$name=="nattymort")],0,STD$value[which(STD$name=="nattymort")],1,lwd=3,col="white")
mtext("Probability Density",side=2,line=4.5,cex=1.5)

par(mar=c(4,0.1,0.1,7))
h<-hist(mcmc_other[,which(colnames(mcmc_other)==paste("tot_biom_",yrs[length(yrs)],sep=""))]/1000,plot=FALSE,breaks=37)
h$counts<-h$counts/sum(h$counts)
plot(h,main="",ylab="",xlab="",las=2,xaxt="n",yaxt="n",cex.axis=1.25,col=c(rainbow(length(h$density[1:which(h$density==max(h$density))]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[1:which(h$density==max(h$density))]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67),rev(rainbow(length(h$density[(which(h$density==max(h$density))+1):length(h$density)]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67))))
axis(1,cex.axis=1.25)
axis(4,cex.axis=1.25,las=2)
mtext("Current Total Biomass (kt)",side=1,line=2.7,cex=1.25)
segments(STD$value[which(STD$name=="tot_biom")[length(yrs)]]/1000,0,STD$value[which(STD$name=="tot_biom")[length(yrs)]]/1000,1,lwd=3,col="white")

par(mar=c(4,7,0.1,0.1))
h<-hist(mcmc_other[,which(colnames(mcmc_other)=="F40")],plot=FALSE,breaks=37)
h$counts<-h$counts/sum(h$counts)
plot(h,main="",ylab="",xlab="",las=2,xaxt="n",cex.axis=1.25,col=c(rainbow(length(h$density[1:which(h$density==max(h$density))]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[1:which(h$density==max(h$density))]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67),rev(rainbow(length(h$density[(which(h$density==max(h$density))+1):length(h$density)]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67))))
axis(1,cex.axis=1.25)
mtext(expression(italic(F)["40%"]),side=1,line=2.7,cex=1.25)
segments(STD$value[which(STD$name=="F40")],0,STD$value[which(STD$name=="F40")],1,lwd=3,col="white")

par(mar=c(4,0.1,0.1,7))
h<-hist(mcmc_other[,which(colnames(mcmc_other)==paste("spawn_biom_",yrs[length(yrs)],sep=""))]/1000,plot=FALSE,breaks=37)
h$counts<-h$counts/sum(h$counts)
plot(h,main="",ylab="",xlab="",las=2,xaxt="n",yaxt="n",cex.axis=1.25,col=c(rainbow(length(h$density[1:which(h$density==max(h$density))]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[1:which(h$density==max(h$density))]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67),rev(rainbow(length(h$density[(which(h$density==max(h$density))+1):length(h$density)]),s=0.75,v=0.95,start=0.4+0.27*(max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))-length(h$density[(which(h$density==max(h$density))+1):length(h$density)]))/max(length(h$density[1:which(h$density==max(h$density))]),length(h$density[(which(h$density==max(h$density))+1):length(h$density)])),end=0.67))))
axis(1,cex.axis=1.25)
axis(4,cex.axis=1.25,las=2)
mtext("Current Spawning Biomass (kt)",side=1,line=2.7,cex=1.25)
segments(STD$value[which(STD$name=="spawn_biom")[length(yrs)]]/1000,0,STD$value[which(STD$name=="spawn_biom")[length(yrs)]]/1000,1,lwd=3,col="white")








