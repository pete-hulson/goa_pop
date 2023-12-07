
#####################################################################################################
######### Plot observed and estimated catch 
#####################################################################################################

mcmc_tot_biom<-mcmc_other[,which(colnames(mcmc_other)>=paste("tot_biom_",yrs[1],sep="") & colnames(mcmc_other)<=paste("tot_biom_",yrs[length(yrs)],sep=""))]
mcmc_spawn_biom<-mcmc_other[,which(colnames(mcmc_other)>=paste("spawn_biom_",yrs[1],sep="") & colnames(mcmc_other)<=paste("spawn_biom_",yrs[length(yrs)],sep=""))]

uci_tot_biom<-vector(length=length(yrs))
lci_tot_biom<-vector(length=length(yrs))
uci_spawn_biom<-vector(length=length(yrs))
lci_spawn_biom<-vector(length=length(yrs))

for(y in 1:length(yrs)){
uci_tot_biom[y]<-quantile(mcmc_tot_biom[,y],probs=0.975)
lci_tot_biom[y]<-quantile(mcmc_tot_biom[,y],probs=0.025)
uci_spawn_biom[y]<-quantile(mcmc_spawn_biom[,y],probs=0.975)
lci_spawn_biom[y]<-quantile(mcmc_spawn_biom[,y],probs=0.025)}


layout(matrix(c(0,1,2,0), 4, 1, byrow = TRUE),heights=c(0.1,0.5,0.5,0.1))

par(mar=c(0.1,6,0.1,0.1),family="A")

ylim<-c(0,round(1.05*max(uci_tot_biom/1000)))
plot(yrs,tot_biom/1000,las=2,type="l",lwd=3,cex.axis=1.5,xaxt="n",ylab="",xlab="",ylim=ylim,cex=1.5)
polygon(c(yrs,sort(yrs,decreasing=TRUE)),c(uci_tot_biom/1000,rev(lci_tot_biom/1000)),col="light grey",border=NA)
lines(yrs,tot_biom/1000,lwd=3)
#lines(yrs,uci_tot_biom/1000,lty=2)
#lines(yrs,lci_tot_biom/1000,lty=2)
mtext("Total biomass (kt)",cex=1.5,side=2,line=4)
lines(yrs[1:length(tot_biom_20)],tot_biom_20/1000,lwd=3,lty=2,col="aquamarine4")
legend(yrs[7],1200,legend=c("2020 predicted","2021 predicted"),cex=1.5,col=c("aquamarine4","black"),lwd=c(3,3),lty=c(2,1),bty="n")

ylim<-c(0,round(1.05*max(uci_spawn_biom/1000)))
plot(yrs,sp_biom/1000,las=2,type="l",lwd=3,cex.axis=1.5,xaxt="n",ylab="",xlab="",ylim=ylim,cex=1.5)
polygon(c(yrs,sort(yrs,decreasing=TRUE)),c(uci_spawn_biom/1000,rev(lci_spawn_biom/1000)),col="light grey",border=NA)
lines(yrs,sp_biom/1000,lwd=3)
#lines(yrs,uci_spawn_biom/1000,lty=2)
#lines(yrs,lci_spawn_biom/1000,lty=2)
mtext("Spawning biomass (kt)",cex=1.5,side=2,line=4)
lines(yrs[1:length(sp_biom_20)],sp_biom_20/1000,lwd=3,lty=2,col="aquamarine4")

axis(1,cex.axis=1.5)

mtext("Year",cex=1.5,side=1,line=3)



