
#####################################################################################################
######### Plot swath plot for spawning biomass
#####################################################################################################

mcmc_spawn_biom<-cbind(mcmc_other[,which(colnames(mcmc_other)>=paste("spawn_biom_",yrs[1],sep="") & colnames(mcmc_other)<=paste("spawn_biom_",yrs[length(yrs)],sep=""))],mcmc_other[,which(colnames(mcmc_other)>=paste("spawn_biom_proj_",yrs[length(yrs)]+1,sep="") & colnames(mcmc_other)<=paste("spawn_biom_proj_",yrs[length(yrs)]+15,sep=""))])


ssb.quantiles<-apply(mcmc_spawn_biom,2,function(x){quantile(x,seq(.025,.975,.05))})
nquant<-nrow(ssb.quantiles)
ylim<-c(0,1.05*max(ssb.quantiles)/1000)
pall<-rainbow(9,s=0.75,v=0.95,start=0.52,end=0.70)
#pall<-terrain.colors(54,1)
mcmc_years<-c(yrs,seq(yrs[length(yrs)]+1,yrs[length(yrs)]+15))

par(mar=c(4,4,1,1),family="A")

plot(mcmc_years,ssb.quantiles[(nquant+1)/2,]/1000,type="b",ylim=ylim,las=1,xlab="",ylab="",yaxs="i")
for(i in 1:((nquant-1)/2)){polygon(c(mcmc_years,sort(mcmc_years,decreasing=TRUE)),c(ssb.quantiles[i,],rev(ssb.quantiles[nquant-i+1,]))/1000,col=paste(pall[i],sep=""),border=NA)} 
lines(mcmc_years,ssb.quantiles[(nquant+1)/2,]/1000,type="l",lwd=3,col="white")
lines(mcmc_years,rep(STD$value[which(STD$name=="B40")],length(mcmc_years))/1000,type="l", lty=2, lwd=3, col="red")	
lines(mcmc_years, rep(as.numeric(REP[(grep("B_35",REP)+1):(grep("F_40",REP)[1]-1)]),length(mcmc_years))/1000, type="l", lty=1, lwd=3)

mtext("Spawning Biomass (kt)",side=2,line=2.777,cex=1.25)
mtext("Year",side=1,line=2.5,cex=1.25)



