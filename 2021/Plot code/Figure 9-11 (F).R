
#####################################################################################################
######### Plot fishery and survey selectivity 
#####################################################################################################


avg_F<-mcmc_params$log_avg_F
F_devs<-mcmc_params[,which(names(mcmc_params)=="log_F_devs")]
F_full<-exp(avg_F+F_devs)

uci_F_full<-vector(length=length(yrs))
lci_F_full<-vector(length=length(yrs))

for(y in 1:length(yrs)){
uci_F_full[y]<-quantile(F_full[,y],probs=0.975)
lci_F_full[y]<-quantile(F_full[,y],probs=0.025)}

par(mar=c(4,4,1,1),family="A")


plot(yrs,F,type="l",lwd=3,ylab="",xlab="",las=2,xaxt="n",ylim=c(0,0.8))
axis(1)

polygon(c(yrs,sort(yrs,decreasing=TRUE)),c(uci_F_full,rev(lci_F_full)),col="light grey",border=NA)
lines(yrs,F,lwd=3)
#lines(yrs,uci_F_full,lty=2)
#lines(yrs,lci_F_full,lty=2)

mtext("Fishing Mortality Rate",side=2,line=2.8,cex=1.25)
mtext("Year",side=1,line=2.5,cex=1.25)






