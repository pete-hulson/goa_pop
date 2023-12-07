
#####################################################################################################
######### Plot observed and estimated catch 
#####################################################################################################


mcmc_srv1_biom<-mcmc_other[,which(colnames(mcmc_other)>=paste("srv1_biom_",yrs_srv1_biom[1],sep="") & colnames(mcmc_other)<=paste("srv1_biom_",yrs_srv1_biom[length(yrs_srv1_biom)],sep=""))]

uci_srv1_biom_mcmc<-vector(length=length(yrs_srv1_biom))
lci_srv1_biom_mcmc<-vector(length=length(yrs_srv1_biom))

for(y in 1:length(yrs_srv1_biom)){
uci_srv1_biom_mcmc[y]<-quantile(mcmc_srv1_biom[,y],probs=0.975)/1000
lci_srv1_biom_mcmc[y]<-quantile(mcmc_srv1_biom[,y],probs=0.025)/1000}

ylim<-c(0,round(1.05*max(uci_srv1_biom/1000)))

par(mar=c(3,4,0.1,0.777),family="A")

plot(yrs_srv1_biom,obs_srv1_biom/1000,las=2,type="p",pch=16,lwd=3,xaxt="n",ylab="",xlab="",ylim=ylim)
polygon(c(yrs_srv1_biom,sort(yrs_srv1_biom,decreasing=TRUE)),c(uci_srv1_biom_mcmc,rev(lci_srv1_biom_mcmc)),col="light grey",border=NA)
#lines(yrs_srv1_biom,uci_srv1_biom_mcmc,lty=2)
#lines(yrs_srv1_biom,lci_srv1_biom_mcmc,lty=2)
lines(yrs_srv1_biom,pred_srv1_biom/1000,lwd=3)
points(yrs_srv1_biom,obs_srv1_biom/1000,pch=16)
arrows(yrs_srv1_biom,obs_srv1_biom/1000,yrs_srv1_biom,uci_srv1_biom/1000,angle=90,length=0.05)
arrows(yrs_srv1_biom,obs_srv1_biom/1000,yrs_srv1_biom,lci_srv1_biom/1000,angle=90,length=0.05)
lines(yrs_srv1_biom[1:(length(yrs_srv1_biom)-1)],pred_srv1_biom_20/1000,lwd=3,lty=2,col="aquamarine4")
#lines(yrs_srv1_biom,pred_srv1_biom_20/1000,lwd=3,lty=2,col="aquamarine4")
legend(yrs_srv1_biom[2],2200,legend=c("Observed","2020 predicted","2021 predicted"),col=c("black","aquamarine4","black"),lwd=c(NA,3,3),lty=c(NA,2,1),pch=c(16,NA,NA),bty="n")
mtext("Trawl survey biomass (kt)",cex=1.25,side=2,line=3)
axis(1)
mtext("Year",cex=1.25,side=1,line=2)



# GG-plot for presentation

survB_plot<-as.data.frame(cbind(yrs_srv1_biom,obs_srv1_biom/1000,uci_srv1_biom/1000,lci_srv1_biom/1000))
names(survB_plot)<-c("yrs","srv","uci","lci")

ggplot(survB_plot, aes(x=yrs, y=srv)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=.5) +
  geom_point(colour="darkblue",size=3) +
  xlab("Year") +
  ylab("Trawl survey biomass (kt)") +
  theme(axis.title.x=element_text(family="A",size=14)) +
  theme(axis.title.y=element_text(family="A",size=14)) +
  theme(axis.text.x=element_text(family="A",size=14)) +
  theme(axis.text.y=element_text(family="A",size=14))



