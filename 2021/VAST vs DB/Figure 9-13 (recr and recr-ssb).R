
#####################################################################################################
######### Plot fishery and survey selectivity 
#####################################################################################################


mcmc_rec<-exp(mcmc_other[,which(colnames(mcmc_other)=="log_mean_rec")]+mcmc_other[,which(colnames(mcmc_other)>=paste("log_rec_dev_",yrs[1],sep="") & colnames(mcmc_other)<=paste("log_rec_dev",yrs[length(yrs)],sep=""))])

uci_rec<-vector(length=length(yrs))
lci_rec<-vector(length=length(yrs))

for(y in 1:length(yrs)){
uci_rec[y]<-quantile(mcmc_rec[,y],probs=0.975)
lci_rec[y]<-quantile(mcmc_rec[,y],probs=0.025)}

ylim=c(0,1.05*max(uci_rec))


layout(matrix(c(0,1,2,0), 4, 1, byrow = TRUE),heights=c(0.07,0.5,0.5,0.07))

par(mar=c(2,7,0.1,7),family="A")

x<-barplot(pred_rec[1:(length(yrs)-2)],col="aquamarine4",las=2,xaxt="n",ylab="",xlab="",ylim=ylim,cex.axis=1.5)
arrows(x,pred_rec[1:(length(yrs)-2)],x,uci_rec[1:(length(yrs)-2)],angle=90,length=0.05)
arrows(x,pred_rec[1:(length(yrs)-2)],x,lci_rec[1:(length(yrs)-2)],angle=90,length=0.05)
points(x[1:length(pred_rec_17)],pred_rec_17,pch=16,col="red")
axis(1,at=x,labels=yrs[1:(length(yrs)-2)]-2,tick=FALSE,line=-0.5,cex.axis=1.5)
#mtext("Recruitment (age-2, millions)",side=2,line=3.5,cex=1.25)
mtext("Year class",side=1,line=2,cex=1.25)

par(mar=c(2,7,2,7),family="A")

x_plot<-sp_biom[1:(length(yrs)-2)]/1000
y_plot<-pred_rec[3:length(yrs)]

yr_class<-yrs[1:(length(yrs)-2)]

plot(x_plot,y_plot,type="n",ylab="",xlab="",las=2,xaxt="n",ylim=c(0,300),xlim=c(0,250),cex.axis=1.5)
axis(1,cex.axis=1.5)

phasecolor<- c("black","red","darkorange","forestgreen","deepskyblue","blue")
text(x_plot[which(yr_class>=1960 & yr_class<1970)],y_plot[which(yr_class>=1960 & yr_class<1970)],labels=yr_class[which(yr_class>=1960 & yr_class<1970)]-1900,col=phasecolor[1],font=2)
text(x_plot[which(yr_class>=1970 & yr_class<1980)],y_plot[which(yr_class>=1970 & yr_class<1980)],labels=yr_class[which(yr_class>=1970 & yr_class<1980)]-1900,col=phasecolor[2],font=2)
text(x_plot[which(yr_class>=1980 & yr_class<1990)],y_plot[which(yr_class>=1980 & yr_class<1990)],labels=yr_class[which(yr_class>=1980 & yr_class<1990)]-1900,col=phasecolor[3],font=2)
text(x_plot[which(yr_class>=1990 & yr_class<2000)],y_plot[which(yr_class>=1990 & yr_class<2000)],labels=yr_class[which(yr_class>=1990 & yr_class<2000)]-1900,col=phasecolor[4],font=2)
text(x_plot[which(yr_class>=2000 & yr_class<2010)],y_plot[which(yr_class>=2000 & yr_class<2010)],labels=paste("0",yr_class[which(yr_class>=2000 & yr_class<2010)]-2000,sep=""),col=phasecolor[5],font=2)
text(x_plot[which(yr_class>=2010 & yr_class<2020)],y_plot[which(yr_class>=2010 & yr_class<2020)],labels=yr_class[which(yr_class>=2010 & yr_class<2020)]-2000,col=phasecolor[6],font=2)

mtext("Recruitment (age-2, millions)",side=2,line=3.5,cex=1.25,at=325)

#mtext("Recruitment (millions)",side=2,line=3.5,cex=1.25)
mtext("SSB (kt)",side=1,line=2.5,cex=1.25)

