
#####################################################################################################
######### Plot fishery and survey selectivity 
#####################################################################################################

yrs_plot<-yrs[which(yrs>=1977 & yrs<=yrs[length(yrs)]-2)]
rec_new<-pred_rec[which(yrs>=1977 & yrs<=yrs[length(yrs)]-2)]
rec_old<-pred_rec_20[which(yrs>=1977 & yrs<=yrs[length(yrs)]-2)]

rec_new_anom<-log(rec_new)-mean(log(rec_new))
rec_old_anom<-log(rec_old)-mean(log(rec_old))

space_mat<-matrix(nrow=length(rec_new_anom),ncol=2)
space_mat[,1]<-0
space_mat[,2]<-1

space_vec<-vector(length=2*length(rec_new_anom))
space_vec[seq(1,length(space_vec),2)]<-1
space_vec[seq(2,length(space_vec),2)]<-0

plot_vec<-vector(length=2*length(rec_new_anom))
plot_vec[seq(1,length(plot_vec),2)]<-rec_new_anom
plot_vec[seq(2,length(plot_vec),2)]<-rec_old_anom

par(mar=c(4,4,1,1),family="A")

x<-barplot(plot_vec,beside=TRUE,space=space_vec,col=c("deepskyblue","red"),las=2,xaxt="n",ylab="",xlab="",ylim=c(-1,1))

axis(1,at=x[seq(2,length(space_vec),2)],labels=yrs_plot-2,tick=FALSE,line=-0.5,las=2)

mtext("log Recruitment anomalies",side=2,line=2.777,cex=1.25)
mtext("Year class",side=1,line=2,cex=1.25)


