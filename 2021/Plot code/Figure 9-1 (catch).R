
#####################################################################################################
######### Plot observed and estimated catch 
#####################################################################################################


layout(matrix(c(0,1,2,0), 4, 1, byrow = TRUE),heights=c(0.05,0.5,0.5,0.05))


par(mar=c(3,5,0.1,0.3777),family="A")

plot(yrs,obs_catch/1000,las=2,type="l",lwd=3,cex.axis=1.5,xaxt="n",ylab="",xlab="",ylim=c(0,350))
axis(1,cex.axis=1.5)
mtext("Catch (kt)",cex=1.5,side=2,line=3.5)
lines(yrs,pred_catch/1000,col="aquamarine4",lty=2,lwd=2)
legend(1997,150,cex=1.5,legend=c("Observed","Predicted"),col=c("black","aquamarine4"),lwd=c(3,3),lty=c(1,2),bty="n")

plot(yrs[which(yrs==1995):length(yrs)],obs_catch[which(yrs==1995):length(yrs)]/1000,las=2,type="l",lwd=3,cex.axis=1.5,xaxt="n",ylab="",xlab="",ylim=c(0,30))
axis(1,cex.axis=1.5)
mtext("Recent Catch (kt)",cex=1.5,side=2,line=3.5)
lines(yrs[which(yrs==1995):length(yrs)],pred_catch[which(yrs==1995):length(yrs)]/1000,col="aquamarine4",lty=2,lwd=2)
mtext("Year",side=1,cex=1.5,line=3)



