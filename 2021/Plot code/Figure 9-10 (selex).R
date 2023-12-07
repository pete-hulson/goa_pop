
#####################################################################################################
######### Plot fishery and survey selectivity 
#####################################################################################################




par(mar=c(4,4,1,1),family="A")


plot(ages,fish_sel_1[1:length(ages)],type="l",lwd=3,ylab="",xlab="",las=2,xaxt="n",col="aquamarine4")
lines(ages,mat[1:length(ages)],lwd=3,col="red")
lines(ages,fish_sel_2[1:length(ages)],lwd=3,col="aquamarine4",lty=2)
lines(ages,fish_sel_3[1:length(ages)],lwd=3,col="aquamarine4",lty=3)
lines(ages,fish_sel_4[1:length(ages)],lwd=3,col="blue")
lines(ages,srv1_sel[1:length(ages)],lwd=3)

axis(1)

legend(10,0.55,legend=c("Fishery 1961-1976","Fishery 1977-1995","Fishery 1996-2006","Fishery 2007-2020","Bottom trawl survey","Maturity"),lwd=3,col=c("aquamarine4","aquamarine4","aquamarine4","blue","black","red"),lty=c(1,2,3,1,1,1),bty="n")


mtext("Selectivity/Maturity",side=2,line=2.5,cex=1.25)
mtext("Age",side=1,line=2.5,cex=1.25)






