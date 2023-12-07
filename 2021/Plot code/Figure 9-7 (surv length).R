
#####################################################################################################
######### Plot observed and estimated survey length comps 
#####################################################################################################


rows<-length(osc_srv1[,1])-2
rw<-seq(from=1,to=rows)
ylim<-c(0,1.05*max(osc_srv1[,2:length(osc_srv1[1,])]))

layout(matrix(c(0,rw,0),rows+2,1,byrow = TRUE),heights=c(1,seq(1,1,length.out=rows),1))
par(mar=c(0,6,0,0.1),family="A")


for(y in 1:rows){
x<-barplot(osc_srv1[y+2,2:length(osc_srv1[y,])],col="grey",xaxt="n",las=2,ylab="",yaxt="n",ylim=ylim,border="black")
box(col="black",lwd=0.5)
if(y==8){
mtext("Survey length composition",side=2,cex=1.25,line=4)}
text(x[2],0.13,osc_srv1[y+2,1],cex=1.25)
axis(2,at=0,labels="0.00",las=2,cex.axis=1.25)
axis(2,at=mean(ylim),labels=round(mean(ylim),digits=2),las=2,cex.axis=1.25)
if(y==1){axis(2,at=max(ylim),labels=round(max(ylim),digits=2),las=2,cex.axis=1.25)}}
axis(1,at=x,labels=lenbins,tick=FALSE,line=-0.777,cex.axis=1.25)
mtext("Size",side=1,cex=1.25,line=2)

