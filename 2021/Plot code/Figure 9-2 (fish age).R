
#####################################################################################################
######### Plot observed and estimated fishery age comps 
#####################################################################################################


rows<-length(oac_fish[,1])
rw<-seq(from=1,to=rows)
nages<-length(oac_fish[1,2:length(oac_fish[1,])])
years<-max(oac_fish[,1])-min(oac_fish[,1])
ylim<-c(0,1.05*max(oac_fish[,2:length(oac_fish[1,])],eac_fish[,2:length(oac_fish[1,])]))
Year<-oac_fish[,1]
ncolors<-nages+years
cohort.color<-rainbow(ncolors)
diff<-0

layout(matrix(c(0,rw,0),rows+2,1,byrow = TRUE),heights=c(0.15,seq(1,1,length.out=rows),0.9))
par(mar=c(0,6,0,0.1),family="A")

for(y in 1:rows){
x<-barplot(oac_fish[y,2:length(oac_fish[y,])],col=cohort.color[(1+years-diff):(nages+years-diff)],xaxt="n",las=2,ylab="",yaxt="n",ylim=ylim)
box(col="black",lwd=0.5)
lines(x,eac_fish[y,2:length(oac_fish[y,])],lty=2,pch=16,type="b",cex=1.25)
text(x[2],0.15,oac_fish[y,1],cex=1.25)
axis(2,at=0,labels="0.00",las=2,cex.axis=1.25)
axis(2,at=mean(ylim),labels=round(mean(ylim),digits=2),las=2,cex.axis=1.25)
if(y==1){axis(2,at=max(ylim),labels=round(max(ylim),digits=2),las=2,cex.axis=1.25)}
if(y==8){mtext("Fishery age composition",at=0,side=2,cex=1.25,line=4)}
diff<-Year[y+1]-Year[1]}
axis(1,at=x,labels=ages,tick=FALSE,line=-0.777,cex.axis=1.25)
mtext("Age",side=1,cex=1.25,line=1.77)


# Most recent 3 years

rows<-3
rw<-seq(from=1,to=rows)
nages<-length(oac_fish[1,2:length(oac_fish[1,])])
years<-max(oac_fish[,1])-min(oac_fish[,1])
ylim<-c(0,1.05*max(oac_fish[,2:length(oac_fish[1,])],eac_fish[,2:length(oac_fish[1,])]))
Year<-oac_fish[,1]
ncolors<-nages+years
cohort.color<-rainbow(ncolors)
diff<-0

layout(matrix(c(0,rw,0),rows+2,1,byrow = TRUE),heights=c(0.15,seq(1,1,length.out=rows),0.9))
par(mar=c(0,6,0,0.1))

for(y in 1:rows){
x<-barplot(oac_fish[y+12,2:length(oac_fish[y,])],col=cohort.color[(1+years-diff):(nages+years-diff)],xaxt="n",las=2,ylab="",yaxt="n",ylim=ylim)
box(col="black",lwd=0.5)
text(x[2],0.15,oac_fish[y+12,1],cex=1.25)
axis(2,at=0,labels="0.00",las=2,cex.axis=1.25)
axis(2,at=mean(ylim),labels=round(mean(ylim),digits=2),las=2,cex.axis=1.25)
if(y==1){axis(2,at=max(ylim),labels=round(max(ylim),digits=2),las=2,cex.axis=1.25)}
if(y==13){mtext("Fishery age composition",side=2,cex=1.25,line=4)}
diff<-Year[y+1]-Year[1]}
axis(1,at=x,labels=ages,tick=FALSE,line=-0.777,cex.axis=1.25)
mtext("Age",side=1,cex=1.25,line=1.77)
