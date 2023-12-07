
#####################################################################################################
######### Get param ests 
#####################################################################################################


params<-matrix(nrow=5,ncol=7)
rownames(params)<-c("q","M","F40","SSB_proj","ABC")
colnames(params)<-c("mu","mu_mcmc","median_mcmc","sig","sig_mcmc","lci","uci")

params[1,1]<-STD$value[which(STD$name=="q_srv1")]
params[1,2]<-mean(mcmc_other[,which(colnames(mcmc_other)=="q_srv1")])
params[1,3]<-median(mcmc_other[,which(colnames(mcmc_other)=="q_srv1")])
params[1,4]<-STD$std[which(STD$name=="q_srv1")]
params[1,5]<-sd(mcmc_other[,which(colnames(mcmc_other)=="q_srv1")])
params[1,6]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="q_srv1")],probs=0.025)
params[1,7]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="q_srv1")],probs=0.975)

params[2,1]<-STD$value[which(STD$name=="nattymort")]
params[2,2]<-mean(mcmc_other[,which(colnames(mcmc_other)=="natmort")])
params[2,3]<-median(mcmc_other[,which(colnames(mcmc_other)=="natmort")])
params[2,4]<-STD$std[which(STD$name=="nattymort")]
params[2,5]<-sd(mcmc_other[,which(colnames(mcmc_other)=="natmort")])
params[2,6]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="natmort")],probs=0.025)
params[2,7]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="natmort")],probs=0.975)

params[3,1]<-STD$value[which(STD$name=="F40")]
params[3,2]<-mean(mcmc_other[,which(colnames(mcmc_other)=="F40")])
params[3,3]<-median(mcmc_other[,which(colnames(mcmc_other)=="F40")])
params[3,4]<-STD$std[which(STD$name=="F40")]
params[3,5]<-sd(mcmc_other[,which(colnames(mcmc_other)=="F40")])
params[3,6]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="F40")],probs=0.025)
params[3,7]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="F40")],probs=0.975)

params[4,1]<-STD$value[which(STD$name=="spawn_biom_proj")[1]]
params[4,2]<-mean(mcmc_other[,which(colnames(mcmc_other)==paste("spawn_biom_proj_",yrs[length(yrs)]+1,sep=""))])
params[4,3]<-median(mcmc_other[,which(colnames(mcmc_other)==paste("spawn_biom_proj_",yrs[length(yrs)]+1,sep=""))])
params[4,4]<-STD$std[which(STD$name=="spawn_biom_proj")[1]]
params[4,5]<-sd(mcmc_other[,which(colnames(mcmc_other)==paste("spawn_biom_proj_",yrs[length(yrs)]+1,sep=""))])
params[4,6]<-quantile(mcmc_other[,which(colnames(mcmc_other)==paste("spawn_biom_proj_",yrs[length(yrs)]+1,sep=""))],probs=0.025)
params[4,7]<-quantile(mcmc_other[,which(colnames(mcmc_other)==paste("spawn_biom_proj_",yrs[length(yrs)]+1,sep=""))],probs=0.975)

params[5,1]<-STD$value[which(STD$name=="ABC")]
params[5,2]<-mean(mcmc_other[,which(colnames(mcmc_other)=="ABC")])
params[5,3]<-median(mcmc_other[,which(colnames(mcmc_other)=="ABC")])
params[5,4]<-STD$std[which(STD$name=="ABC")]
params[5,5]<-sd(mcmc_other[,which(colnames(mcmc_other)=="ABC")])
params[5,6]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="ABC")],probs=0.025)
params[5,7]<-quantile(mcmc_other[,which(colnames(mcmc_other)=="ABC")],probs=0.975)

write.csv(params,paste(path,"/table_9_16.csv",sep=""))


