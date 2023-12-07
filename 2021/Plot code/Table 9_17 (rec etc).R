
#####################################################################################################
######### Get recruitment,etc, MCMC CIs
#####################################################################################################

mcmc_tot_biom<-cbind(mcmc_other[,which(colnames(mcmc_other)>=paste("tot_biom_",yrs[1],sep="") & colnames(mcmc_other)<=paste("tot_biom_",yrs[length(yrs)],sep=""))],mcmc_other[,which(colnames(mcmc_other)>=paste("tot_biom_proj_",yrs[length(yrs)]+1,sep="") & colnames(mcmc_other)<=paste("tot_biom_proj_",yrs[length(yrs)]+2,sep=""))])
mcmc_spawn_biom<-cbind(mcmc_other[,which(colnames(mcmc_other)>=paste("spawn_biom_",yrs[1],sep="") & colnames(mcmc_other)<=paste("spawn_biom_",yrs[length(yrs)],sep=""))],mcmc_other[,which(colnames(mcmc_other)>=paste("spawn_biom_proj_",yrs[length(yrs)]+1,sep="") & colnames(mcmc_other)<=paste("spawn_biom_proj_",yrs[length(yrs)]+2,sep=""))])
mcmc_rec<-cbind(exp(mcmc_other[,which(colnames(mcmc_other)=="log_mean_rec")]+mcmc_other[,which(colnames(mcmc_other)>=paste("log_rec_dev_",yrs[1],sep="") & colnames(mcmc_other)<=paste("log_rec_dev",yrs[length(yrs)],sep=""))]),mcmc_other[,which(colnames(mcmc_other)>=paste("rec_proj_",yrs[length(yrs)]+1,sep="") & colnames(mcmc_other)<=paste("rec_proj_",yrs[length(yrs)]+2,sep=""))])

rec<-c(pred_rec,mean(pred_rec[which(yrs>=(1977+2) & yrs<=(yrs[length(yrs)]-2))]),mean(pred_rec[which(yrs>=(1977+2) & yrs<=(yrs[length(yrs)]-2))]))
t_biom<-c(tot_biom,STD$value[which(STD$name=="tot_biom_proj")][1:2])
s_biom<-c(sp_biom,STD$value[which(STD$name=="spawn_biom_proj")][1:2])

uci_rec<-vector(length=length(yrs)+2)
lci_rec<-vector(length=length(yrs)+2)
uci_tot_biom<-vector(length=length(yrs)+2)
lci_tot_biom<-vector(length=length(yrs)+2)
uci_spawn_biom<-vector(length=length(yrs)+2)
lci_spawn_biom<-vector(length=length(yrs)+2)

for(y in 1:(length(yrs)+2)){
uci_rec[y]<-quantile(mcmc_rec[,y],probs=0.975)
lci_rec[y]<-quantile(mcmc_rec[,y],probs=0.025)
uci_spawn_biom[y]<-quantile(mcmc_spawn_biom[,y],probs=0.975)
lci_spawn_biom[y]<-quantile(mcmc_spawn_biom[,y],probs=0.025)
uci_tot_biom[y]<-quantile(mcmc_tot_biom[,y],probs=0.975)
lci_tot_biom[y]<-quantile(mcmc_tot_biom[,y],probs=0.025)}

year<-c(yrs,yrs[length(yrs)]+1,yrs[length(yrs)]+2)
rec<-round(rec*1000,digits=0)
rec_lci<-round(lci_rec*1000,digits=0)
rec_uci<-round(uci_rec*1000,digits=0)
TB<-round(t_biom,digits=0)
TB_lci<-round(lci_tot_biom,digits=0)
TB_uci<-round(uci_tot_biom,digits=0)
SB<-round(s_biom,digits=0)
SB_lci<-round(lci_spawn_biom,digits=0)
SB_uci<-round(uci_spawn_biom,digits=0)



table_10_14<-cbind(year,rec,rec_lci,rec_uci,TB,TB_lci,TB_uci,SB,SB_lci,SB_uci)

write.csv(table_10_14,paste(path,"/table_9_17.csv",sep=""))


