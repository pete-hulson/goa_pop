
# Get working directories

path<-getwd()
pathM<-paste(path,"/2020.1 (2021)",sep="")
pathR<-paste(path,"/Results",sep="")

# Get data and report files and set up sections for ESS iteration

DAT_init<-readLines(paste(pathM,"/goa_pop_2021_init.dat",sep=""),warn=FALSE)
Sec_brks_DAT<-grep("#--",DAT_init)
st_end_DAT<-matrix(NA,nrow=length(Sec_brks_DAT)+1,ncol=2)
st_end_DAT[1,1]<-1
st_end_DAT[1:(length(st_end_DAT[,1])-1),2]<-Sec_brks_DAT
st_end_DAT[2:length(st_end_DAT[,1])]<-Sec_brks_DAT+2
st_end_DAT[length(st_end_DAT[,1]),2]<-length(DAT_init)

ESS_FA_init<-as.numeric(strsplit(DAT_init[(st_end_DAT[1,2]+1):(st_end_DAT[2,1]-1)]," ")[[1]])
ESS_SA_init<-as.numeric(strsplit(DAT_init[(st_end_DAT[2,2]+1):(st_end_DAT[3,1]-1)]," ")[[1]])
ESS_FS_init<-as.numeric(strsplit(DAT_init[(st_end_DAT[3,2]+1):(st_end_DAT[4,1]-1)]," ")[[1]])

yrs_FA<-as.numeric(strsplit(DAT_init[(st_end_DAT[1,2]-5)]," ")[[1]])
yrs_SA<-as.numeric(strsplit(DAT_init[(st_end_DAT[2,2]-5)]," ")[[1]])
yrs_FS<-as.numeric(strsplit(DAT_init[(st_end_DAT[3,2]-5)]," ")[[1]])

REP_init<-readLines(paste(pathM,"/Model_20_1.rep",sep=""),warn=FALSE)
Sec_st_brks_REP<-grep("#--",REP_init)
Sec_end_brks_REP<-grep("#!!",REP_init)
st_end_REP<-matrix(NA,nrow=length(Sec_st_brks_REP),ncol=2)
st_end_REP[,1]<-Sec_st_brks_REP
st_end_REP[,2]<-Sec_end_brks_REP

CTL_init<-readLines(paste(pathM,"/goa_pop_2021_init.ctl",sep=""),warn=FALSE)
wt_FA_line<-grep("# wt_fish_age",CTL_init)
wt_SA_line<-grep("# wt_srv1_age",CTL_init)
wt_FS_line<-grep("# wt_fish_size",CTL_init)

ages<-seq(2,25)
lengths<-c(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)

# Set up results matrices
ITERS<-10

ESS_iter_FA<-matrix(nrow=ITERS+1,ncol=1)
rownames(ESS_iter_FA)<-c("Init",paste("Iter_",seq(1,ITERS),sep=""))
colnames(ESS_iter_FA)<-c("wt_FA")
ESS_iter_FA[1,1]<-1
SSQ_iter_FA<-matrix(nrow=ITERS,ncol=1)
rownames(SSQ_iter_FA)<-paste("Iter_",seq(1,ITERS),sep="")
colnames(SSQ_iter_FA)<-"SSQ_ESS_diff"

ESS_iter_SA<-matrix(nrow=ITERS+1,ncol=1)
rownames(ESS_iter_SA)<-c("Init",paste("Iter_",seq(1,ITERS),sep=""))
colnames(ESS_iter_SA)<-c("wt_SA")
ESS_iter_SA[1,1]<-1
SSQ_iter_SA<-matrix(nrow=ITERS,ncol=1)
rownames(SSQ_iter_SA)<-paste("Iter_",seq(1,ITERS),sep="")
colnames(SSQ_iter_SA)<-"SSQ_ESS_diff"

ESS_iter_FS<-matrix(nrow=ITERS+1,ncol=1)
rownames(ESS_iter_FS)<-c("Init",paste("Iter_",seq(1,ITERS),sep=""))
colnames(ESS_iter_FS)<-c("wt_FS")
ESS_iter_FS[1,1]<-1
SSQ_iter_FS<-matrix(nrow=ITERS,ncol=1)
rownames(SSQ_iter_FS)<-paste("Iter_",seq(1,ITERS),sep="")
colnames(SSQ_iter_FS)<-"SSQ_ESS_diff"

# Write initalized DAT file
write.table(DAT_init,file=paste(pathM,"/goa_pop_2021.dat",sep=""),quote=F,row.names=F,col.names=F)

start_time<-Sys.time()

# Start iteration loop
for (i in 1:ITERS){

print(paste("i = ",i,sep=""))

# Run model
setwd(pathM)
shell("Model_20_1.EXE")
#system("iterated_ess.EXE",show.output.on.console=FALSE)

# Read in report file and get output ESSs
REP<-readLines(paste(pathM,"/model_20_1.rep",sep=""),warn=FALSE)

line1<-grep("Obs_P_fish_age",REP)+1
line2<-grep("Pred_P_fish_age",REP)-2
fy<-length(REP[line1:line2])
obs_fa<-REP[line1:line2]
obs_p_fa<-matrix(nrow=fy,ncol=24)
line1<-grep("Pred_P_fish_age",REP)+1
line2<-grep("yrs_fish_age",REP)-2
pred_fa<-REP[line1:line2]
pred_p_fa<-matrix(nrow=fy,ncol=24)
v_y<-vector(length=fy)
w_denom<-vector(length=fy)
obs_bar<-vector(length=fy)
pred_bar<-vector(length=fy)
for(y in 1:fy){
pred_y<-as.numeric(strsplit(pred_fa[y]," ")[[1]])[3:(length(ages)+2)]
pred_bar[y]<-sum(ages*pred_y)
obs_y<-as.numeric(strsplit(obs_fa[y]," ")[[1]])[3:(length(ages)+2)]
obs_bar[y]<-sum(ages*obs_y)
v_y[y]<-sum(ages^2*pred_y)-pred_bar[y]^2
w_denom[y]<-(obs_bar[y]-pred_bar[y])/sqrt(v_y[y]/ESS_FA_init[y])}
wt_FA<-1/var(w_denom)
ESS_iter_FA[i+1,1]<-wt_FA
SSQ_iter_FA[i]<-sum((ESS_iter_FA[i+1,1]-ESS_iter_FA[i,1])^2)

line1<-grep("Obs_P_fish_size",REP)+1
line2<-grep("Pred_P_fish_size",REP)-2
fy<-length(REP[line1:line2])
obs_fs<-REP[line1:line2]
obs_p_fs<-matrix(nrow=fy,ncol=24)
line1<-grep("Pred_P_fish_size",REP)+1
line2<-grep("yrs_fish_size",REP)-2
pred_fs<-REP[line1:line2]
pred_p_fs<-matrix(nrow=fy,ncol=24)
v_y<-vector(length=fy)
w_denom<-vector(length=fy)
obs_bar<-vector(length=fy)
pred_bar<-vector(length=fy)
for(y in 1:fy){
pred_y<-as.numeric(strsplit(pred_fs[y]," ")[[1]])[3:(length(lengths)+2)]
pred_bar[y]<-sum(lengths*pred_y)
obs_y<-as.numeric(strsplit(obs_fs[y]," ")[[1]])[3:(length(lengths)+2)]
obs_bar[y]<-sum(lengths*obs_y)
v_y[y]<-sum(lengths^2*pred_y)-pred_bar[y]^2
w_denom[y]<-(obs_bar[y]-pred_bar[y])/sqrt(v_y[y]/ESS_FS_init[y])}
wt_FS<-1/var(w_denom)
ESS_iter_FS[i+1,1]<-wt_FS
SSQ_iter_FS[i]<-sum((ESS_iter_FS[i+1,1]-ESS_iter_FS[i,1])^2)

line1<-grep("Obs_P_srv1_age",REP)+1
line2<-grep("Pred_P_srv1_age",REP)-2
sy<-length(REP[line1:line2])
obs_sa<-REP[line1:line2]
obs_p_sa<-matrix(nrow=sy,ncol=24)
line1<-grep("Pred_P_srv1_age",REP)+1
line2<-grep("yrs_srv1_age",REP)-2
pred_sa<-REP[line1:line2]
pred_p_sa<-matrix(nrow=sy,ncol=24)
v_y<-vector(length=sy)
w_denom<-vector(length=sy)
obs_bar<-vector(length=sy)
pred_bar<-vector(length=sy)
for(y in 1:sy){
pred_y<-as.numeric(strsplit(pred_sa[y]," ")[[1]])[3:(length(ages)+2)]
pred_bar[y]<-sum(ages*pred_y)
obs_y<-as.numeric(strsplit(obs_sa[y]," ")[[1]])[3:(length(ages)+2)]
obs_bar[y]<-sum(ages*obs_y)
v_y[y]<-sum(ages^2*pred_y)-pred_bar[y]^2
w_denom[y]<-(obs_bar[y]-pred_bar[y])/sqrt(v_y[y]/ESS_SA_init[y])}
wt_SA<-1/var(w_denom)
ESS_iter_SA[i+1,1]<-wt_SA
SSQ_iter_SA[i]<-sum((ESS_iter_SA[i+1,1]-ESS_iter_SA[i,1])^2)

if(SSQ_iter_FA[i]+SSQ_iter_FS[i]+SSQ_iter_SA[i]==0){break;}

# Write new CTL file with iterated weights
CTL<-c(
CTL_init[1:(wt_FA_line-1)],
paste(wt_FA,"\t# wt_fish_age",sep=""),
paste(wt_SA,"\t# wt_srv1_age",sep=""),
paste(wt_FS,"\t# wt_fish_size",sep=""),
CTL_init[(wt_FS_line+1):length(CTL_init)])

write.table(CTL,file=paste(pathM,"/goa_pop_2021.ctl",sep=""),quote=F,row.names=F,col.names=F)

# end iteration loop
}

end_time<-Sys.time()

end_time-start_time

# Write results
write.csv(ESS_iter_FA,paste(pathR,"/ESS_iter_FA.csv",sep=""))
write.csv(ESS_iter_SA,paste(pathR,"/ESS_iter_SA.csv",sep=""))
write.csv(ESS_iter_FS,paste(pathR,"/ESS_iter_FS.csv",sep=""))

write.csv(SSQ_iter_FA,paste(pathR,"/SSQ_iter_FA.csv",sep=""))
write.csv(SSQ_iter_SA,paste(pathR,"/SSQ_iter_SA.csv",sep=""))
write.csv(SSQ_iter_FS,paste(pathR,"/SSQ_iter_FS.csv",sep=""))

