#######################

######### HEY
# These model scenarios are old scenario names

# Get directories
#path<-getwd()
path<-"C:/AA - PH Run Stuff/POP21/MCMC"

#path<-"C:/Users/Pete.Hulson/Desktop/NOAA/Stock Assessments/Tier 3/2020/POP"

# Name model scenarios and paths
#path_15<-paste(path,"/Model 15 (base)",sep="")
#path_17_0<-paste(path,"/Model 17_0 (lenbins)",sep="")
#path_17_1a<-paste(path,"/Model 17_1a (drop SB)",sep="")
#path_17_1b<-paste(path,"/Model 17_1b (drop SB and AC)",sep="")
#path_17_2<-paste(path,"/Model 17_2 (lnN srvbiom)",sep="")
#path_17_3a<-paste(path,"/Model 17_3a (gamm All Y)",sep="")
#path_17_3b<-paste(path,"/Model 17_3b (old breaks, RP bk-gamma)",sep="")
#modpaths<-c(path_15,path_17_0,path_17_1a,path_17_1b,path_17_2,path_17_3a,path_17_3b)
#modnames<-c('Model_15','Model_17_0','Model_17_1a','Model_17_1b','Model_17_2','Model_17_3a','Model_17_3b')

path_20_1<-paste(path,"/Model",sep="")

modpaths<-c(path_20_1)
modnames<-c('Model_20_1')

# Define MCMC stuff
mcmcruns<-10000000
mcmcsave<-mcmcruns/5000
#mcmcruns<-100000
#mcmcsave<-mcmcruns/5000

# Run MCMC loop
start_time<-Sys.time()
for(m in length(modnames):length(modnames)){
setwd(modpaths[m])
shell(paste(modnames[m],'.exe',' -mcmc ',mcmcruns,'-mcsave ',mcmcsave))
shell(paste(modnames[m],'.exe',' -mceval'))}

end_time<-Sys.time()
end_time-start_time

(end_time-start_time)*100/60

