
#====================================================================================================
#================ Instructions
#====================================================================================================

# The code provided in this script automatically updates the ADMB DAT file
# in order to increase efficiency and reduce transcription errors from
# spreadsheets to text files. This code has been customized to GOA Pacific
# ocean perch (POP). To run this code for each year prior to the full assessment
# a couple of steps need to be taken. The following steps are provided as
# a reminder:
#	Step 1: Define the most recent 3 full years TAC (i.e., if the current year
#	        is X, these would be TACs for years X-3, X-2, and X-1) here:

		TAC<-c(29236,28555,31238)

#	Step 2: Copy data file and ctl file from most recent full assessment
#	   to Get Data\POP\Data Files\Model Data
#	    ~ Declare what the name of the ctl file is here:

		CTL_name<-"goa_pop_2020.ctl"

#	Step 3: Download most recent and available age agreement data to
#	   Get Data\POP\Data Files\Other Data (name it 'Age Agreement.csv')
#	    ~ Note: If the length bins changed from the last full assessment
#	            copy the new length bin labels into 'len_bin_labels.csv'.
#	Step 4: Define your oracle username and password here (for both AFSC and AKFIN):

		username_afsc="hulsonp"
		password_afsc="Liam1Fin2Bri3!"
		username_akfin="phulson"
		password_akfin="$blwins1"

#	Step 5: Run this code and enjoy your new data file! Code to automatically
#	   compare the new data to the old data to find differences has not
#	   yet been constructed, so, until then, make sure you check the new
#	   data from this to the data used in the most recent full assessment
#	   for differences. The DAT and CTL files will be written to the folder 
#	   Get Data/POP/ASSESSMENT DATA FILES and data to perform retropective 
#	   analysis will be written to 'Retrospective Data' in this folder.
#	Step 6: Copy DAT and CTL file to folder containing TPL code and run. Note that
#	   the CTL name is the same as the DAT, so make sure you change this name prior
#	   to running the model.

#====================================================================================================
#================ Get Working Directories, define format vectors, define data call parameters
#====================================================================================================

######### Get directories

path<-getwd()
path_DAT<-paste(path,"/Data Files",sep="")
path_FCN<-paste(path,"/Data Fcns",sep="")
path_TXT<-paste(path,"/Text Files",sep="")
path_MDL<-paste(path,"/Models",sep="")
path_UD<-paste(path,"/ASSESSMENT DATA FILES",sep="")



######### Define vector for formatting

Sep<-"#=========================================================================================================================="

######### Define data call parameters, ID, and password; source functions

afsc_species<-30060
norpac_species<-301
group_code<-'POPA'

source(paste(path_FCN,"/Get Trawl Survey Biomass.R",sep=""))
source(paste(path_FCN,"/Get Trawl Survey AC.R",sep=""))
source(paste(path_FCN,"/Get Trawl Survey SC.R",sep=""))
source(paste(path_FCN,"/Get Fishery AC.R",sep=""))
source(paste(path_FCN,"/Get Fishery SC.R",sep=""))
source(paste(path_FCN,"/Get Fishery Catch.R",sep=""))
source(paste(path_FCN,"/Get Growth.R",sep=""))
source(paste(path_FCN,"/Get A@A.R",sep=""))
source(paste(path_FCN,"/Compile DAT.R",sep=""))

#====================================================================================================
#===== Model Input Parameters/Values - Those that need changing each year are highlighted
#====================================================================================================

##### Ones that don't need changing each year
styr<-1961
endyr<-as.numeric(substr(Sys.time(),1,4))
recage<-2
nages_D<-24 # Here input the number of data ages, the script will automatically select model plus ages
n_ageage_mat<-1
n_sizeage_mat<-2
len_bin_labels<-read.csv(paste(path_DAT,"/Other Data/len_bin_labels.csv",sep=""))
nlenbins<-length(len_bin_labels$len_bins)
spawn_fract<-5
ages_D<-seq(recage,(recage+nages_D)-1)
plus_age_D<-(recage+nages_D)-1

#====================================================================================================
#================ Run functions to get data
#====================================================================================================

##### Get Ageing Error Matrix
ageage<-get_ageage(norpac_species,recage,nages_D)
nages_M<-length(ageage[,1])
ages_M<-seq(recage,(recage+nages_M)-1)

##### Get Growth Statistics: Size-Age Transition Matrix and weight-at-age
SzA_60z<-get_szaa_sixties(nages_M,ages_M,len_bin_labels$len_bins)
SzA_curr<-get_szaa_current(username_afsc,password_afsc,afsc_species,nages_M,ages_M,len_bin_labels$len_bins)
Wbar<-get_waa(nages_M,ages_M)

##### Get Fishery Catch
FC<-get_FC(norpac_species,group_code,username_akfin,password_akfin)
yeild_ratio<-FC[1]
obs_catch<-FC[2:length(FC)]

##### Get Trawl Survey Biomass
TSB<-get_TSB(afsc_species,username_afsc,password_afsc)

##### Get Fishery Age Composition
FAC<-get_FAC(norpac_species,username_akfin,password_akfin,plus_age_D,nages_D)

##### Get Trawl Survey Age Composition
TSAC<-get_TSAC(afsc_species,username_afsc,password_afsc,nages_D,plus_age_D)

##### Get Fishery Size Composition
FSC<-get_FSC(afsc_species,nlenbins,len_bin_labels$len_bins)

##### Get Trawl Survey Size Composition
TSSC<-get_TSSC(afsc_species,username_afsc,password_afsc,len_bin_labels$len_bins)

#====================================================================================================
#================ Compile and write data files
#====================================================================================================

######### Concatenate and write DAT file

DAT<-get_DAT(styr,endyr,recage,nages_D,nages_M,nlenbins,n_ageage_mat,n_sizeage_mat,len_bin_labels$len_bins,spawn_fract,Wbar,obs_catch,TSB,FAC,TSAC,FSC,TSSC,SzA_60z,SzA_curr,ageage)
DAT_NAME<-paste("goa_pop_",endyr,".dat",sep="")
write.table(DAT,file=paste(path_UD,"/",DAT_NAME,sep=""),quote=F,row.names=F,col.names=F)

######### Concatenate and write CTL file

CTL_OLD<-read.delim(paste(path_DAT,"/Model Data/",CTL_name,sep=""),sep="",header=F)
CTL<-matrix(nrow=length(CTL_OLD[,1]),ncol=1)
CTL[1,1]<-paste("Model_1","# model_name",sep="\t")
CTL[2,1]<-paste(DAT_NAME,"# data_file",sep="\t")
CTL[3,1]<-paste(CTL_OLD[3,1],paste(CTL_OLD[3,2],CTL_OLD[3,3]),sep="\t")
CTL[4,1]<-paste(endyr,"# endyr_rec_est",sep="\t")
for (i in 5:(length(CTL_OLD[,1])-1)){
CTL[i,1]<-paste(CTL_OLD[i,1],paste(CTL_OLD[i,2],CTL_OLD[i,3]),sep="\t")}
CTL[length(CTL_OLD[,1]),1]<-paste(yeild_ratio,"# yieldratio",sep="\t")
CTL_NAME<-paste("goa_pop_",endyr,".ctl",sep="")
write.table(CTL,file=paste(path_UD,"/",CTL_NAME,sep=""),quote=F,row.names=F,col.names=F)

######### Write data for retrospective analysis

write.table(DAT,file=paste(path_UD,"/Retrospective Data/",DAT_NAME,sep=""),quote=F,row.names=F,col.names=F)
yrs<-seq(styr,endyr)
Catch<-matrix(nrow=length(yrs),ncol=2)
colnames(Catch)<-c("Year","Catch")
Catch[,1]<-yrs
Catch[,2]<-obs_catch
write.csv(Catch,paste(path_UD,"/Retrospective Data/obs_catch.csv",sep=""))
write.csv(FAC,paste(path_UD,"/Retrospective Data/oac_fish.csv",sep=""))
write.csv(TSAC,paste(path_UD,"/Retrospective Data/oac_srv1.csv",sep=""))
write.csv(TSB,paste(path_UD,"/Retrospective Data/oac_srv1_biom.csv",sep=""))
write.csv(FSC,paste(path_UD,"/Retrospective Data/osc_fish.csv",sep=""))
write.csv(TSSC,paste(path_UD,"/Retrospective Data/osc_srv1.csv",sep=""))

