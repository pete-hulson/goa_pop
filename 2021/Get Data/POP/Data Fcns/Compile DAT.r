
#============== Compile DAT file for ADMB

get_DAT<-function(styr,endyr,recage,nages_D,nages_M,nlenbins,n_ageage_mat,n_sizeage_mat,lenbins,spawn_fract,wt,obs_catch,TSB,FAC,TSAC,FSC,TSSC,sizeage_sixties,sizeage_current,ageage){

##### Header

L_1<-paste("#- ",substr(Sep,2,nchar(Sep)),sep="")
L_2<-"# GOA Pacific ocean perch .dat file for ADMB optimization"
L_3<-paste("# New data provided on,",substr(Sys.time(),1,10),sep=" ")
L_4<-"# Notes:"
L_5<-"#   ~ Total catch prior to 1990 frozen"
L_6<-"#   ~ Total catch from 1991 on uses catch downloaded from AKFIN"
L_7<-"#   ~ Weight-at-age and current length-age transition matrix automatically updated"
L_8<-"#   ~ Formatted to conduct automated retrospective analysis"
L_9<-"#   ~ This data file provides ABL kluge for 2001 survey biomass for Eastern Gulf"
L_10<-"#   ~ Does not use most recent years fishery size data"
L_11<-"#   ~ Does not use fishery size data in years when ages are expected"
L_12<-Sep
L_13<-""
L_14<-""

Header<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14)

######### Model Input Parameters/Values

L_1<-Sep
L_2<-"# Model input parameters/vectors"
L_3<-Sep
L_4<-"# Start and end years, recruitment age, number of age and length bins"
L_5<-"# Model start year (styr):"
L_6<-as.character(styr)
L_7<-"# Model end year (endyr): #!"
L_8<-as.character(endyr)
L_9<-"# Age at recruitment (recage): #-"
L_10<-as.character(recage)
L_11<-"# Number of ages in data (nages_D):"
L_12<-as.character(nages_D)
L_13<-"# Number of ages in model (nages_M):"
L_14<-as.character(nages_M)
L_15<-"# Number of length bins (nlenbins):"
L_16<-as.character(nlenbins)
L_17<-"# Number of age-age transition matrices (n_ageage_mat):"
L_18<-as.character(n_ageage_mat)
L_19<-"# Number of size-age transition matrices (n_sizeage_mat):"
L_20<-as.character(n_sizeage_mat)
L_21<-"# Length bin labels (len_bin_labels):"
L_22<-paste(lenbins,collapse=" ")
L_23<-"# Spawn month (spawn_fract):"
L_24<-as.character(spawn_fract)
L_25<-"# Weight-at-age (wt):"
L_26<-paste(as.vector(wt),collapse=" ")
L_27<-""
L_28<-""

MIPV<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18,L_19,L_20,L_21,L_22,L_23,L_24,L_25,L_26,L_27,L_28)

######### Fishery Catch

fy<-as.character(paste(seq(styr,endyr),collapse=" "))
L_1<-Sep
L_2<-"# Fishery catch (mt): obs_catch(styr,endyr)"
L_3<-Sep
L_4<-paste("#!",fy,sep="",collapse=" ")
L_5<-paste(obs_catch,collapse=" ")
L_6<-"#-"
L_7<-""
L_8<-""

FC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8)

######### CPUE -- Not fit in POP model

L_1<-Sep
L_2<-"# CPUE Data"
L_3<-Sep
L_4<-"# Number of CPUE years"
L_5<-"0"
L_6<-"# CPUE observations (leave blank if 0)"
L_7<-""
L_8<-""

CPUE<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8)

######### Trawl Survey Biomass

L_1<-Sep
L_2<-"# Trawl Survey Biomass"
L_3<-Sep
L_4<-"#! Number of trawl surveys: nyrs_srv1"
L_5<-as.character(length(TSB[,1]))
L_6<-"#- Trawl survey years: yrs_srv1(1,nyrs_srv1) #!"
L_7<-paste(as.vector(TSB[,1]),collapse=" ")
L_8<-"#- Observed trawl survey biomass (mt): obs_srv1_biom(1,nyrs_srv1) #!"
L_9<-paste(as.vector(TSB[,2]),collapse=" ")
L_10<-"#- SE of observed trawl survey biomass: obs_srv1_se(1,nyrs_srv1) #!"
L_11<-paste(as.vector(TSB[,3]),collapse=" ")
L_12<-"#- Lower CI, 1.96*SE #!"
L_13<-paste(as.vector(TSB[,4]),collapse=" ")
L_14<-"#- Upper CI, 1.96*SE #!"
L_15<-paste(as.vector(TSB[,5]),collapse=" ")
L_16<-"#-"
L_17<-""
L_18<-""

TSB<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18)

######### Longline Survey Biomass -- Not fit in POP model

L_1<-Sep
L_2<-"# Longline Survey Biomass"
L_3<-Sep
L_4<-"# Number of longline surveys: nyrs_srv2"
L_5<-"1"
L_6<-"# Longline survey years: yrs_srv2(1,nyrs_srv2)"
L_7<-"1999"
L_8<-"# Observed longline survey biomass (mt): obs_srv2_biom(1,nyrs_srv2)"
L_9<-"1000"
L_10<-"# SE of observed longline survey biomass: obs_srv2_se(1,nyrs_srv2)"
L_11<-"100"
L_12<-"# Lower CI, 1.96*SE"
L_13<-"10"
L_14<-"# Upper CI, 1.96*SE"
L_15<-"10000"
L_16<-""
L_17<-""

LSB<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17)

######### Fishery Age Composition

L_1<-Sep
L_2<-"# Fishery Age Composition"
L_3<-Sep
L_4<-"#! Number of years: nyrs_fish_age"
L_5<-as.character(length(FAC[,1]))
L_6<-"#- Fishery age comp years: yrs_fish_age #!"
L_7<-paste(as.vector(FAC[,1]),collapse=" ")
L_8<-"#- Number of samples: nsamples_fish_age(1,nyrs_fish_age) #!"
L_9<-paste(as.vector(FAC[,2]),collapse=" ")
L_10<-"#- Number of hauls: nhauls_fish_age(1,nyrs_fish_age) #!"
L_11<-paste(as.vector(FAC[,3]),collapse=" ")
L_12<-"#- Index for age-age error matrix #!"
L_13<-paste(as.vector(FAC[,4]),collapse=" ")
L_14<-"#- Observed fishery age compositions (proportions at age): oac_fish(1,nyrs_fish_age,1,nages) #!"
L_15<-paste(as.vector(FAC[1,5:length(FAC[1,])]),collapse=" ")
for(y in 2:length(FAC[,1])){
L_add<-paste(as.vector(FAC[y,5:length(FAC[1,])]),collapse=" ")
L_15<-c(L_15,L_add)}
L_16<-"#-"
L_17<-""
L_18<-""

FAC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18)

######### Trawl Survey Age Composition

L_1<-Sep
L_2<-"# Trawl Survey Age Composition"
L_3<-Sep
L_4<-"#! Number of years: nyrs_srv1_age"
L_5<-as.character(length(TSAC[,1]))
L_6<-"#- Trawl Survey age comp years: yrs_srv1_age #!"
L_7<-paste(as.vector(TSAC[,1]),collapse=" ")
L_8<-"#- Number of samples: nsamples_srv1_age(1,nyrs_srv1_age) #!"
L_9<-paste(as.vector(TSAC[,2]),collapse=" ")
L_10<-"#- Number of hauls: nhauls_srv1_age(1,nyrs_srv1_age) #!"
L_11<-paste(as.vector(TSAC[,3]),collapse=" ")
L_12<-"#- Index for age-age error matrix #!"
L_13<-paste(as.vector(TSAC[,4]),collapse=" ")
L_14<-"#- Observed trawl survey age compositions (proportions at age): oac_srv1(1,nyrs_srv1_age,1,nages) #!"
L_15<-paste(as.vector(TSAC[1,5:length(TSAC[1,])]),collapse=" ")
for(y in 2:length(TSAC[,1])){
L_add<-paste(as.vector(TSAC[y,5:length(TSAC[1,])]),collapse=" ")
L_15<-c(L_15,L_add)}
L_16<-"#-"
L_17<-""
L_18<-""

TSAC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18)

######### Fishery Size Composition

L_1<-Sep
L_2<-"# Fishery Size Composition"
L_3<-Sep
L_4<-"#! Number of years: nyrs_fish_size"
L_5<-as.character(length(FSC[,1]))
L_6<-"#- Fishery size comp years: yrs_fish_size #!"
L_7<-paste(as.vector(FSC[,1]),collapse=" ")
L_8<-"#- Number of samples: nsamples_fish_size(1,nyrs_fish_size) #!"
L_9<-paste(as.vector(FSC[,2]),collapse=" ")
L_10<-"#- Number of hauls: nhauls_fish_size(1,nyrs_fish_size) #!"
L_11<-paste(as.vector(FSC[,3]),collapse=" ")
L_12<-"#- Index for size-age error matrix #!"
L_13<-paste(as.vector(FSC[,4]),collapse=" ")
L_14<-"#- Observed fishery size compositions (proportions at length): osc_fish(1,nyrs_fish_size,1,nlenbins) #!"
L_15<-paste(as.vector(FSC[1,5:length(FSC[1,])]),collapse=" ")
for(y in 2:length(FSC[,1])){
L_add<-paste(as.vector(FSC[y,5:length(FSC[1,])]),collapse=" ")
L_15<-c(L_15,L_add)}
L_16<-"#-"
L_17<-""
L_18<-""

FSC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18)

######### Trawl Survey Size Composition -- Not fit in POP model

L_1<-Sep
L_2<-"# Trawl Survey Size Composition, NOT USED IN MODEL"
L_3<-Sep
L_4<-"#! Number of years: nyrs_srv1_size"
L_5<-as.character(length(TSSC[,1]))
L_6<-"#- Trawl Survey size comp years: yrs_srv1_size #!"
L_7<-paste(as.vector(TSSC[,1]),collapse=" ")
L_8<-"#- Number of samples: nsamples_srv1_size(1,nyrs_srv1_size) #!"
L_9<-paste(as.vector(TSSC[,2]),collapse=" ")
L_10<-"#- Number of hauls: nhauls_srv1_size(1,nyrs_srv1_size) #!"
L_11<-paste(as.vector(TSSC[,3]),collapse=" ")
L_12<-"#- Index for size-age error matrix #!"
L_13<-paste(as.vector(TSSC[,4]),collapse=" ")
L_14<-"#- Observed trawl survey size compositions (proportions at length): osc_srv1(1,nyrs_srv1_size,1,nlenbins) #!"
L_15<-paste(as.vector(TSSC[1,5:length(TSSC[1,])]),collapse=" ")
for(y in 2:length(TSSC[,1])){
L_add<-paste(as.vector(TSSC[y,5:length(TSSC[1,])]),collapse=" ")
L_15<-c(L_15,L_add)}
L_16<-"#-"
L_17<-""
L_18<-""

TSSC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18)

######### Longline Survey Size Composition -- Not fit in POP model

L_1<-Sep
L_2<-"# Longline Survey Size Composition, NOT USED IN MODEL, include one year of fake data"
L_3<-Sep
L_4<-"# Number of years: nyrs_srv2_size"
L_5<-"1"
L_6<-"# Longline Survey size comp years: yrs_srv1_size"
L_7<-"1999"
L_8<-"# Number of samples: nsamples_srv2_size(1,nyrs_srv2_size)"
L_9<-"99"
L_10<-"# Number of hauls: nhauls_srv2_size(1,nyrs_srv2_size)"
L_11<-"99"
L_12<-"# Index for size-age error matrix"
L_13<-"1"
L_14<-"# Observed longline survey size compositions (proportions at length): osc_srv2(1,nyrs_srv2_size,1,nlenbins)"
L_15<-paste(seq(1/nlenbins,1/nlenbins,length.out=nlenbins),collapse=" ")
L_16<-""
L_17<-""

LSSC<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17)

######### Size-age transition matrix

L_1<-Sep
L_2<-"# Size-age transition matrix: proportion at size given age: sizeage(1,nages,1,nlenbins)"
L_3<-Sep
L_4<-"# Size-age matrix representing slower growth in the 1960s by 6% in the 60's and 70's: sizeage1(1,nages,1,nlenbins)"
L_5<-paste(as.vector(sizeage_sixties[1,]),collapse=" ")
for(y in 2:length(sizeage_sixties[,1])){
L_add<-paste(as.vector(sizeage_sixties[y,]),collapse=" ")
L_5<-c(L_5,L_add)}
L_6<-""
L_7<-"# Size-age matrix using mean estimation for 1980s-2000s: sizeage2(1,nages,1,nlenbins)"
L_8<-paste(as.vector(sizeage_current[1,]),collapse=" ")
for(y in 2:length(sizeage_current[,1])){
L_add<-paste(as.vector(sizeage_current[y,]),collapse=" ")
L_8<-c(L_8,L_add)}
L_9<-""
L_10<-""

SA<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10)

######### Aging error matrix

L_1<-Sep
L_2<-"# Ageing error matrix: proportion at reader age given true age: ageage(1,nages,1,nages)"
L_3<-Sep
L_4<-paste(as.vector(ageage[1,]),collapse=" ")
for(y in 2:length(ageage[,1])){
L_add<-paste(as.vector(ageage[y,]),collapse=" ")
L_4<-c(L_4,L_add)}
L_5<-""
L_6<-""

AA<-c(L_1,L_2,L_3,L_4,L_5,L_6)

######### End of File Marker

L_1<-Sep
L_2<-"# EOF marker"
L_3<-Sep
L_4<-"42"
L_5<-"#!"

EOF<-c(L_1,L_2,L_3,L_4,L_5)

######### Concatenate DAT file

DAT<-c(Header,MIPV,FC,CPUE,TSB,LSB,FAC,TSAC,FSC,TSSC,LSSC,SA,AA,EOF)

DAT}


