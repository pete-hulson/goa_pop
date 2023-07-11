#' @param year  assessment year
#' @param model_dir  full path of model being evaluated  
#' @param MCMC = logical, does this run include MCMC evaluations to be processed?
#' @param no_mcmc = number of mcmc runs
#' @param rec_age recruitment age
#' @param plus_age plus age group 
#' @param mcsave the number of mcmcs saved 
#' @param ... future functions

process_results_pop <- function(year=2023, model_dir, 
                            rec_age=2, plus_age=25, MCMC=FALSE, no_mcmc = 100000, mcsave=100, ...){

  # setup

  if (!dir.exists(paste0(model_dir, "/processed"))){
    dir.create(paste0(model_dir, "/processed"), recursive=TRUE)
  }

  if (!dir.exists(paste0(model_dir, "/figs"))){
    dir.create(paste0(model_dir, "/figs"), recursive=TRUE)
  }
  if (!dir.exists(paste0(model_dir, "/tables"))){
    dir.create(paste0(model_dir, "/tables"), recursive=TRUE)
  }


  # helper functions
  rep_item <- function(name){
    t <- strsplit(REP[grep(name, REP)]," ")
    t <- subset(t[[1]], t[[1]]!="")
    if(t[[1]][1] == "TWL"){
      as.numeric(t[3:length(t)])
    } else {
      as.numeric(t[2:length(t)])
    }
  }


  # read in report and ctl files
dats <- list.files(model_dir, full.names = TRUE)
DAT <- readLines(dats[grepl("*.dat",dats) & !grepl('proj',dats)])
REP <- readLines(list.files(model_dir, pattern="*.rep", full.names = TRUE)) 
modname <- gsub(".rep","",basename(list.files(model_dir, pattern="*.rep", full.names = TRUE))) ## strip model name from rep file
CTL <- readLines(list.files(model_dir, pattern="*.ctl", full.names = TRUE)) 
STD <- read.delim(list.files(model_dir, pattern="*.std", full.names = TRUE), sep="", header = TRUE) 


  # clean rep file
  suppressWarnings(data.frame(year = unlist(base::strsplit(REP[grep("Year", REP)[1]]," "))) %>%
                     tidytable::mutate.(year = as.numeric(year)) %>%
                     tidytable::drop_na.() %>%
                     tidytable::pull.(year)) -> yrs

  suppressWarnings(data.frame(age = unlist(base::strsplit(REP[grep("Age", REP)[1]]," "))) %>%
                     tidytable::mutate.(age = as.numeric(age)) %>%
                     tidytable::drop_na.() %>%
                     tidytable::pull.(age)) -> ages

  styr_rec <- yrs[1] - length(ages) + rec_age

  suppressWarnings(as.data.frame(cbind(yrs = yrs, ages = ages, styr_rec = styr_rec)) %>%
                     tidytable::mutate.(ages = replace(ages, duplicated(ages), NA),
                                        styr_rec = replace(styr_rec, duplicated(styr_rec), NA))) %>%
    write.csv(paste0(model_dir, "/processed/ages_yrs.csv"), row.names = FALSE)

## pull out likelihoods ----
## this function extracts & binds them rowwise
do.call(rbind, 
lapply(unlist(base::strsplit(REP[grep('Likelihood|Priors|Penalty|Objective', REP)][2:19],"\n")), 
FUN = function(x){
  tmpl <- unlist(strsplit(x," "))
  tempr <- matrix(c(tmpl[1], tmpl[2], paste0(tmpl[3:length(tmpl)], collapse = ' ')), ncol = 3)
  return(tempr)
  })) %>% 
  data.frame() %>%
  mutate(value = as.numeric(X2)) %>%
  select(weight = X1, value , variable = X3) %>% 
  mutate(model = basename(model_dir),
    weight = ifelse(weight == '', 1, weight)) %>%
  write.csv(., paste0(model_dir, "/processed/likelihoods.csv"), row.names = FALSE)


  # MCMC parameters ----
if(MCMC){
    mceval <- read.delim(list.files(model_dir, pattern="*evalout.prj", full.names = TRUE), sep="", header = FALSE) 
    PSV <- file(paste0(model_dir,"/",modname,".psv"), "rb")

 npar = readBin(PSV, what = integer(), n=1)
  mcmcs = readBin(PSV, what = numeric(), n = (npar * no_mcmc / mcsave))
  close(PSV)
  mcmc_params = matrix(mcmcs, byrow=TRUE, ncol=npar)
  # thin the string
  mcmc_params = mcmc_params[501:nrow(mcmc_params),]
  colnames(mcmc_params) = STD$name[1:ncol(mcmc_params)]
  write.csv(mcmc_params,paste0(model_dir, "/processed/mcmc.csv"), row.names = FALSE)

  # mceval phase output ----

  #Curry's Change
  mceval = mceval[501:nrow(mceval),]

  #Length colnames = 286
  # columns mcmc_other = 271

  #1-8: Through objective function value

  colnames(mceval) = c("sigr", "q_srv1", "q_srv2", "F40", "natmort", "spawn_biom_proj",
                       "ABC", "obj_fun",
                       paste0("tot_biom_", yrs),
                       paste0("log_rec_dev_", seq(styr_rec, yrs[length(yrs)])),
                       paste0("spawn_biom_", yrs),
                       "log_mean_rec",
                       paste0("spawn_biom_proj_", max(yrs) + 1:15),
                       paste0("pred_catch_proj_", max(yrs) + 1:15),
                       paste0("rec_proj_", max(yrs) + 1:10),
                       paste0("tot_biom_proj_", max(yrs)))
  write.csv(mceval, paste0(model_dir, "/processed/mceval.csv"), row.names = FALSE)

} ## end if MCMC == T
 
  # catch data ----
  pred = base::strsplit(REP[grep("Pred_Catch", REP)], " ")
  r1 = which(pred[[1]] == "Pred_Catch")
  r2 = which(pred[[1]] == "Pred_catch_later")
  r3 = which(pred[[1]] == "")
  pred = as.numeric(pred[[1]][-c(r1, r2, r3)])

  obs = base::strsplit(REP[grep("Obs_Catch", REP)], " ")
  r1 = which(obs[[1]] == "Obs_Catch")
  r2 = which(obs[[1]] == "Obs_Catch_Later")
  r3 = which(obs[[1]] == "")
  obs = as.numeric(obs[[1]][-c(r1, r2, r3)])

  data.frame(year = yrs, obs = obs, pred = pred) %>%
    write.csv(paste0(model_dir, "/processed/catch.csv"), row.names = FALSE)

  # survey data ----
  syr = REP[grep("Survey Biomass",REP)[1]:(grep("Survey Biomass",REP)[2]-2)][2]
  syr = base::strsplit(syr," ")
  syr = subset(syr[[1]], syr[[1]]!="")
  syr = as.numeric(syr[2:length(syr)])

  obs = REP[grep("Survey Biomass",REP)[1]:(grep("Survey Biomass",REP)[2]-2)][4]
  obs = base::strsplit(obs," ")
  obs = subset(obs[[1]], obs[[1]]!="")
  obs = as.numeric(obs[2:length(obs)])

  se = REP[grep("Survey Biomass",REP)[1]:(grep("Survey Biomass",REP)[2]-2)][5]
  se = base::strsplit(se," ")
  se = subset(se[[1]], se[[1]]!="")
  se = as.numeric(se[2:length(se)])

  pred = REP[grep("Survey Biomass",REP)[1]:(grep("Survey Biomass",REP)[2]-2)][3]
  pred = base::strsplit(pred," ")
  pred = subset(pred[[1]], pred[[1]]!="")
  pred = as.numeric(pred[2:length(pred)])


  data.frame(year = syr, biomass = obs, pred = pred, se = se) %>%
    tidytable::mutate.(lci = biomass - 1.96 * se,
                       uci = biomass + 1.96 * se) %>%
    write.csv(paste0(model_dir, "/processed/survey.csv"), row.names = FALSE)


  # recruitment ----
  N = REP[grep("Numbers", REP):(grep("Obs_P_fish_age", REP)-2)]
  t = NA
  for(i in 1:length(yrs)){
    ts = as.numeric(base::strsplit(N[i+1]," ")[[1]][3])
    t = c(t, ts)
  }
  pred_rec = t[!is.na(t)]

  # biomass & F & recruits ----
  data.frame(year = yrs,
             tot_biom = afscassess::rep_item("Tot_biom"),
             sp_biom = afscassess::rep_item("SpBiom"),
             F = afscassess::rep_item("Fully_selected_F"),
             recruits = pred_rec) %>%
    write.csv(paste0(model_dir, "/processed/bio_rec_f.csv"), row.names = FALSE)

  # selectivity ----
  data.frame(age = ages,
             fish = afscassess::rep_item("Fishery_Selectivity_2007-2021"),
             srv1 = afscassess::rep_item("Trawl_Survey_Selectivity"),
             maturity = afscassess::rep_item("Maturity")) %>%
    write.csv(paste0(model_dir, "/processed/selex.csv"), row.names = FALSE)

  # yield ratio B40 & B35----

  data.frame(B40 = STD$value[which(STD$name=="B40")],
             B35 = as.numeric(REP[(grep("B_35",REP)+1):(grep("F_40",REP)[1]-1)]),
             yld_rat = as.numeric(unlist(base::strsplit(CTL[grep("yieldratio", CTL)], " "))[1])) %>%
    write.csv(paste0(model_dir, "/processed/b35_b40_yld.csv"), row.names = FALSE)

  # size comps ----
  size_bins <- as.numeric(strsplit(DAT[grep('len_bin_labels', DAT)+1]," ")[[1]])

  cat('processed results for', basename(model_dir),'\n')

  #! this will need a switch for multiple surveys

#   obs = REP[grep("Obs_P_fish_age",REP):(grep("Pred_P_fish_age",REP)-2)]
#   pred = REP[grep("Pred_P_fish_age",REP):(grep("Obs_P_fish_size",REP)-2)]

#   obs_l = REP[grep("Obs_P_fish_size",REP):(grep("Pred_P_fish_size",REP)-2)]
#   pred_l = REP[grep("Pred_P_fish_size",REP):(grep("Obs_P_srv1_age",REP)-2)]

#   s_obs = REP[grep("Obs_P_srv1_age",REP):(grep("Pred_P_srv1_age",REP)-2)]
#   s_pred = REP[grep("Pred_P_srv1_age",REP):(grep("Obs_P_srv1_size",REP)-2)]

#   s_obs_l = REP[grep("Obs_P_srv1_size",REP):(grep("Pred_P_srv1_size",REP)-2)]

#   afscassess::purrit(obs, pred, rec_age, plus_age, comp = "age", lenbins = size_bins) %>%
#     write.csv(paste0(model_dir, "/processed/fac.csv"))

#   afscassess::purrit(obs_l, pred_l, rec_age, plus_age, comp = "length", lenbins = size_bins) %>%
#     write.csv(paste0(model_dir, "/processed/fsc.csv"))

#   afscassess::purrit(s_obs, s_pred, rec_age, plus_age, comp = "age", lenbins = size_bins) %>%
#     write.csv(paste0(model_dir, "/processed/sac.csv"))

#   afscassess::purrit(s_obs_l, pred = NULL, rec_age, plus_age, comp = "length", lenbins = size_bins) %>%
#     write.csv(paste0(model_dir, "/processed/ssc.csv"))

}
