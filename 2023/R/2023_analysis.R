# notes ----
# code for 2023 goa pop data queries and such 
# pete.hulson@noaa.gov
# maia.kapur@noaa.gov
# ben.williams@noaa.gov

# load ----

# devtools::unload("afscdata")
# devtools::unload("afscassess")

devtools::install_github("afsc-assessments/afscdata", force = TRUE)
devtools::install_github("BenWilliams-NOAA/afscassess@devph", force = TRUE)
# devtools::install_github("BenWilliams-NOAA/afscassess", force = TRUE)
library(afscdata)
library(afscassess)

# working on getting rema installed, but not working on my machine at the moment, don't have time to figure out
# pak::pkg_install("afsc-assessments/rema")
# library(rema)

# previous accepted model
# this is more for an example since the previous assessment was not in the afscdata framework
# accepted_model(base_year=2021, base_model="model_20_1", year=2023)

# globals ----
year = 2023
rec_age = 2
plus_age = 25
lengths = 16:45
TAC = c(31238, 36177, 38268) # previous 3 years
species = "POP"
admb_home = "C:/ADMB-13.0" # I use this because I have multiple admb versions
curr_mdl_fldr = "2020.1-2023"
prev_mdl_fldr = "2020.1-2021"
mdl_name = "model_20_1"
dat_name = "goa_pop"

afscassess::sp_switch(species)
# setup folder structure - only run this once
# afscdata::setup_folders(year)
# setup .tpl files
# afscassess::setup_tpl(year)

# query data ----
## you must be on the VPN for this to work
afscdata::goa_pop(year)

# get data files together (dat and ctl) ----

# weight-at-age
# note from ben on these admb called functions: !!! I'm having trouble running this function via R2admb so stepped out and ran it command line, works fine if I compile it command line and then use the R2admb run function, maybe I'll pass the .exe instead of rebuilding the .tpl each year?
afscassess::weight_at_age(year = year,
                          admb_home = admb_home,
                          rec_age = rec_age,
                          area = "goa")

# fishery catch
suppressWarnings(afscassess::clean_catch(year = year, 
                                        species = species, 
                                        TAC = TAC))

# bottom trawl survey biomass
afscassess::bts_biomass(year = year, 
                        rmv_yrs = c(1984, 1987))

# fishery age comp
# base case (currently used)
afscassess::fish_age_comp(year = year,
                          exp_meth = 'marg_len',
                          rec_age = rec_age, 
                          plus_age = plus_age,
                          lenbins = lengths,
                          rmv_yrs = c(1987, 1989))
# expanded comps (expanded in years with obs catch data)
# afscassess::fish_age_comp(year = year,
#                           exp_meth = 'exp_len',
#                           rec_age = rec_age, 
#                           plus_age = plus_age,
#                           lenbins = lengths,
#                           rmv_yrs = c(1987, 1989),
#                           id = "exp")

# bottom trawl survey age comp
afscassess::bts_age_comp(year = year,
                         area = "goa",
                         rec_age = rec_age,
                         plus_age = plus_age,
                         rmv_yrs = c(1984,1987))

# fishery size comp
afscassess::fish_length_comp_pop(year = year,
                                 rec_age = rec_age,
                                 lenbins = lengths,
                                 rmv_yrs = c(1988, 1993, 1994, 2003, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2022, 2023))

# bottom trawl survey size comp
afscassess::bts_length_comp(year = year,
                            area = "goa",
                            sa_index = 2,
                            lenbins = lengths)

# 60s size-age matrix
afscassess::size_at_age_pop_60(year = year,
                               rec_age = rec_age,
                               lenbins = lengths)

# current size-age matrix
afscassess::size_at_age(year = year,
                        admb_home = admb_home,
                        rec_age = rec_age,
                        lenbins = lengths)

# ageing error matrix
afscassess::age_error(year = year, 
                      reader_tester = "reader_tester.csv", 
                      admb_home = admb_home, 
                      species = species, 
                      rec_age = rec_age, 
                      plus_age = plus_age)

# concatenate dat file, for now writing it to output folder in data
afscassess::concat_dat_pop(year = year,
                           species = species,
                           area = "goa",
                           folder = "data/output",
                           dat_name = dat_name,
                           rec_age = rec_age,
                           plus_age = plus_age,
                           spawn_mo = 5)

# write ctl file, for now writing it to output folder in data
afscassess::write_ctl_pop(year = year,
                          base_mdl_fldr = '2020.1-2021',
                          mdl_name = "Model_1",
                          ctl_name = dat_name,
                          dat_name = dat_name,
                          folder = "data/output")


# run base model ----

# write dat and ctl files to base model folder
afscassess::concat_dat_pop(year = year,
                           species = species,
                           area = "goa",
                           folder = "mgmt/2020.1-2023",
                           dat_name = dat_name,
                           rec_age = rec_age,
                           plus_age = plus_age,
                           spawn_mo = 5)

afscassess::write_ctl_pop(year = year,
                          base_mdl_fldr = '2020.1-2021',
                          mdl_name = "Model_1",
                          ctl_name = dat_name,
                          dat_name = dat_name,
                          folder = "mgmt/2020.1-2023")

# run base model
setwd(here::here(year, "mgmt", curr_mdl_fldr))

R2admb::run_admb(mdl_name, verbose = TRUE)


# run mcmc within it's own sub-folder ----

# create mcmc folder and copy over model files

if (!dir.exists(here::here(year, "mgmt", curr_mdl_fldr, "mcmc"))){
  dir.create(here::here(year, "mgmt", curr_mdl_fldr, "mcmc"), recursive=TRUE)
}

file.copy(c(paste0(mdl_name, ".tpl"),
            paste0(mdl_name, ".exe"),
            paste0(dat_name, "_", year, ".dat"),
            paste0(dat_name, "_", year, ".ctl"),
            "mat.dat"),
          here::here(year, "mgmt", curr_mdl_fldr, "mcmc"),
          overwrite = TRUE)

# set sigr to mle in ctl file (Maia - you will want to look into this at some point)

afscassess::write_ctl_pop(year = year,
                          base_mdl_fldr = prev_mdl_fldr,
                          curr_mdl_fldr = curr_mdl_fldr,
                          mdl_name = "Model_1",
                          ctl_name = dat_name,
                          dat_name = dat_name,
                          folder = paste0("mgmt/", curr_mdl_fldr),
                          mcmc = TRUE)

# run mcmc

setwd(here::here(year, "mgmt", curr_mdl_fldr, "mcmc"))

# for testing
mcmcruns <- 100000
mcmcsave <- mcmcruns / 50

# for full run
# mcmcruns <- 10000000
# mcmcsave <- mcmcruns / 5000

R2admb::run_admb(mdl_name, verbose = TRUE, mcmc = TRUE, 
                 mcmc.opts = R2admb::mcmc.control(mcmc = mcmcruns,
                                                  mcsave = mcmcsave, 
                                                  mcmcpars = 'log_mean_rec'))

system(paste0(mdl_name,'.exe',' -mceval'))


# run retrospective, returns run time for testing ----

# for testing
mcmcruns_ret <- 10000
mcmcsave_ret <- mcmcruns / 5

# for full run
# mcmcruns_ret = 500000  # Could change these, but 500,000 is a manageable number to deal with
# mcmcsave_ret = mcmcruns / 250

# run retro (note: there's warnings that pop up for closing unused connection, can disregard)
suppressWarnings(afscassess::run_retro_pop(year = year, 
                                           model = curr_mdl_fldr, 
                                           model_name = mdl_name, 
                                           dat_name = dat_name, 
                                           n_retro = 10, 
                                           mcmcon = TRUE, 
                                           mcmc =  mcmcruns_ret, 
                                           mcsave = mcmcsave_ret))
 
 
# run projections ----

# note that there's a warning that pops up with this version of the proj model, so suppressed it
suppressWarnings(afscassess::run_proj(st_year = year,
                                      spec = dat_name,
                                      model = curr_mdl_fldr,
                                      on_year = TRUE))


# run apportionment ----

# rema stuff to be put in here



# process results ----

# example to get model results without running mcmc
mdl_res <- afscassess::process_results_pop(year = year,
                                           model_dir = here::here(year, "mgmt", curr_mdl_fldr), 
                                           modname = mdl_name,
                                           rec_age = rec_age,
                                           plus_age = plus_age, 
                                           size_bins = lengths,
                                           mcmc = FALSE,
                                           proj = FALSE,
                                           on_year = TRUE)

# example to get model results with mcmc, retrospective, and projection model runs
mdl_res <- afscassess::process_results_pop(year = year,
                                           model_dir = here::here(year, "mgmt", curr_mdl_fldr), 
                                           modname = mdl_name,
                                           dat_name = dat_name,
                                           rec_age = rec_age,
                                           plus_age = plus_age, 
                                           size_bins = lengths,
                                           mcmc = TRUE,
                                           no_mcmc = mcmcruns,
                                           mcsave = mcmcsave,
                                           proj = TRUE,
                                           on_year = TRUE,
                                           retro = TRUE,
                                           retro_mcmc = TRUE,
                                           no_mcmc_ret = mcmcruns_ret,
                                           mcsave_ret = mcmcsave_ret)


# create figures ----

# spot for figure fcns


# create tables ----

#spot for table fcns

