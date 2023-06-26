# notes ----
# code for 2023 goa pop data queries and such 
# pete.hulson@noaa.gov
# maia.kapur@noaa.gov
# ben.williams@noaa.gov

# load ----

devtools::unload("afscdata")
devtools::unload("afscassess")

devtools::install_github("afsc-assessments/afscdata", force = TRUE)
devtools::install_github("BenWilliams-NOAA/afscassess@devph", force = TRUE)
# devtools::install_github("BenWilliams-NOAA/afscassess", force = TRUE)

library(afscdata)
library(afscassess)

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


afscassess::sp_switch(species)
# setup folder structure - only run this once
# afscdata::setup_folders(year)
# setup .tpl files
# afscassess::setup_tpl(year)

# query data ----
## you must be on the VPN for this to work
afscdata::goa_pop(year)

# get data file parts ----

# weight-at-age
# note from ben on these admb called functions: !!! I'm having trouble running this function via R2admb so stepped out and ran it command line, works fine if I compile it command line and then use the R2admb run function, maybe I'll pass the .exe instead of rebuilding the .tpl each year?
waa <- afscassess::weight_at_age(year=year, admb_home=admb_home, rec_age=rec_age, area = "goa")

# fishery catch
catch <- afscassess::clean_catch(year = year, 
                                 species = species, 
                                 TAC = TAC)

# bottom trawl survey biomass
bts_biom <- afscassess::bts_biomass(year = year, 
                                    rmv_yrs = c(1984, 1987))

# fishery age comp
fish_ac <- afscassess::fish_age_comp(year = year,
                                     exp_meth = 'marg_len',
                                     rec_age = rec_age, 
                                     plus_age = plus_age,
                                     lenbins = lengths,
                                     rmv_yrs = c(1987, 1989))

# bottom trawl survey age comp
bts_ac <- afscassess::bts_age_comp(year = year,
                                   area = "goa",
                                   rec_age = rec_age,
                                   plus_age = plus_age,
                                   rmv_yrs = c(1984,1987))

# fishery size comp
fish_lc <- afscassess::fish_length_comp_pop(year = year,
                                            rec_age = rec_age,
                                            lenbins = lengths,
                                            rmv_yrs = c(1988, 1993, 1994, 2003, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2022, 2023))

# bottom trawl survey size comp
bts_lc <- afscassess::bts_length_comp(year = year,
                                      area = "goa",
                                      lenbins = lengths)

# 60s size-age matrix
sz_age_60 <- afscassess::size_at_age_pop_60(year = year,
                                            rec_age = rec_age,
                                            lenbins = lengths)

# current size-age matrix
sz_age <- afscassess::size_at_age(year = year,
                                  admb_home = admb_home,
                                  rec_age = rec_age,
                                  lenbins = lengths)

# ageing error matrix
ae_mtx <- afscassess::age_error(year = year, 
                                reader_tester = "reader_tester.csv", 
                                admb_home = admb_home, 
                                species = species, 
                                rec_age = rec_age, 
                                plus_age = plus_age)








