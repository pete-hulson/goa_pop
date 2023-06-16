# notes ----
# code for 2023 goa pop data queries and such 
# pete.hulson@noaa.gov
# maia.kapur@noaa.gov
# ben.williams@noaa.gov

# load ----
# devtools::unload("afscassess")

devtools::unload("afscdata")
devtools::unload("afscassess")

devtools::install_github("afsc-assessments/afscdata", force = TRUE)
devtools::install_github("BenWilliams-NOAA/afscassess@ph_dev", force = TRUE)
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

# clean data ----
clean_catch(year = year, species = species, TAC = TAC)
bts_biomass(year = year, rmv_yrs = c(1984, 1987))

# !!! I'm having trouble running this function via R2admb so stepped out and ran it command line, works fine if I compile it command line and then use the R2admb run function, maybe I'll pass the .exe instead of rebuilding the .tpl each year?
age_error(year=year, reader_tester="reader_tester.csv", admb_home=admb_home, species=species, rec_age=rec_age, plus_age=plus_age)
size_at_age(year=year, admb_home=admb_home, lenbins=lengths)
weight_at_age(year=year, admb_home=admb_home, rec_age=rec_age, area = "goa")
fish_age_comp(year=year, rec_age=rec_age, plus_age=plus_age)
fish_length_comp(year=year, rec_age=rec_age, lenbins=lengths)
# bts_age_comp(year=year, area="goa", rec_age=rec_age, plus_age=plus_age, rmv_yrs=c(1984,1987))
# bts_length_comp(...)




# fishery age comp ----

# need to rename 'fsh_specimen_data.csv' as output from afscdata to 'fsh_age_comp_data.csv' as read in by afscassess
# also getting error could not find function "." when using summarise fcn - maybe has to do with error in pipe?
file.rename(from = here::here('2023', 'data', 'raw', 'fsh_specimen_data.csv'),
            to = here::here('2023', 'data', 'raw', 'fsh_age_comp_data.csv'))



afscassess::fish_age_comp(year = year, rec_age = rec_age, plus_age = plus_age)

fish_age_comp(year = year, rec_age = rec_age, plus_age = plus_age)

#' fishery age composition analysis
#'
#' @param year assessment year
#' @param fishery default is fsh1, change if age comps from multiple fisheries (e.g., fsh2)
#' @param rec_age recruitment age
#' @param plus_age plus age group
#' @param exp_type age comp expansion type: 'none' (just uses the raw ages), 
#' 'unwtd' (expands with length comps that are not catch weighted), 
#' 'wtd' (expands with length comps that are weighted by catch)
#' @param save whether to save the file
#'
#' @return
#' @export  fish_age_comp
#'
#' @examples
#' \dontrun{
#' fish_age_comp(year, fishery = "fsh1", rec_age, plus_age)
#' }
fish_age_comp <- function(year, fishery = "fsh", rec_age, plus_age, exp_type = 'none', save = TRUE){
  
  if(exp_type == 'none'){
    vroom::vroom(here::here(year, "data", "raw", paste0(fishery, "_age_comp_data.csv")),
                 col_types = list(HAUL_JOIN = "c",
                                  PORT_JOIN = "c")) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::filter(age>=rec_age) %>%
      dplyr::mutate(age = ifelse(age>plus_age, plus_age, age)) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(tot = dplyr::n()) %>%
      dplyr::filter(tot>49) %>%
      dplyr::mutate(n_h = length(unique(na.omit(haul_join))) +
                      length(unique(na.omit(port_join)))) %>%
      dplyr::group_by(year, age) %>%
      dplyr::summarise(n_s = mean(tot),
                       n_h = mean(n_h),
                       age_tot = dplyr::n()) %>%
      dplyr::mutate(prop = age_tot / n_s) %>%
      dplyr::left_join(expand.grid(year = unique(.$year),
                                   age = rec_age:plus_age), .) %>%
      tidyr::replace_na(list(prop = 0)) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(AA_Index = 1,
                    n_s = mean(n_s, na.rm = T),
                    n_h = mean(n_h, na.rm = T)) %>%
      dplyr::select(-age_tot) %>%
      tidyr::pivot_wider(names_from = age, values_from = prop) -> fac
    
    if(!(isTRUE(save)) | !(isFALSE(save)) | (!is.null(save))) {
      readr::write_csv(fac, here::here(year, alt, "data", paste0(fishery, "_age_comp.csv")))
      fac
    } else if(isTRUE(save)) {
      readr::write_csv(fac, here::here(year, "data", "output", paste0(fishery, "_age_comp.csv")))
      fac
    } else {
      fac
    }
  }
  
  
  
  
  
  
}


# from dusky assessment ----


# load ----
library(groundfishr)
devtools::unload("groundfishr")
library(gfdata)
library(funcr)
ggplot2::theme_set(theme_report())
library(scico)

# globals ----
year = 2022
species <- "DUSK"
area = "goa"
afsc_species1 =  30150
afsc_species2 = 30152
norpac_species = 330
TAC <- c(3700, 3676, 5389)
fishery = "fsh"


pwds <- readRDS(here::here('pwds.rds'))
afsc_user = pwds$afsc_user
afsc_pwd = pwds$afsc_pwd
akfin_user = pwds$akfin_user
akfin_pwd = pwds$akfin_pwd

# setup(year = year)

species = "DUSK"
area = "GOA"
afsc_species1 =  30150
afsc_species2 = 30152
norpac_species = 330

rec_age = 4
plus_age = 25

# data ----
# data query
goa_dusk(year, akfin_user, akfin_pwd, afsc_user, afsc_pwd)

groundfishr::clean_catch(year=year, species=species, TAC=TAC)
groundfishr::age_error(reader_tester = "reader_tester.csv", species, year, rec_age=rec_age, plus_age = plus_age)
groundfishr::fish_age_comp(year=year, rec_age=rec_age, plus_age=plus_age)
groundfishr::ts_age_comp(year=year, rec_age=rec_age, plus_age=plus_age, rmv_yrs = c(1984, 1987))
groundfishr::fish_length_comp(year=year, rec_age=rec_age, lenbins = 'lbins.csv')
groundfishr::ts_length_comp(year=year, lenbins = 'lbins.csv', bysex = FALSE,  rmv_yrs = c(1984, 1987))
groundfishr::size_at_age(year=year, rec_age=rec_age, lenbins="lbins.csv")
groundfishr::weight_at_age(year=year, rec_age=rec_age)

# note: must provide file for VAST or DB estimates are output

# Base model w/updated data & diff surveys
# design-based model
groundfishr::ts_biomass(year=year, rmv_yrs = c(1984, 1987))
# base model with design-based survey
groundfishr::concat_dat(year=year, species=species, model="db", dat_name='goa_dr_2022', spawn_mo=3, rec_age=rec_age, plus_age=plus_age)





