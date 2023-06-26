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











# stuff that still needs work ---
# fishery age composition
fish_age_comp(year = year, 
              rec_age = rec_age, 
              plus_age = plus_age)















# bottom trawl survey size comp



#' fishery age composition analysis
#'
#' @param year assessment year
#' @param fishery default is fsh, change if age comps from multiple fisheries (e.g., fsh1, fsh2)
#' @param exp_meth expansion method: marg - use marginal ages, marg_len - expand by marginal lengths, exp_len - expand by expanded lengths
#' @param rec_age recruitment age
#' @param plus_age plus age group
#' @param rmv_yrs any years to remove form the age comp e.g. c(1987, 1989)
#' @param alt alternate folder to save to - will be placed in "year/alt/data" folder
#' @param save whether to save the file - wll be placed in "year/data/output" folder
#'
#' @return
#' @export  fish_age_comp
#'
#' @examples
#' \dontrun{
#' fish_age_comp(year, fishery = "fsh", rec_age, plus_age)
#' }
fish_age_comp <- function(year, fishery = "fsh", exp_meth, rec_age, plus_age, rmv_yrs = NULL, alt=NULL, save = TRUE){
  
  # compute age comps with marginal ages
  if(exp_meth == 'marg'){
    vroom::vroom(here::here(year, "data", "raw", paste0(fishery, "_specimen_data.csv"))) %>%
      tidytable::filter(age>=rec_age, !(year %in% rmv_yrs), !is.na(length), !is.na(performance)) %>%
      tidytable::mutate(age = ifelse(age>plus_age, plus_age, age)) %>%
      tidytable::mutate(tot = tidytable::n(), .by = year) %>%
      tidytable::filter(tot>49) %>%
      tidytable::mutate(n_h = length(unique(na.omit(haul_join))) +
                          length(unique(na.omit(port_join))),
                        .by = year) %>%
      tidytable::summarise(n_s = mean(tot),
                           n_h = mean(n_h),
                           age_tot = tidytable::n(),
                           .by = c(year, age)) %>%
      tidytable::mutate(prop = age_tot / n_s) %>%
      tidytable::left_join(expand.grid(year = unique(.$year),
                                       age = rec_age:plus_age), .) %>%
      tidytable::replace_na(list(prop = 0)) %>%
      tidytable::mutate(AA_Index = 1,
                        n_s = mean(n_s, na.rm = T),
                        n_h = mean(n_h, na.rm = T),
                        .by = year) %>%
      tidytable::select(-age_tot) %>%
      tidytable::pivot_wider(names_from = age, values_from = prop) -> fac
  }
  
  # expand age comps with marginal lengths
  if(exp_meth == 'marg_len'){
    vroom::vroom(here::here(year, "data", "raw", paste0(fishery, "_specimen_data.csv"))) %>%
      tidytable::filter(age>=rec_age, !(year %in% rmv_yrs), !is.na(length), !is.na(performance)) %>%
      tidytable::mutate(age = ifelse(age>plus_age, plus_age, age)) %>%
      tidytable::mutate(tot = tidytable::n(), .by = year) %>%
      tidytable::filter(tot>49) %>%
      tidytable::mutate(n_h = length(unique(na.omit(haul_join))) +
                          length(unique(na.omit(port_join))),
                        .by = year) %>%
      tidytable::summarise(n_s = mean(tot),
                           n_h = mean(n_h),
                           age_tot = tidytable::n(),
                           .by = c(year, age)) %>%
      tidytable::mutate(prop = age_tot / n_s) %>%
      tidytable::left_join(expand.grid(year = unique(.$year),
                                       age = rec_age:plus_age), .) %>%
      tidytable::replace_na(list(prop = 0)) %>%
      tidytable::mutate(AA_Index = 1,
                        n_s = mean(n_s, na.rm = T),
                        n_h = mean(n_h, na.rm = T),
                        .by = year) %>%
      tidytable::select(-age_tot) %>%
      tidytable::pivot_wider(names_from = age, values_from = prop) -> fac
  }
  
  
  
  
  
  
  
  if(!is.null(alt)) {
    vroom::vroom_write(fac, here::here(year, alt, "data", paste0(fishery, "_age_comp.csv")), ",")
    fac
  } else if(isTRUE(save)) {
    vroom::vroom_write(fac, here::here(year, "data", "output", paste0(fishery, "_age_comp.csv")), ",")
    fac
  } else {
    fac
  }
  
}

