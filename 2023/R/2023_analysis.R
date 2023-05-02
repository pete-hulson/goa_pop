# notes ----
# code for 2023 goa pop data queries and such 
# pete.hulson@noaa.gov
# maia.kapur@noaa.gov
# ben.williams@noaa.gov

# load ----
# devtools::unload("afscassess")
library(afscdata)
library(afscassess)

# previous accepted model
# this is more for an example since the previous assessment was not in the afscdata framework
# accepted_model(base_year=2021, base_model="model_20_1", year=2023)

# globals ----
year = 2023
TAC = c(31238, 36177, 38268) # previous 3 years
species = "POP"

# setup folder structure - only run this once
# setup_folders(year)

# query data ----
goa_pop(year)

# clean data ----
clean_catch(year=year, species=species, TAC=TAC)
