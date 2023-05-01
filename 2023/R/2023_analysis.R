# notes ----
# code for 2023 goa pop data queries and such 
# pete.hulson@noaa.gov
# ben.williams@noaa.gov

# load ----
library(afscdata)

# globals 
year = 2023

# setup folder structure
# setup_folders(year)

# query data 
goa_pop(year, off_yr=FALSE)


# globals ----
species = "POPA"
area = "GOA"
afsc_species = 30060
norpac_species = 301

akfin = connect()
q_catch(year=year, species=species, area=area, db=akfin)
q_fish_obs(year=year, species=norpac_species, area=area, db=akfin)
q_bts_biomass(year=year, area=area, species=afsc_species, by='total', db=akfin) 
