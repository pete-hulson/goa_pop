# notes ----
# code for 2023 goa pop data queries and such 
# pete.hulson@noaa.gov
# maia.kapur@noaa.gov
# ben.williams@noaa.gov

# load ----
devtools::unload("afscdata")
library(afscdata)

# globals 
year = 2023

# setup folder structure
# setup_folders(year)

# query data 
goa_pop(year)

