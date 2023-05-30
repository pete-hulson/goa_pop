## SS dataprep
## Munging the data for input into parallel Synthesis model
## data was pulled using funciton in 2023_analysis.R; stored in data/raw
require(dplyr)
require(here)

## survey obs ----
## there is a singleLL survey observation in 1999 in an observed mt of 1000 (?) and SE 100
trawl <- read.csv(here('2023','data','raw','goa_total_bts_biomass_data.csv'))
## back out CV given lwr and upr are 1.96 * se
trawl %>%  
  mutate(fleet = 2, 
         cpue = round(total_biomass),
         se = (total_biomass-min_biomass)/1.96,
         cv = round(se/total_biomass,2)) %>%
  select(Yr = year, fleet, cpue, cv) %>%
  arrange(Yr) %>%
  filter(Yr >= 1990) %>%
  write.csv(.,here('2023','data','for_ss',paste0(Sys.Date(),'-cpue.csv')))

