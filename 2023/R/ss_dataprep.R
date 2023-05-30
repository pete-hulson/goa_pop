## SS dataprep
## Munging the data for input into parallel Synthesis model
## data was pulled using funciton in 2023_analysis.R; stored in data/raw
require(dplyr)
require(here)

## catches ----
## these begin in 1991
catches_new <- read.csv(here('2023','data','raw','fsh_catch_data.csv'))
catches_old <- scan(here('2023','base','goa_pop_2021.dat'), skip = 46, nlines = 1)

catches_91 <- catches_new %>%  
  group_by(year) %>%
  summarise(catch = round(sum(weight_posted ))) %>% 
  select(Yr = year, catch) %>%
  arrange(Yr)

cbind(catch_old = catches_old, Yr = 1961:2021) %>%
  merge(., catches_91, by = 'Yr', all = T) %>%
  mutate(catch == catch_old) ## double check that overlapping years match

cbind(catch_old = catches_old, Yr = 1961:2021) %>%
  merge(., catches_91, by = 'Yr', all = T) %>%
  mutate(seas = 1, catch_use = ifelse(Yr <1991, catch_old, catch), fleet = 1, cv = 0.01) %>% 
  select(Yr, seas, fleet, catch = catch_use, cv) %>%
  write.csv(.,here('2023','data','for_ss',paste0(Sys.Date(),'-catches.csv')), row.names = FALSE)


## survey obs ----
## there is a singleLL survey observation in 1999 in an observed mt of 1000 (?) and SE 100
trawl <- read.csv(here('2023','data','raw','goa_total_bts_biomass_data.csv'))
## back out CV given lwr and upr are 1.96 * se
trawl0 <- trawl %>%  
  mutate(fleet = 2, seas = 7,
         cpue = round(total_biomass),
         se = (total_biomass-min_biomass)/1.96,
         cv = round(se/total_biomass,2)) %>%
  select(Yr = year, seas , fleet, cpue, cv) %>%
  arrange(Yr) %>%
  filter(Yr >= 1990) 

rnorm(1, mean = mean(trawl0$cpue), sd = 370950.77 ) ## placeholder 2023 value

write.csv(trawl0,here('2023','data','for_ss',paste0(Sys.Date(),'-cpue.csv')), row.names = FALSE)

