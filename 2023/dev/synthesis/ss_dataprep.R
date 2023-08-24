## SS dataprep
## Munging the data for input into parallel Synthesis model
## data was pulled using funciton in 2023_analysis.R; stored in data/raw
require(dplyr)
require(here)
require(reshape2)
require(ggplot2)

theme_set(theme_minimal())

## SECTION 1: RAW DATA DOWNLOAD AND PROCESSING FOR SS----
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

## age comps----
#* fishery agecomps ----
# bins 2-25
# it looks like the sqrt(nsamples) is what gets passed to the likelihood function
# these are in the 100s-1000s; nhauls is unused
fsh_age0 <- read.csv(here('2023','data','raw','fsh_specimen_data.csv'))

fsh_age1 <- fsh_age0 %>%
  filter(year %in% c(1990,1998:2002,2004:2006,seq(2008,2020,2))) %>%
  mutate(age = ifelse(age > 25,25,age)) %>% 
  group_by(year, age) %>%
  summarise(n =n())

 
fsh_age2 <- fsh_age1 %>% group_by(year) %>%
  summarise(nsamples_fish_age = sum(n)) 

fsh_age3 <- merge(fsh_age2, fsh_age1, by = 'year') %>%
  mutate(freq = n/nsamples_fish_age) %>%
  tidyr::pivot_wider(., id_cols = year, 
                     names_from = age, values_from = freq,
                     values_fill = 0) 

fsh_age_plot <- melt(fsh_age3, id = 'year')

ggplot(fsh_age_plot, aes(x = variable, y = value )) +
  geom_bar(stat = 'identity')+
  facet_wrap(~year, ncol = 2)

fsh_age3 %>% mutate(`2` = 0,
                    month = 7, fleet = 1, sex = 1, part = 0, ageerr = 1, Lbin_lo = -1,
                    Lbin_hi = -1, Nsamp = fsh_age2$nsamples_fish_age) %>%
  select(yr = year, month, fleet, sex, part, ageerr, Lbin_lo, Lbin_hi, Nsamp, 
         `2`,everything()) %>%
  write.csv(.,here('2023','data','for_ss',paste0(Sys.Date(),'-fishery_ages.csv')), row.names = FALSE)


#* survey agecomps----
srv_age0 <- read.csv(here('2023','data','raw','bts_specimen_data.csv'))
srv_age1 <- srv_age0 %>%
  filter(year %in% c(1990, 1993, 1996 ,1999 ,2003, 2005 ,2007, 2009 ,
                     2011 ,2013, 2015, 2017 ,2019)) %>%
  filter(!is.na(age)) %>%
  mutate(age = ifelse(age > 25,25,age)) %>% 
  mutate(age = ifelse(age<2,2,age)) %>% 
  group_by(year, age) %>%
  summarise(n =n())

srv_age2 <- srv_age1 %>% group_by(year) %>%
  summarise(nsamples_srv_age = sum(n)) 

srv_age3 <- merge(srv_age2, srv_age1, by = 'year') %>%
  mutate(freq = n/nsamples_srv_age) %>%
  tidyr::pivot_wider(., id_cols = year, 
                     names_from = age, values_from = freq,
                     values_fill = 0) 

srv_age_plot <- melt(srv_age3, id = 'year')

ggplot(srv_age_plot, aes(x = variable, y = value )) +
  geom_bar(stat = 'identity')+
  facet_wrap(~year, ncol = 2)

srv_ages<- srv_age3 %>% mutate(month = 7, fleet = 2, sex = 1, part = 0, ageerr = 1, Lbin_lo = -1,
                    Lbin_hi = -1, Nsamp = srv_age2$nsamples_srv_age) %>%
  select(yr = year, month, fleet, sex, part, ageerr, Lbin_lo, Lbin_hi, Nsamp, 
         everything()) 
  
  rbind(srv_ages,
        c(-9999, rep(0, ncol(srv_ages)-1))) %>%
  write.csv(.,here('2023','data','for_ss',paste0(Sys.Date(),'-goa_bts_ages.csv')), row.names = FALSE)


#* fishery length comps ----
## raw data before 1987 is not available here
## i struggled to get these too look write, going to read from file for now
fish_len_years<- c(1963, 1964, 1965, 1966, 1967, 1968 ,1969 ,1970, 1971,
                   1972, 1973 ,1974, 1975, 1976, 1977 ,1991, 1992, 1995, 1996 ,1997)

fsh_len2 <- scan(here('goa_pop','2023','base','goa_pop_2021.dat'), 
                 skip = 164, nlines = 1) %>%
  data.frame(nsamples_fsh_len=.) %>%
  mutate(year =fish_len_years) %>%
  select(year, nsamples_fsh_len)

fsh_len0 <- scan(here('goa_pop','2023','base','goa_pop_2021.dat'), 
     skip = 170, nlines = 20) %>% 
  matrix(ncol = length(16:45), byrow = TRUE) %>%
  data.frame()%>%
  mutate(yr = fish_len_years) %>%
  mutate(month = 7, fleet = 1, sex = 1, part = 0, 
         Nsamp = fsh_len2$nsamples_fsh_len) %>%
  select(yr, month, fleet, sex, part, Nsamp, 
         everything())


fsh_len_plot <- melt(fsh_len0, id = 'yr')

ggplot(fsh_len_plot, aes(x = variable, y = value )) +
  geom_bar(stat = 'identity')+
  facet_wrap(~yr, ncol = 2)


rbind(fsh_len0,
      c(-9999, rep(0, ncol(fsh_len0)-1))) %>%
  write.csv(.,here('goa_pop','2023','data','for_ss',paste0(Sys.Date(),'-fishery_lengths.csv')), row.names = FALSE)


# fsh_len0 <- read.csv(here('2023','data','raw','fsh_length_data.csv'))
# 
# fsh_len1 <- fsh_len0 %>%
#   filter(year %in%fish_len_years) %>%
#   # filter(!is.na(length )) %>%
#   mutate(length = ifelse(length  > 45,45,length )) %>% 
#   mutate(length = ifelse(length <16,16,length )) %>% 
#   group_by(year, length ) %>%
#   summarise(n =n())
# 
# ## get nsamples -- these are not intuitive for me, read from file
# # fsh_len2 <- fsh_len1 %>%
# #   group_by(year) %>%
# #   summarise(nsamples_fsh_len = sum(n))
# 
# 
# 
# fsh_len3 <- merge(fsh_len2, fsh_len1, by = 'year') %>%
#   mutate(freq = n/nsamples_fsh_len) %>%
#   tidyr::pivot_wider(., id_cols = year, 
#                      names_from = length, values_from = freq,
#                      values_fill = 0) 
# 
# fsh_lcomps_old <- scan(here('2023','base','goa_pop_2021.dat'), 
#                        skip = 170, nlines = 20) %>%
#   matrix(ncol = length(16:45)) %>%
#   data.frame()%>%
#   mutate(year =fish_len_years ) %>%
#   filter(year < min(fsh_len1$year)) %>%
#   select(year, everything())
# 
# names(fsh_lcomps_old) <- names(fsh_len3)
# 
# 
# fsh_len4<- bind_rows(fsh_lcomps_old, fsh_len3)
# 
# 
# fsh_len_plot <- melt(fsh_len3, id = 'year')
# 
# ggplot(fsh_len_plot, aes(x = variable, y = value )) +
#   geom_bar(stat = 'identity')+
#   facet_wrap(~year, ncol = 2)
# 
# fsh_len3 %>% mutate(month = 7, fleet = 2, sex = 1, part = 0, ageerr = 1, Lbin_lo = -1,
#                     Lbin_hi = -1, Nsamp = fsh_len2$nsamples_fsh_len) %>%
#   select(yr = year, month, fleet, sex, part, ageerr, Lbin_lo, Lbin_hi, Nsamp, 
#          everything()) %>%
#   write.csv(.,here('2023','data','for_ss',paste0(Sys.Date(),'-goa_bts_ages.csv')), row.names = FALSE)



#* unused: trawl survey (bts) length comps -----
## year month fleet sex = 1 part =0, nsamp, datavector
surv_l_bins <- read.csv(here(year,'data','raw','goa_total_bts_sizecomp_data.csv'))  %>%
mutate(length_cm = length_mm/10,
length_cm_use = ifelse(length_cm < min(lengths), min(lengths), 
ifelse(length_cm > max(lengths), max(lengths), length_cm )))

surv_l_nsamp <- surv_l_bins %>%
  group_by(year) %>%
  summarise(nsamp = n())

surv_lcomps <- surv_l_bins %>%
  group_by(year, length_cm_use) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()%>%
  mutate(month = 7, fleet = 2, sex = 1, part = 0) %>% 
  select(-n) %>%
  merge(., surv_l_nsamp, by = 'year') %>%
  tidyr::pivot_wider(., id_cols = c(year, month, fleet, sex, part, nsamp), 
  names_from = length_cm_use, values_from = freq, values_fill = 0) 

all(rowSums(surv_lcomps[,7:36]) ==1) 

write.csv(surv_lcomps,here(year,'data','for_ss',paste0(Sys.Date(),"-goa_bts_lengths.csv")), row.names = FALSE)

#* unused: ll survey length comps -----
## these are NOT USED IN MODEL; ghosted here

## SECTION 2: ADDITIONAL STUFF FOR SS MODEL----

#* extra DAT file stuff (copy paste) ----
cat(1961 ,'\t#_StartYr')
cat(2021 ,'\t#_EndYr')
cat(1 ,'\t#_Nseas')
cat(12 ,'\t#_months/season')
cat(2 ,'\t#_Nsubseasons (even number, minimum is 2)')
cat(5 ,'\t#_spawn_month')
cat(1 ,'\t#_Ngenders: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)')
cat(28 ,'\t#_Nages=accumulator age, first age is always age 0')
cat(1 ,'\t#_Nareas')
cat(2 ,'\t#_Nfleets (including surveys)')
cat(2 ,'\t#_Nsubseasons')


## length bins don't hae to match the observations
## because LMIN is less than 16, make this go from 1 to 45
cat(length(1:45),'\t#_N_LengthBins' ,  '\n', 1:45)
cat(length(16:45),'\t#_N_LengthBins' ,  '\n', 16:45)
cat(length(2:25),'\t#_N_age_bins' ,  '\n', 2:25) ## note this is not the same as the data

#* ageing error matrix ----
# the bespoke model has this hard coded. we instead need the mean bias and imprecision
# this is a placeholder
cat(rep(-1, length(0:28))) ## must match the poplenbins
cat(rep(0.001, length(0:28)))
nages = length(1:29)
ae_raw <- t(read.csv(here('2023','data','output','ae_sd.csv')))[2,1:nages] ## assume no bias but SDs
ae_ss <- rbind(rbind(rep(-1, length(1:nages)), round(ae_raw,4)),
 rbind(rep(-1, length(1:nages)), rep(0.001, length(1:nages))))
write.csv(ae_ss, here('2023','data','for_ss',paste0(Sys.Date(),'-age_err_vector.csv')),row.names=FALSE)
