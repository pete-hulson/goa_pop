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
library(dplyr)
library(here)
library(ggplot2)
theme_set(afscassess::theme_report())
# working on getting rema installed, but not working on my machine at the moment, don't have time to figure out
## This worked for MK in VSCode:
# options(buildtools.check = function(action) TRUE )
# devtools::install_github("afsc-assessments/rema") ## did not update other pckgs
# pak::pkg_install("afsc-assessments/rema")
library(rema)

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

# fishery catch (note: this automates the in-year estimation)
## output/yld_ratio.csv has the expansion factor ($ratio) and the 3-year catch/TAC ratio ($yld)
## which are used for in-year and next-two-year catches, respectively
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
                                 rmv_yrs = c(1988, 1993, 1994, 2003, 2007, 2009,
                                  2011, 2013, 2015, 2017, 2019, 2021, 2022, 2023))

# bottom trawl survey size comp (not fit to in model, used for size-age matrices)
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

## manually rename a file; lookup funs want "bts"
file.rename(from=here::here(year, 'data','output','goa_ts_length_comp.csv'),
to = here::here(year, 'data','output','goa_bts_length_comp.csv'))

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
## MK note: PH and I discussed this; fixing sigmaR after mod conditioning is pretty standard
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
# mk note: this take 2 mins on laptop
# mcmcruns <- 1e5
# mcmcsave <- mcmcruns / 50

# for full run
# mk note: 2.75 hours on laptop
mcmcruns <- 1e7
mcmcsave <- mcmcruns / 5000

R2admb::run_admb(mdl_name, verbose = TRUE, mcmc = TRUE, 
                 mcmc.opts = R2admb::mcmc.control(mcmc = mcmcruns,
                                                  mcsave = mcmcsave, 
                                                  mcmcpars = 'log_mean_rec'))

system(paste0(mdl_name,'.exe',' -mceval'))


# run retrospective, returns run time for testing ----

# for testing
#mcmcruns_ret <- 10000
#mcmcsave_ret <- mcmcruns / 5

# for full run
# mk note: 1.35 hours on laptop
 mcmcruns_ret = 500000  # Could change these, but 500,000 is a manageable number to deal with
 mcmcsave_ret = mcmcruns_ret / 250

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
# requires download of egoa fractions from AKFIN (format as CSV w/o header material; colnames unchanged)
afscassess::run_apport_pop(year = year,
                           model = curr_mdl_fldr)


# process results ----

# example to get model results without running mcmc
mdl_res <- afscassess::process_results_pop(year = year,
                                           model_dir = here::here(year, "mgmt",curr_mdl_fldr), 
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

## Run Profiles on Base model ----

## Natural Mortality
mvec = seq(0.01,0.3,0.02)

for(m in seq_along(mvec)){
new_dir <- here::here(year, "mgmt", curr_mdl_fldr, "profiles",paste0(Sys.Date(),'-Mprofile_M=',mvec[m]))
dir.create(new_dir)
file.copy(from = list.files(here::here(year,'mgmt',curr_mdl_fldr), full.names = TRUE), to = new_dir, overwrite = TRUE)

## populate the relevant lines in the CTL
ctl_file <- list.files(new_dir, pattern = "*.ctl", full.names = TRUE)
newctl <- read.delim(ctl_file, sep = ' ', header = FALSE)
newctl$V1[which(newctl$V3 == 'mprior')] <- mvec[m]
newctl$V1[which(newctl$V3 == 'cvmprior')] <- 0.1
newctl$V1[which(newctl$V3 == 'ph_m')] <- -1
newctl$V1 <- newctl$V1
write.table(newctl, ctl_file, quote = FALSE,row.names = FALSE, col.names = FALSE)

## run the new model
setwd(new_dir)
shell('admb Model_20_1.tpl')
shell('Model_20_1')
}

## process all the profile runs
mdirs <- list.files(here::here(year, "mgmt", curr_mdl_fldr, "profiles"), 
    pattern = 'profile_M=', full.names = T, recursive = FALSE)

lapply(mdirs, FUN = function(x) afscassess::process_results_pop(model_dir=x, year = year,
modname = mdl_name, rec_age = rec_age, plus_age = plus_age, size_bins = lengths, 
proj = FALSE, on_year = TRUE, mcmc = FALSE))

m_likes <- do.call(rbind, 
 lapply(list.files(mdirs, pattern = 'likelihoods.csv', recursive = TRUE, full.names = TRUE), 
 FUN = read.csv)) %>% 
 mutate(M_fixed =as.numeric(sub('.*=', '', model))) %>%  ## pull out fixed value of M
filter(weight !=0)

min_likes_var <- m_likes %>% 
filter(value != 0 ) %>%
group_by(variable) %>% 
summarise(min_value = min(value))

m_likes %>%
filter(weight != 0 & value != 0) %>%
merge(., min_likes_var, by = 'variable') %>% 
mutate(value_adj=ifelse(value == 0,value, value - min_value)) %>%
select(weight, variable, M_fixed,value,value_adj )-> m_likes

rich_cols <- r4ss::rich.colors.short(length(unique(m_likes$variable)), alpha = 1)

ggplot(m_likes, aes(x =M_fixed, y = value_adj, color = variable )) +
theme(legend.position = c(0.8,0.5))+
scale_color_manual(values = c(rich_cols[2:3],
'red',rich_cols[4:7],'black',
rich_cols[8:10],'goldenrod')) +
geom_line() +
scale_y_continuous(limits = c(0,10)) +
scale_x_continuous(limits = c(0,0.3), breaks = seq(0,0.3,0.05))+
labs(x = 'M', y = 'NLL (scaled)', color = 'Like. Component')

ggsave(file = here::here(year, "mgmt", curr_mdl_fldr, "profiles",paste0(Sys.Date(),"-Mprofile_ymax=15.png")),
width = 6, height =4 , dpi = 500)

# create figures ----



catch <- read.csv(here(year,'data','output','fsh_catch.csv')) %>% 
mutate(catch = catch/1000)
cplot1<-ggplot(catch, aes(x = year, y =catch)) +
  geom_line() + 
  labs(x = 'Year', y = 'Catch (t)')
cplot2<-ggplot(subset(catch, year > 1995), aes(x = year, y =catch)) +
  geom_line() + 
  scale_y_continuous(limits = c(0,35))+
  labs(x = 'Year', y = 'Catch (t)')


vp <- grid::viewport(width = 0.5, height = 0.5, x = 0.70, y = 0.65)
png(here::here(year,'mgmt',curr_mdl_fldr,'figs', 'catch_timeseries.png'),
    width = 6, height = 4, unit = 'in', res = 500)
print(cplot1)
print(cplot2, vp = vp)
dev.off()
 


## comp data fits ----
## to use these functions it will want to look into processed/ for the fac
#afscassess::correct_comps(year, model_dir = curr_mdl_fldr, modname = mdl_name,
#dat_name = dat_name,  rec_age = 2, plus_age = 25, len_bins = lengths)
afscassess::plot_comps(year, folder = paste0('mgmt/',curr_mdl_fldr),save = TRUE)
# afscassess::plot_catch(year, folder = paste0('mgmt/',curr_mdl_fldr),save = TRUE)
## params 
afscassess::plot_params(year, 
folder = paste0('mgmt/',curr_mdl_fldr),
model_name = mdl_name, save = TRUE)


afscassess::plot_retro(year, folder = paste0('mgmt/',curr_mdl_fldr), n_retro = 10 )
afscassess::plot_selex(year, folder = paste0('mgmt/',curr_mdl_fldr))
afscassess::plot_survey(year, folder = paste0('mgmt/',curr_mdl_fldr))
afscassess::plot_phase(year, folder = paste0('mgmt/',curr_mdl_fldr), model_name = mdl_name)
afscassess::plot_rec_ssb(year, folder = paste0('mgmt/',curr_mdl_fldr), rec_age=rec_age)

afscassess::base_plots(year, 
folder = paste0('mgmt/',curr_mdl_fldr),
model_name = mdl_name, rec_age = rec_age)


afscassess::plot_compare_biomass(year,
models = c('2020.1-2021','2020.1-2023'))

## comparison of survey fits
vroom::vroom(here::here(year, paste0("mgmt/",curr_mdl_fldr), "processed", "survey.csv")) %>%
    tidytable::rename_with.(tolower) %>%
    tidytable::select.(year = starts_with("y"),
                       Observed = starts_with("bio"),
                       Predicted = pred,
                       se, lci, uci) %>%
    tidytable::pivot_longer.(-c(year, se, uci, lci)) %>%
    tidytable::mutate.(value = value / 1000,
                       uci = uci / 1000,
                       lci = lci / 1000,
                       src = '2023 Assessment') %>%
    bind_rows(vroom::vroom(here::here(year, paste0("mgmt/",prev_mdl_fldr), "processed", "survey.csv")) %>%
    tidytable::rename_with.(tolower) %>%
    tidytable::select.(year = starts_with("y"),
                       Observed = starts_with("bio"),
                       Predicted = pred,
                       se, lci, uci) %>%
    tidytable::pivot_longer.(-c(year, se, uci, lci)) %>%
    tidytable::mutate.(value = value / 1000,
                       uci = uci / 1000,
                       lci = lci / 1000,
                       src = '2021 Assessment'))    -> dat

ggplot(data= NULL, aes(x = year, color = src)) +
geom_point(data=subset(dat, src == '2023 Assessment' &
 name == 'Observed'), aes(y = value), color = 'grey44') +
geom_errorbar(data=subset(dat, src == '2023 Assessment'), width = 0, 
aes( ymin = lci, ymax = uci), color ='grey44') +
geom_line(data=subset(dat, src == '2023 Assessment' & 
name == 'Predicted'),aes(y = value),lwd = 0.75)+
geom_line(data=subset(dat, src == '2021 Assessment' & 
name == 'Predicted'),aes(y = value),lwd = 0.75) +
scale_color_manual(values = c('grey22','blue')) +
labs(x = 'Year', y = 'Survey Biomass (t)', color = '') +
theme(legend.position = c(0.2,0.8))

ggsave(last_plot(),
height = 4, width = 6, unit = 'in',
file = here(here(year,'mgmt',curr_mdl_fldr,'figs','survey_fit_compare.png')))



### comparison of VAST and DB estimator
biomass_dat <- read.csv(here(year,'data','raw','goa_total_bts_biomass_data.csv')) %>% 
mutate(sd = sqrt(biomass_var) ) %>%
select(year, 
biomass = total_biomass,sd) %>% mutate(src = 'Design-based')
vast <- read.csv(here(year,'dev','mb_vs_db','table_for_ss3.csv')) %>% 
select(year = Year, biomass = Estimate_metric_tons, sd = SD_mt) %>%
mutate(src = 'VAST (model-based)') %>%
filter(biomass >0)

rbind(biomass_dat) %>%
mutate(lci = biomass-1.96*sd, uci = biomass+1.96*sd) %>%
ggplot(.,
 aes(x = year, y = biomass, fill = src, color = src)) +
geom_point(size = 3) +
#geom_ribbon(aes(ymin =lci,ymax = uci), alpha = 0.2) +
geom_errorbar(aes(ymin =lci,ymax = uci), width = 0) +
theme(legend.position = c(0.15,0.8)) +
scale_color_manual(values = c('blue','grey45')) +
scale_fill_manual(values = c('blue','grey45')) +
#scale_y_continuous(limits = c(0,2500), expand = c(0,0)) +
labs(x = 'Year', y = 'Biomass (1000 t)', fill = '', color = '')

ggsave(last_plot(),
height = 4, width = 6, unit = 'in',
file = here(here(year,'dev','mb_vs_db','mb_db_comparison.png')))


read.csv(here(year,'mgmt',curr_mdl_fldr,'processed','selex.csv')) %>%
  reshape2::melt(., id = 'age') %>%
  bind_rows(.,read.csv(here(year,'mgmt',curr_mdl_fldr,'processed','waa_mat.csv')) %>%
              dplyr::rename(., "waa"="srv1") %>%
  reshape2::melt(., id = 'age')) %>%
  filter(variable != 'waa') %>%
  ggplot(., aes(x = age, y = value, color = variable)) +
  geom_line(lwd = 1.1) +
  theme(legend.position = 'top') +
  scale_color_manual(values = c("#023047","#126782","#219ebc","#8ecae6","#fb8500","#ffb703"),
                     labels = c(paste0('Fishery ',c("1967-1976","1977-1995","1996-2006","2007-2023")),
                                'Survey','Maturity'))+
  labs(x = 'Age', y = 'Proportion', color = '')

ggsave(last_plot(), file =here::here(year,'mgmt', curr_mdl_fldr, "figs", "selex_mat.png"), 
       width = 6, height = 6, unit = 'in')


## key derived quantities with uncertainty and comparison
biolabs <- as_labeller(c(
  'tot_biom'="Total Biomass (kt, ages 2+)",
  'spawn_biom'="Spawning Biomass (kt)",
  'Frate'="Fishing Mortality",
  'age2_recruits'="Age-2 Recruits (thousands)")
)

mcmc_summary_raw <- read.csv(here(year,'mgmt',curr_mdl_fldr,'processed','mceval.csv')) 
## manually rename the last chunk because the processing function didn't expect rec or F
names(mcmc_summary_raw)[282:(281+length(1961:2023))] <- paste0('age2_recruits_',1961:2023)
names(mcmc_summary_raw)[345:407] <- paste0('Frate_',1961:2023)

mcmc_summary_raw %>%
reshape2::melt()%>%
filter(grepl("biom|rec|Frate",variable)) %>%
mutate(year = as.numeric(stringr::str_sub(variable,-4,-1)),
variable = stringr::str_sub(variable, 1, -6)) %>%
mutate(src = '2023 Model') %>%
bind_rows(., read.csv(here(year,'mgmt',"2020.1-2021",'processed','mceval.csv')) %>%
reshape2::melt() %>%
filter(grepl("biom|rec|Frate",variable)) %>%
mutate(year = as.numeric(stringr::str_sub(variable,-4,-1)),
variable = stringr::str_sub(variable, 1, -6)) %>%
mutate(src = '2021 Model')) %>%
mutate(value = ifelse(value >1000,value/1000,value)) %>%
group_by(variable, year, src) %>%
summarise(median = median(value),lower = quantile(value, probs = 0.025), upper = quantile(value, probs = 0.975)) ->
mcmc_summary

write.csv(subset(mcmc_summary, src == '2023 Model'), 
file = here(year,'mgmt',curr_mdl_fldr,'processed','mceval_summary.csv'), row.names = FALSE)             
summarize(median = median(value),
              lower = median(value) - qt(1- 0.05/2, (n() - 1))*sd(value)/sqrt(n()),
              upper = median(value) + qt(1- 0.05/2, (n() - 1))*sd(value)/sqrt(n())) 
mcmc_summary %>% filter(year == 2023 & variable == 'spawn_biom')

mcmc_summary %>%
filter(variable %in% c('spawn_biom', 'tot_biom', 'age2_recruits','Frate')) %>%
  ggplot(., aes(x = year,  color = src, fill = src)) +
  geom_line(aes(y=median))+
  geom_ribbon(aes(ymin =lower, ymax = upper),alpha = 0.2, color = NA)+
  theme(legend.position = 'top') +
  scale_fill_manual(values = c('grey44','blue'))+
  scale_color_manual(values = c('grey44','blue'))+
  facet_wrap(~variable,scales = 'free_y',labeller = biolabs) +
  labs(x = 'Year', y = '',color = '') 

ggsave(last_plot(), file =here::here(year,'mgmt', curr_mdl_fldr, "figs", 
"bio_f_rec_compare.png"), 
       width = 7, height =7, unit = 'in')

## recdevs plot (requires parameter_summary csv made below)

bind_rows(read.csv(here::here(year,'mgmt',model,'processed','parameter_summary_devs.csv')) %>%
  filter(grepl('rec',variable)) %>%
  mutate(src = '2023 Model'),
  read.csv(here::here(year,'base','processed','parameter_summary_devs.csv')) %>%
  filter(grepl('rec',variable)) %>%
    mutate(src = '2021 Model')) %>%
  ggplot2::ggplot(., aes(x = year, color = src)) +
  geom_hline(yintercept =0, linetype = 'dashed', color = 'grey88') +
  geom_point(aes(y= median)) +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0, alpha =0.5) +
  scale_color_manual(values = c(alpha('grey44',0.5),'blue')) +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(1975,2025), 
                     breaks = c(seq(1975,2020,5),2023), 
                     labels = c(seq(1975,2020,5),2023))+
  labs(x = 'Year', y = 'log Recruitment Deviation', color = '') +
  theme(legend.position = c(0.1,0.8))

ggsave(last_plot(), file =here::here(year,'mgmt', curr_mdl_fldr, "figs", 
                                     "recdevs.png"), 
       width = 6, height =4, unit = 'in')


### survey CPUE
library(akgfmaps)
## devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = FALSE)

SEBS <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto")

## format this based on  https://github.com/afsc-gap-products/akgfmaps/blob/master/R/make_idw_map.R
## from 2021 production modsquad google drive folder
raw_surv <- readRDS(here(year,'data', 'raw','Data_Geostat_Sebastes_alutus.rds')) 

s2021 <- raw_surv %>% 
  mutate( COMMON_NAME = 'Pacific Ocean Perch') %>%
  dplyr::select(Year,COMMON_NAME, CPUE_KGHA  = Catch_KG, LATITUDE = Lat, LONGITUDE = Lon) %>%
  dplyr::filter(Year == 2021) %>%
  make_idw_map(region = "goa",
                set.breaks = "jenks", ## auto
              #set.breaks = c(0, 100000, 250000, 500000, 750000), ## standardized breaks
               in.crs = "+proj=longlat",
              
               out.crs = "EPSG:3338", # Set output coordinate reference system
               use.survey.bathymetry = FALSE, ## for GOA
               grid.cell = c(20000, 20000)) %>% # 20x20km grid
  add_map_labels() %>% 
  change_fill_color(new.scheme = "green2", show.plot = TRUE) #%>%
  # create_map_file(file.prefix = NA, 
  #                           file.path = NA, 
  #                           try.change_text_size = TRUE, # 12x9 is a pre-defined size
  #                           width = 12, 
  #                           height = 9, 
  #                           units = "in", 
  #                           res = 300, 
  #                           bg = "transparent")

s2019 <- raw_surv%>% 
  mutate( COMMON_NAME = 'Pacific Ocean Perch') %>%
  dplyr::select(Year,COMMON_NAME, CPUE_KGHA  = Catch_KG, LATITUDE = Lat, LONGITUDE = Lon) %>%
  dplyr::filter(Year == 2019) %>%
  make_idw_map(region = "goa",
              set.breaks = "jenks", ## auto
              #set.breaks = c(0,100000, 250000, 500000, 750000), ## standardized breaks
               in.crs = "+proj=longlat",
               out.crs = "EPSG:3338", # Set output coordinate reference system
               use.survey.bathymetry = FALSE, ## for GOA
               grid.cell = c(20000, 20000)) %>% # 20x20km grid
  add_map_labels() %>% 
  change_fill_color(new.scheme = "green2", show.plot = TRUE) #%>%
  # create_map_file(file.prefix = NA,
  #                           file.path = NA,
  #                           try.change_text_size = TRUE, # 12x9 is a pre-defined size
  #                           width = 12,
  #                           height = 9,
  #                           units = "in",
  #                           res = 300,
  #                           bg = "transparent")


p1 <- s2019$plot+theme(legend.position = 'right') 
p2 <- s2021$plot+theme(legend.position = 'right') 

png(file = here(year,'base','figs','2_cpue_maps.png'),
width = 8, height = 10, unit = 'in', res = 520)
Rmisc::multiplot(plotlist = list(p1,p2), cols = 1)
dev.off()

# create tables ----
allpars <- read.csv(here::here(year, 'mgmt',model,'processed','mcmc.csv')) %>%
  reshape2::melt() %>%
  dplyr::group_by(variable) %>%
  summarise_at(vars(value),
               list(
                 Q1=~quantile(., probs = 0.25),
                 median=median, 
                 Q3=~quantile(., probs = 0.75))) 

other_pars <- filter(allpars, !grepl('dev',variable)) %>%
  mutate(Parameter = c('Avg. log Annual Recruitment',
                       'Age at 50% Selectivity, Timeblock 2',
                       'Delta Selectivity, Timeblock 2',
                       'Age at 50% Selectivity, Timeblock 3',
                       'Delta Selectivity, Timeblock 3',
                       'Age at 50% Selectivity, Timeblock 4',
                       'Delta Selectivity, Timeblock 4',
                       'Age at 50% Selectivity, Survey',
                       'Delta Selectivity, Survey',
                       'Avg. log fishing mortality',
                       'Age at 50% maturity',
                       'Delta Maturity',
                       'log catchability (survey)',
                       'log natural mortality',
                       'F50%',
                       'F40%',
                       'F35%'
                       )) %>%
  select(Parameter, everything())
f_rec_devs <- filter(allpars, grepl('dev',variable)) %>% 
  mutate(year = c(1961:2023,1935:2023)) %>%
  select(variable, year, Q1,median,Q3)

write.csv(f_rec_devs,here::here(year, 'mgmt', curr_mdl_fldr,'processed','parameter_summary_devs.csv'),row.names = FALSE)
write.csv(other_pars,here::here(year, 'mgmt', curr_mdl_fldr,'processed','parameter_summary.csv'),row.names = FALSE)
