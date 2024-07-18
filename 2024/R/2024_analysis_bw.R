# 2024 goa pacific ocean perch assessment code
# ben.williams@noaa.gov
# remotes::install_github('afsc-assessments/afscdata')
# remotes::install_github('BenWilliams-NOAA/afscassess')
library(afscdata)
library(afscassess)
library(ggplot2)
library(dplyr)
theme_set(theme_report())

# retrieve base model from 2023 full assessment
accepted_model(2023, 'mgmt/2020.1-2023', 2024)

# globals ----
year <- 2024
rec_age <- 2
plus_age <- 25
lengths <- 16:45
TAC = c(36177, 38268, 36196) # previous 3 years
area = 'goa'
species = "POP"
mdl_name = "model_20_1"
folder="update" # where to put projection

# query data ----
goa_pop(year, off_yr = TRUE)
clean_catch(year, species=species, TAC=TAC)
bts_biomass(year=year, area=area, rmv_yrs=c(1984,1987)) # design-based

# run projection
proj_ak(year=year, last_full_assess = 2023, species=species, region=area,
        rec_age=rec_age, folder=folder, off_yr=TRUE)

# plot catch/biomass
# note - ran the base model in order to get the .std file
std <- read.delim(here::here(year, "base", paste0(mdl_name, ".std")), sep="", header = TRUE)
catch <- vroom::vroom(here::here(year, "data", "output", "fsh_catch.csv"))

std %>%
  filter(name=="tot_biom") %>%
  bind_cols(filter(catch, year < max(year))) %>%
  filter(year >= 1991) %>%
  dplyr::select(year, catch, value, std.dev) %>%
  bind_rows(filter(catch, year == max(year)) %>%
              left_join(vroom::vroom(here::here(year, folder, "proj", "author_f", "bigsum.csv")) %>%
                          filter(Year == year, Alt == 2) %>%
                          mutate(value = Total_Biom * 1000) %>%
                          dplyr::select(year=Year, value))) %>%
  mutate(std.dev = ifelse(is.na(std.dev), std.dev[year==max(year)-1], std.dev)) %>%
  mutate(lci = value - std.dev * 1.96,
         uci = value + std.dev * 1.96) %>%
  mutate(perc = catch / value,
         lci = catch / lci,
         uci = catch / uci,
         mean = mean(perc)) %>%
  dplyr::select(year, value, mean, perc, lci, uci) -> df

png(filename=here::here(year, "figs", "catch_bio.png"), width = 6.5, height = 6.5,
    units = "in", type ="cairo", res = 200)
df %>%
  ggplot2::ggplot(ggplot2::aes(year, perc)) +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lci, ymax = uci), alpha = 0.2) +
  ggplot2::geom_hline(yintercept = df$mean, lty = 3) +
  ggplot2::expand_limits(y = c(0, 0.08)) +
  afscassess::scale_x_tickr(data=df, var=year, start = 1990) +
  afscassess::theme_report() +
  ggplot2::xlab("\nYear") +
  ggplot2::ylab("Catch/Biomass\n")

dev.off()

# plot survey results ----
sb <- vroom::vroom(here::here(year, "data", "output",  "goa_total_bts_biomass.csv"))


png(filename=here::here(year, "figs", "bts_biomass.png"), width = 6.5, height = 6.5,
    units = "in", type ="cairo", res = 200)

sb %>%
  mutate(mean = mean(biomass, na.rm=T)) %>% 
  ggplot2::ggplot(ggplot2::aes(year, biomass)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lci, ymax = uci), alpha = 0.2, color = NA) +
  afscassess::scale_x_tickr(data=sb, var=year) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::ylab("Survey biomass (t)\n") +
  ggplot2::xlab("\nYear") +
  ggplot2::expand_limits(y = 0) +
  ggplot2::geom_line(ggplot2::aes(y = mean), lty = 3) +
  afscassess::theme_report() 

dev.off()

# setup for next year
setup_folders(year+1)
accepted_model(2023,'mgmt/2020.1-2023', year+1)
