## Code to demonstrate how to make a "Piner" plot using GOA
## pollock ADMB model
## https://www.sciencedirect.com/science/article/pii/S0165783613003111


library(tidyverse)
theme_set(theme_bw())
library(GOApollock) # has function to read in custom report file

getwd() # directory of model to profile

## component names and types for plotting and grouping
xx <- c("total catch", "fsh age comps", "fsh len comps", "surv1 index",
                     "surv1 age comps", "surv1 len comps", "surv2 index",
                     "surv2 age comps", "surv2 len comps", "unused", "surv3 index",
                     "surv3 age comps", "surv3 len comps", "surv4 index",
                     "surv5 index", "surv6 index", "surv6 age comps",
                     "recruit penalties", "TV selex penalties", "proj recruits",
                     "TV Q penalties", "unused", "prior trawl Q",
                     "unused")
ctype <- case_when(grepl('fsh',xx)~'fishery',
                   grepl('surv1|surv4|surv5',xx)~'Shelikof',
                   grepl('surv2',xx)~'NMFS BT',
                   grepl('surv3', xx)~'ADF&G',
                   grepl('prior',xx)~'Q NMFS BT',
                   grepl('penal',xx)~'Recdevs',
                   grepl('surv6',xx)~'Summer',
                   TRUE~'other')
csurv <- case_when(grepl('age',xx)~'ages',
          grepl('len', xx)~'lengths',
          grepl('index',xx)~'indices',
          grepl('prior|penal',xx)~'priors',
          TRUE~'other')
components <- data.frame(component=xx, ctype=ctype, csurv=csurv)


### ------------------------------------------------------------
### Profile over catchability for survey 2 (logq2). Need to set
### the right phase negative and recompile. Do this manually.
logq2seq <- seq(-.5,.25, len=15)
reps <- list();k <- 1
for(logq2 in logq2seq){
  q2 <- exp(logq2)
  init <- readLines('goa_pk.par') ## the full MLE values
  ## Replace initial value with the current value. Must match
  ## your parameter name
  init[grep('log_q2_mean:', init)+1] <- logq2
  writeLines(init, con='init.pin')
  ## run model w/ this initial value, since negative phase it
  ## won't move
  system('goa_pk -ind pk20_8.txt -ainp init.pin -nox -iprint 500 -nohess')
  ## read in report file as a list, must contain likelihood components
  reps[[k]] <- c(q2=q2, read_pk_rep('goa_pk.rep', version=q2))
  k <- k+1
}
clean_pk_dir()
saveRDS(list(reps=reps), file='q2_results.RDS')

reps <- readRDS('q2_results.RDS')$reps
## quick check it works
ssb <- lapply(reps, function(x) mymelt(x, 'Expected_spawning_biomass')) %>%
  bind_rows %>% mutate(q2=model)
g <- ggplot(ssb, aes(year, value, group=factor(q2), color=q2)) + geom_line()+ labs(y='SSB')
ggsave('prof_q2_ssb.png', g, width=7, height=5)

### Make Piner plot
## strip out the likelihood componets (vector) from each run and cbind the
## informative names on
loglikes <- lapply(reps, function(x) cbind(components, mymelt(x, 'Likelihood_components'))) %>%
  bind_rows %>% rename(q2=model)
## ## tack on total NLL
## total <- loglikes %>% group_by(q2) %>%
##   summarize(component='0_total', value=sum(value), .groups='drop')
## ## now normalize within each component
total <- NULL
loglikes <- bind_rows(total, loglikes) %>% group_by(q2) %>%
  group_by(component) %>% mutate(delta_NLL=-value-min(-value)) %>%
  ## some of these change a tiny bit so drop for visual clarity
  filter(max(delta_NLL)>.5 & q2>.55)
g <- ggplot(filter(loglikes),
            aes(x=q2, y=delta_NLL, color=ctype)) +
  geom_line() + facet_wrap('csurv')
g
ggsave('prof_q2_NLL.png', g, width=7, height=5)
