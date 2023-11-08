## re-create 2021 profiles as sanity check

require(here)
require(dplyr)
require(ggplot2)
require(afscassess);theme_set(afscassess::theme_report())
require(plotly)

## munge all files into logical, can be used for 2d plots
list.files(here('2021_cie','profiles'), pattern = '_like', full.names = TRUE) %>%
lapply(., FUN = function(x){ tmp <- read.csv(x) %>% mutate(src = basename(x))
return(tmp)}) %>%
bind_rows() %>%
reshape2::melt(id = c('X','src')) %>%
filter(!is.na(value)) %>%
mutate(q = as.numeric(substr(variable,3,5)),
M = as.numeric(gsub('M=','',X)),
src = gsub('_like.csv','',src)) %>%

ungroup() %>%
select(M,q,src,value,value_adj) -> likes

likes_m <- likes %>%
group_by(src, q) %>%
mutate(value_adj = value-min(value)) 

likes_q <- likes %>%
group_by(src, M) %>%
mutate(value_adj = value-min(value))


## filter to MLE q


ggplot(data = subset(likes_m, q == 1.8), aes(x = M, y = value_adj, color = src)) +
geom_line(lwd = 1) +
scale_y_continuous(limits = c(0,5)) +
geom_hline(yintercept = qchisq(0.95,1)/2, linetype = 'dashed', color = 'grey77') +
labs(color = 'Component', y = 'Delta NLL')

## filter to MLE M
ggplot(data = subset(likes_q, M== 0.07), aes(x = q, y = value_adj, color = src)) +
geom_line(lwd = 1) +
scale_y_continuous(limits = c(0,5)) +
geom_hline(yintercept = qchisq(0.95,1)/2, linetype = 'dashed', color = 'grey77') +
labs(color = 'Component', y = 'Delta NLL')




## reshape for 3d plots (x is q, y is m, z is value)
qm_df <- likes %>%  
filter(value_adj < 10) %>%
filter(src == 'Total') %>%
select(-value, -src) %>%
 tidyr::pivot_wider(names_from = M, 
                     values_from = value_adj)  


qm_df.plotmeX <- sort(as.numeric(qm_df$q))
qm_df.plotmeY <- sort(as.numeric(colnames(qm_df)[2:ncol(qm_df)]))
qm_df.plotmeZ <- as.matrix(qm_df %>% select(-q))

axx <- list(
  title = "Survey  Q"#,
   #range = c(log(5),0)
)

axy <- list(
  title = "Natural Mortality (both sexes)"
  # range = c(0.1,0.3)
)

axz <- list(
  title = "Change in log-likelihood"
)

which(qm_df.plotmeZ == min(qm_df.plotmeZ), arr.ind = TRUE)
exp(qm_df.plotmeX[2]);qm_df.plotmeY[9] ## q = 1.49, m = 0.09
## manually save from rstudio console

plot_ly(x = ~qm_df.plotmeY,
        y = ~qm_df.plotmeX, 
        z = ~qm_df.plotmeZ, 
        type = "surface",
        contours = 
          list(z = list(show = TRUE, color = 'white', start = 0, 
                         end = qchisq(0.95,1)/2, size =1.92))) %>%
  add_surface() %>%
  #layout(xaxis = axx) 
  layout( scene = list(xaxis = axy,
                      yaxis = axx,
                       zaxis = axz)) %>%
  hide_colorbar()
