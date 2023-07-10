## update this so it is passed a vector of DIRS
## establish model names from basename
## enable comparison across discrepant file locations
plot_compare_biomass_pop <- function(year, model_dirs = NULL)) {

  if (!dir.exists(here::here("compare_models"))){
    dir.create(here::here( "compare_models"))
  }

  dat = data.frame()
  m = list(rep(NA, length(models)))
  for(i in 1:length(model_dirs)) {
  mod_name = basename(model_dirs[i])



    # m[[i]] = scan(text = models[i], sep = ",", what = "")
    # m[[i]][2] = gsub(" ", "", m[[i]][2])
    # # m[[i]][3] = gsub(" ", "", m[[i]][3])

    # year = m[[i]][1]
    # # folder = m[[i]][2]
    # model = m[[i]][2]
    # id = model
    # if(model=="db"){
    #   id = "design-based"
    # } else if(model=="m15.5a"){
    #   id = "A"
    # } else if(model=="pois_gamma_750"){
    #   id = "B"
    # } else if(model=="log_1000"){
    #   id = "C"
    # } else if(model=="log_750"){
    #   id = "D"
    # } else if(model=="pois_log_500"){
    #   id = "E"
    # } else {
    #   id = "F"
    # }

    yrs = read.csv(paste0(model_dir[i],"/processed/ages_yrs.csv"))$yrs
    bio = read.csv(paste0(model_dir[i],"/processed/bio_rec_f.csv"))

    dat %>%
      dplyr::bind_rows(
        read.csv(paste0(model_dir[i],"/processed/mceval.csv"))  %>%
          dplyr::select(paste0("tot_biom_", yrs)) %>%
          dplyr::mutate(group = 1:dplyr::n()) %>%
          tidyr::pivot_longer(-group) %>%
          dplyr::mutate(year = as.numeric(gsub("tot_biom_", "", name)),
                        name = "Total biomass") %>%
          dplyr::bind_rows( read.csv(paste0(model_dir[i],"/processed/mceval.csv")) %>%
                              dplyr::select(paste0("spawn_biom_", yrs)) %>%
                              dplyr::mutate(group = 1) %>%
                              tidyr::pivot_longer(-group) %>%
                              dplyr::mutate(year = as.numeric(gsub("spawn_biom_", "", name)),
                                            name = "Spawning biomass")) %>%
          dplyr::mutate(name = factor(name, levels = c("Total biomass", "Spawning biomass"))) %>%
          dplyr::group_by(year, name) %>%
          dplyr::summarise(median = median(value) / 1000,
                           lci = quantile(value, 0.025) / 1000,
                           uci = quantile(value, 0.975) / 1000) %>%
          dplyr::ungroup() %>%
          dplyr::left_join(data.frame(year = yrs,
                                      tot = bio$tot_biom / 1000,
                                      bio = bio$sp_biom / 1000)) %>%
          dplyr::mutate(biomass = ifelse(name == "Total biomass", tot, bio),
                        model = id) %>%
          dplyr::select(-tot, -bio)) -> dat
  }

  dummy = data.frame(year = rep(unique(dat$year),4),
                     name = rep(c("Total biomass", "Spawning biomass"), each = 2 * length(unique(dat$year))),
                     biomass = c(rep(0, length(unique(dat$year))), rep(160, length(unique(dat$year))),
                                 rep(0, length(unique(dat$year))), rep(60, length(unique(dat$year)))),
                     model = NA)

  dat %>%
    ggplot2::ggplot(ggplot2::aes(year, biomass, color = model, fill = model)) +
    ggplot2::geom_blank(data = dummy) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lci, ymax = uci), alpha = 0.1, color = NA) +
    ggplot2::facet_wrap(~name, dir = "v", scales = "free_y") +
    ggplot2::scale_y_continuous(name = "Biomass (kt)", labels = scales::comma) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(name = "Year",
                                breaks = funcr::tickr(dat, year, 10, start = 1960)$breaks,
                                labels = funcr::tickr(dat, year, 10, start = 1960)$labels) +
    scico::scale_color_scico_d(palette = 'batlow', begin = 0.2, end = 0.8) +
    scico::scale_fill_scico_d(palette = 'batlow', begin = 0.2, end = 0.8) +
    funcr::theme_report() +
    ggplot2::theme(legend.position = c(0.2, .8))

  # ggplot2::ggsave(here::here(year, "compare_models", "figs", "compare_est_biomass.png"),
  # width = 6.5, height = 6.5, units = "in", dpi = 200)
}
