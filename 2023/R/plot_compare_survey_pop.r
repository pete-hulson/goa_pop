plot_compare_survey_pop <- function(year, models = c('2022, default', '2022, lognormal')) {

  if (!dir.exists(here::here(year, "compare_models"))){
    dir.create(here::here(year, "compare_models"))
  }

  dat = data.frame()
  m = list(rep(NA, length(models)))
  for(i in 1:length(models)) {
    m[[i]] = scan(text = models[i], sep = ",", what = "")
    m[[i]][2] = gsub(" ", "", m[[i]][2])
    # m[[i]][3] = gsub(" ", "", m[[i]][3])

    year = m[[i]][1]
    # folder = m[[i]][2]
    model = m[[i]][2]

    id = model
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

    dat %>%
      dplyr::bind_rows(
        read.csv(here::here(year,  model, "processed", "survey.csv")) %>%
          dplyr::rename_all(tolower) %>%
          dplyr::select(year = starts_with("y"),
                        Observed = starts_with("bio"),
                        Predicted = pred,
                        se, lci, uci) %>%
          tidyr::pivot_longer(-c(year, se, uci, lci)) %>%
          dplyr::mutate(value = value / 1000,
                        uci = uci / 1000,
                        lci = lci / 1000,
                        model = id)) -> dat
  }

  dat %>%
    ggplot2::ggplot(ggplot2::aes(year, value, color = model)) +
    ggplot2::geom_point(data = dplyr::filter(dat, name == "Observed"), position=ggplot2::position_dodge(width=0.7)) +
    ggplot2::geom_errorbar(data = dplyr::filter(dat, name == "Observed"),
                           ggplot2::aes(ymin = lci, ymax = uci, color = model), width = 0.4, position=ggplot2::position_dodge(width=0.7)) +
    ggplot2::geom_line(data = dplyr::filter(dat, name == "Predicted"),
                       ggplot2::aes(color = model)) +
    scico::scale_color_scico_d(palette = 'batlow', begin = 0.2, end = 0.8) +
    ggplot2::scale_x_continuous(breaks = funcr::tickr(dat, year)$breaks,
                                labels = funcr::tickr(dat, year)$labels) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Survey biomass (kt)") +
    ggplot2::expand_limits(y = 0) +
    funcr::theme_report() +
    ggplot2::theme(legend.justification=c(1,0),
                   legend.position=c(0.8,0.70))

  # ggplot2::ggsave(here::here(year, "compare_models", "figs", "compare_est_survey.png"),
  # width = 6.5, height = 6.5, units = "in", dpi = 200)
}