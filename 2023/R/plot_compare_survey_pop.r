plot_compare_survey_pop <- function(year, model_dirs = c('2022, default', '2022, lognormal'),
savedir = NULL) {

  dat = data.frame()
  m = list(rep(NA, length(model_dirs)))

  for(i in 1:length(model_dirs)){
    id = basename(model_dirs[i])

    dat %>%
      dplyr::bind_rows(
        read.csv(paste0(model_dirs[i],"/processed/survey.csv")) %>%
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
                   legend.position=c(0.4,0.75))


if(!is.null(savedir)){
  ggplot2::ggsave( paste0(savedir,"/", Sys.Date(),"-survey_compare.png"),
  width = 6.5, height = 6.5, units = "in", dpi = 200)
  }else{
  ggplot2::ggsave( here(paste0( Sys.Date(),"-survey_compare.png")),
  width = 6.5, height = 6.5, units = "in", dpi = 200)
  }
}