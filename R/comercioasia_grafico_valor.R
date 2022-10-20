#' Grafico dos dados extraidos da base_omc
#'
#' @param pais um país
#' @param periodo "anual"
#'
#' @export

comercioasia_grafico_valor <- function(pais1, pais2){


  df_mundo <- Asia::comercioasia_dados_omc(pais1)
  df_br <- Asia::comercioasia_dados_omc(pais2)
  df.valor <- dplyr::full_join(df_mundo, df_br)

  df.valor %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(Year, value, fill = .data$Reporting.Economy),
                      show.legend = F) +
    ggplot2::facet_wrap(~factor(.data$Reporting.Economy),
                        scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_discrete(breaks = scales::breaks_pretty()) +
    ggplot2::labs(title = glue::glue("Exportações totais"),
                  x = NULL, y = NULL, caption = "Fonte: OMC") +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()

}
