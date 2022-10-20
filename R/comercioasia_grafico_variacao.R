#' Dados extraidos da base_omc
#'
#' @param pais um país
#' @param periodo "anual"
#'
#' @export
#'

pais1 <- "World"
pais2 <- "Brazil"

comercioasia_grafico_variacao <- function(pais1, pais2){

  df_mundo <- Asia::comercioasia_dados_omc(pais1)
  df_br <- Asia::comercioasia_dados_omc(pais2)
  df_valor <- dplyr::full_join(df_mundo, df_br)
  df.final <- dplyr::full_join(df_mundo, df_br)%>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = .data$Reporting.Economy , values_from = value)%>%
  dplyr::mutate(var_br = ((.data$Brazil)/dplyr::lag(.data$Brazil)-1)) %>%
  dplyr::mutate(var_wrl = ((.data$World)/dplyr::lag(.data$World)-1))

  teste <- df.final

  a <- df.final %>%
    select(Year, Brazil, var_br)

  b <- df.final %>%
    select(Year, World, var_wrl)

  a <- rename(a,  "value" = "var_br")
  b <- rename(b,  "value" = "var_wrl")

  novo <- bind_rows(a,b)
  #---------------------------------

  df.grafico <- data.frame(novo$Year, df_valor$Reporting.Economy, novo$value)
  df.grafico[is.na(df.grafico)] <- 0

  df.grafico <- rename(df.grafico, "Transação" = "df_valor.Reporting.Economy")

  df.grafico %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(novo.Year, novo.value, color = .data$Transação),
                      show.legend = T)+
    scale_y_continuous(labels = scales::label_percent())+
    scale_x_continuous(breaks = seq(2012, 2021))+
    ggplot2::labs(title = glue::glue("Variação da Exportações Totais"),
                  x = NULL, y = NULL, caption = "Fonte: OMC") +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()

}
