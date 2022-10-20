#' Gráfico de Fluxo de Comércio Brasil-país
#'
#' Com base na função \code{comerciobr_dados_corrente}.
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

comercioasia_grafico_corrente <- function(pais, periodo) {

  frase2 <- paste0("agregado at\u00e9 ", meses(Asia::comercioasia_get_ultimomes()))

  if (pais == bloco_asia) {


      frase <- paste0("Ásia")

    }

  else {

    frase <- paste0("Mundo")
  }

  comercioasia_dados_corrente(pais, periodo) %>%
    dplyr::mutate(co_ano = as.character(co_ano)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(co_ano, value, fill = .data$trade_flow),
                      show.legend = F) +
    ggplot2::facet_wrap(~factor(.data$trade_flow,
                                levels = c("Exportacoes", "Importacoes",
                                           "Corrente", "Saldo")),
                        scales = "free_x") +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::scale_x_discrete(breaks = scales::breaks_pretty()) +
    ggplot2::labs(title = glue::glue("Brasil - {frase}, Fluxo de Com\u00e9rcio {frase2}"),
                  x = NULL, y = NULL, caption = "Fonte: Minist\u00e9rio da Economia") +
    ggthemes::scale_fill_tableau() +
    ggplot2::theme_minimal()

}
