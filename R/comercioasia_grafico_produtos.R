#' Gráfico dos cinco principais produtos comercializados do Brasil com um país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @export

comercioasia_grafico_produtos <- function(pais, periodo) {


  df <- comercioasia_dados_produtos(pais, periodo) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(no_sh4_por = dplyr::case_when(stringr::str_length(no_sh4_por) > 30 ~
                                              paste0(stringr::str_sub(no_sh4_por, 1, 30), ".."),
                                              TRUE ~ no_sh4_por))
  if (periodo == "anual") {
    df <- df %>%
      dplyr::filter(co_ano == max(co_ano)-1)
    frase <- paste0(Asia::comercioasia_get_ulimoano()-1)
  }

  else {
    df <- df %>%
      dplyr::filter(co_ano == max(co_ano))
    frase <- paste0(Asia::comercioasia_get_ulimoano()," at\u00e9 ", Asia::meses(Asia::comercioasia_get_ultimomes()))
  }

  ano <- df %>%
    dplyr::distinct(co_ano) %>%
    dplyr::pull(co_ano)


  df %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(tidytext::reorder_within(no_sh4_por, value, path, sum),
                                   value, fill = path),
                      show.legend = F) +
    ggplot2::facet_wrap(~ path, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggthemes::scale_fill_tableau() +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    tidytext::scale_x_reordered() +
    ggplot2::labs(title = glue::glue("Ásia, pauta comercial, {frase}"),
                  x = NULL, y = NULL, caption = "Fonte: Minist\u00e9rio da Economia")

}
