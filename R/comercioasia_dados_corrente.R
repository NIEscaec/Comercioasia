
#' Dados de fluxo de comércio bilateral entre Brasil e um determinado país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @return tibble com dados de comércio bilateral de 2010 em diante.
#'
#' @export

comercioasia_dados_corrente <- function(pais, periodo) {

  df <- comerciobr2::sh1_df %>%
    dplyr::filter(no_pais %in% pais)

  if (periodo == "anual") {

    df <- df %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::summarise(value = sum(value))
  }

  else {

    df <- df %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::summarise(value = sum(value))

  }

  df <- df %>%
    tidyr::pivot_wider(names_from = path, values_from = value) %>%
    dplyr::mutate(Corrente = .data$EXP + .data$IMP,
                  Saldo = .data$EXP - .data$IMP) %>%
    dplyr::rename(Exportacoes = "EXP", Importacoes = "IMP") %>%
    tidyr::pivot_longer(.data$Exportacoes:.data$Saldo, names_to = "trade_flow", values_to = "value")

  df
}
