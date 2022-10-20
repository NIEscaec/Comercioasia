#' Dados de fluxo de comércio bilateral entre Brasil e um determinado país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @return tibble com dados de comércio bilateral de 2010 em diante.
#'
#' @export


comercioasia_dados_divisao <- function(periodo){

  df_asia <- Asia::comercioasia_dados_corrente(bloco_asia, periodo)

  df_mundo <- Asia::comercioasia_dados_corrente(bloco_mundo, periodo)

  df_mundo$value <- df_asia$value/df_mundo$value

  df_mundo <- subset(df_mundo, trade_flow != "Saldo")

  df_mundo

}
