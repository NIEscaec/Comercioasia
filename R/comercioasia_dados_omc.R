#' Dados extraidos da base_omc
#'
#' @param pais um país
#' @param periodo "anual"
#'
#' @export

comercioasia_dados_omc <- function(pais){

  data_omc <- read.csv("base_omc/Data.csv")
  RE <- c(pais)
  ano_data <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
  exp_val <- data_omc %>%
    dplyr::select("Reporting.Economy", "Year", "Value") %>%
    dplyr::filter(Year %in% ano_data)%>%
    dplyr::filter(Reporting.Economy == pais)%>%
    dplyr::group_by(Year ,Reporting.Economy ) %>%
    dplyr::summarise(value = sum(Value))

  exp_val

}





