#' Tabela dos dados extraidos da base_omc
#'
#' @param pais um país
#' @param periodo "anual"
#'
#' @export


comercioasia_tabela_valores <- function(pais1, pais2){

    df_mundo <- Asia::comercioasia_dados_omc(pais1)
    df_br <- Asia::comercioasia_dados_omc(pais2)

    df.final <- dplyr::full_join(df_mundo, df_br)%>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = .data$Reporting.Economy , values_from = value)%>%
      dplyr::mutate(Mundo = .data$World/dplyr::lag(.data$World)-1)%>%
      dplyr::mutate(Brasil = .data$Brazil/dplyr::lag(.data$Brazil)-1) %>%
      dplyr::filter(Year != min(Year)) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with(c("Mun","Brasil")), scales::label_percent()))%>%
      dplyr::mutate(dplyr::across(dplyr::starts_with(c("Mun","Brasil")), ~ paste0("(", .x, ")"))) %>%
      dplyr::mutate(Year = as.character(Year)) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), scales::label_number_si())) %>%
      tidyr::unite("Mundo", c("World","Mundo"), sep = " ") %>%
      tidyr::unite("Brasil", c("Brazil", "Brasil"), sep = " ") %>%
      dplyr::relocate(Year, .data$Mundo, .data$Brasil) %>%
      dplyr::arrange(dplyr::desc(Year)) %>%
      tidyr::pivot_longer(.data$Mundo:.data$Brasil, names_to = "trade_flow", values_to = "value")%>%
      tidyr::pivot_wider(names_from = Year, values_from = value)%>%
      dplyr::rename(" " = .data$trade_flow)%>%
      dplyr::select(1:10)%>%
      tibble::column_to_rownames(var = " ")

    tabela <- split(1:(ncol(df.final)), sort(rep_len(1:2, ncol(df.final)))) %>%
      purrr::map(~ dplyr::select(df.final, .)) %>%
      purrr::map(~ kableExtra::kbl(.x, format = "latex", booktabs = TRUE, align = "r", escape = T)) %>%
      purrr::map(~ kableExtra::kable_styling(.x, position = "center", full_width = FALSE, latex_options = "hold_position"))

    tabela

}
