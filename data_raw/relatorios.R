retirar_lista_paises <- c("Não Definido", "Provisão de Navios e Aeronaves", "Bancos Centrais",
                          "A Designar", "Niue", "Cocos (Keeling), Ilhas", "Marianas do Norte, Ilhas",
                          "Wake, Ilha", "Sint Maarten", "Midway, Ilhas", "Guernsey", 
                          "Madeira, Ilha da", "Território Britânico do Oceano Índico", 
                          "São Pedro e Miquelon", "Svalbard e Jan Mayen", "Terras Austrais Francesas",
                          "Geórgia do Sul e Sandwich do Sul, Ilhas", "Heard e ilhas mcdonald, Ilha",
                          "Organizações Internacionais", "Mayotte", "Guam", "São Bartolomeu",
                          "Lebuan, Ilhas", "Bouvet, Ilha", "Brasil", "Antártica", "Madagascar")

lista_paises <- unique(comerciobr2::sh1_df$no_pais)
lista_paises_filtrada <- lista_paises[! lista_paises %in% retirar_lista_paises]


#barao2::relatorio_brasil_pais("China")

purrr::walk(lista_paises_filtrada, barao2::relatorio_brasil_pais)



library(roxygen2)


