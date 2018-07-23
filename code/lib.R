library(tidyverse)

theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal()
    ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                            margin=margin(b=strip_text_margin))
    ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                               margin=margin(b=subtitle_margin))
    ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                             margin=margin(b=plot_title_margin))
    ret
}

import_data <- function(){
    original = readr::read_csv(here::here("data/eleicoes2014.csv"), 
                               locale = locale(encoding = "latin1"))
    eleitorado = read_csv(here::here("data/total_votos_uf.csv")) %>% 
        select(UF = uf, total_votos_uf = votos)
    
    gastos_votos = original %>%
        select(nome, 
               UF, 
               partido,
               votos,
               total_despesa,
               quantidade_doadores, 
               quantidade_despesas,
               recursos_de_pessoas_juridicas,
               recursos_de_partidos,
               recursos_de_pessoas_físicas, 
               recursos_proprios,
               idade, 
               sexo, 
               grau
               ) %>% 
        mutate_at(
            vars(
                recursos_de_pessoas_juridicas,
                recursos_de_pessoas_físicas,
                recursos_de_partidos,
                recursos_proprios
            ), 
            function(x){if_else(is.na(x), 0, x)}
        ) %>%
        mutate(
            prop_doacoes_pj = recursos_de_pessoas_juridicas / total_despesa,
            prop_doacoes_pf = recursos_de_pessoas_físicas / total_despesa,
            prop_doacoes_partido = recursos_de_partidos / total_despesa, 
            prop_recursos_proprios = recursos_proprios / total_despesa
        ) %>% 
        left_join(eleitorado, by = "UF") %>% 
        mutate(votos_prop = votos / total_votos_uf)
    
    gastos_votos %>% 
        write_csv(here::here("data/gastos_e_votos.csv"))
}

read_projectdata <- function(){
    read_csv(here::here("data/gastos_e_votos.csv"), 
             col_types = cols(
                 nome = col_character(),
                 UF = col_character(),
                 partido = col_character(),
                 votos = col_integer(),
                 total_despesa = col_double(),
                 quantidade_doadores = col_integer(),
                 quantidade_despesas = col_integer(),
                 recursos_de_pessoas_juridicas = col_double(),
                 idade = col_integer(),
                 sexo = col_character(),
                 grau = col_character(),
                 total_votos_uf = col_integer(),
                 votos_prop = col_double()
             )) 
}
