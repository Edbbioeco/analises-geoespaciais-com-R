# Pacotes ----

library(tidyverse)

library(magrittr)

library(parzer)

library(geosphere)

library(writexl)

# Dados ----

## Importando ----

dados <- readr::read_csv2("calcular_coordenadas/dados_gps.csv")

## Visualizando ----

dados

## Tratando ----

dados %<>%
  dplyr::mutate(long = long |> parzer::parse_lon(),
                lat = lat |> parzer::parse_lat()) %<>%
  dplyr::select(4:5) %<>%
  dplyr::relocate(long, .before = lat)

dados

# Cálculo de graus de Aazimuthe e distâncias ----

## Função dos graus de Azimuthe ----

calcular_graus <- function(x){

  graus <- geosphere::finalBearing(dados[x - 1, 1:2],
                                   dados[x, 1:2])

  dados$azimuthe[x -1] <<- graus

}

purrr::walk(2:nrow(dados), calcular_graus)

dados |> as.data.frame()

## Função das distâncias----

calcular_dist <- function(x){

  distancia <- geosphere::distGeo(dados[x - 1, 1:2],
                                  dados[x, 1:2])

  dados$distance[x - 1] <<- distancia

}

purrr::walk(2:nrow(dados), calcular_dist)

dados |> as.data.frame()

## Inserindo NA ----

dados[c(2:15, 17:30), 1:2] <- NA

dados |> as.data.frame()

## Exportando ----

dados |>
  writexl::write_xlsx("calcular_coordenadas/coordinates.xlsx")
