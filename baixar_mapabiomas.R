# Pacotes ----

library(geobr)

library(sf)

library(tidyverse)

library(rgee)

library(terra)

library(tidyterra)

# Dados ----

## Shapefile de Saltinho ----

### Importando ----

saltinho <- geobr::read_conservation_units() |>
  sf::st_make_valid() |>
  dplyr::filter(name_conservation_unit |> stringr::str_detect("SALTINHO"))

### Visualizando ----

ggplot() +
  geom_sf(data = saltinho, color = "black")

## Map biomas ----

### Iniciando o rgee ----

options(
  rgee.asset_home = "projects/earthengine-legacy/assets/users/ee-edsonbbiologia"
)

rgee::ee_Initialize(project = "ee-edsonbbiologia",
                    user = "edsonbbiologia@gmail.com",
                    drive = TRUE)

### Importando ----
