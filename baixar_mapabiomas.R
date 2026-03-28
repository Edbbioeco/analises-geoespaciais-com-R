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

### Importando ----

mapbiomas <- datazoom.amazonia::load_mapbiomas(dataset = "mapbiomas_cover")

### Visualizando ----

mapbiomas

ggplot() +
  tidyterra::geom_spatraster()
