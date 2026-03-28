# Pacotes ----

library(geobr)

library(sf)

library(tidyverse)

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

download.file("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif",
              "mapbiomas_2024_local.tif",
              mode = "wb")

mapbiomas <- terra::rast("mapbiomas_2024_local.tif")

### Visualizando ----

mapbiomas

ggplot() +
  tidyterra::geom_spatraster(data = mapbiomas) +
  scale_fill_viridis_c()

# Recorte para Saltinho ----

## Igualando os CRS ----

terra::crs(mapbiomas) <- "EPSG:4674"
