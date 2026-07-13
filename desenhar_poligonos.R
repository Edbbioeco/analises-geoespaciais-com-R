# Pacotes ----

library(geobr)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(leafem)

library(terra)

library(mapedit)

# Dados ----

## Importando ----

slm <- geobr::read_municipality(year = 2019) |>
  dplyr::filter(name_muni == "São Lourenço Da Mata")

## Visualizando ----

ggplot() +
  geom_sf(data = slm)

# Mapa de satélite ----

mapa <- leaflet::eaflet::leaflet() |>
  leaflet::addProviderTiles(providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(
    targetGroup = "Draw",
    polylineOptions = leaflet.extras::drawPolylineOptions(),
    polygonOptions = leaflet.extras::drawPolygonOptions(),
    circleOptions = leaflet.extras::drawCircleOptions(),
    rectangleOptions = leaflet.extras::drawRectangleOptions(),
    markerOptions = leaflet.extras::drawMarkerOptions(),
    circleMarkerOptions = leaflet.extras::drawCircleMarkerOptions(),
    editOptions = leaflet.extras::editToolbarOptions()) |>
  leafem::addMouseCoordinates() |>
  leaflet::addPolygons(data = slm |>
                         sf::st_as_sf() |>
                         sf::st_transform(4326))


mapa

# Edição interativa ----

shapes <- mapa |>
  mapedit::editMap(targetGroup = "draw",
                   polylineOptions = TRUE,
                   polygonOptions = TRUE,
                   circleOptions = TRUE,
                   rectangleOptions = TRUE,
                   markerOptions = TRUE,
                   editOptions = leaflet.extras::editToolbarOptions())

# Extraindo as feições ----

sfs <- shapes$finished

ggplot() +
  geom_sf(data = sfs)
