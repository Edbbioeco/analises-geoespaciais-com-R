# Pacotes ----

library(geobr)

library(tidyverse)

library(rgbif)

library(sf)

library(geodata)

library(tidyterra)

library(terra)

library(fields)

library(vegan)

library(reshape2)

# Dados ----

## Shapefile do Brasil ----

### Baixar ----

br <- geobr::read_country(year = 2019)

### Visualizar ----

br

ggplot() +
  geom_sf(data = br, color = "black")

## Shapefile da Mata Atlântica ----

### Baixar ----

ma <- geobr::read_biomes(year = 2019) |>
  dplyr::filter(name_biome == "Mata Atlântica")

### Visualizar ----

ma

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen")

## Registros de ocorrência ----

### Baixar ----

registros <- rgbif::occ_data(scientificName = "Boana albomarginata",
                             hasCoordinate = TRUE,
                             limit = 1e6,
                             geometry = ma |> sf::st_bbox()) %>%
  .$data

### Visualizar ----

registros

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen") +
  geom_point(data = registros,
             aes(x = decimalLongitude, y = decimalLatitude), color = "red", size = 2)

## Variáveis bioclimáticas ----

### Baixar ----

bioclim <- geodata::worldclim_country(var = "bio",
                                      res = 0.5,
                                      path = getwd(),
                                      country = "BRA")

### Visualizar ----

bioclim

ggplot() +
  tidyterra::geom_spatraster(data = bioclim) +
  geom_sf(data = ma, color = "darkred", fill = "transparent") +
  facet_wrap(~lyr) +
  scale_fill_viridis_c() +
  theme_minimal()
