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

# Shapefile dos registros ----

## Criar ----

registros_sf <- registros |>
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = ma |> sf::st_crs()) |>
  sf::st_intersection(ma)

## Visualizar ----

registros_sf

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = registros_sf, color = "red", size = 2)

# Recorte das variáveis bioclimáticas ----

## Alterar o CRS do raster para o shapefile da Mata Atlântica ----

bioclim <- bioclim |>
  terra::project(ma)

## Recortar ----

bioclim_recortado <- bioclim |>
  terra::crop(ma) |>
  terra::mask(ma)

## Visualizar ----

bioclim_recortado

ggplot() +
  tidyterra::geom_spatraster(data = bioclim_recortado) +
  geom_sf(data = ma, color = "darkred", fill = "transparent") +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(na.value = "transparent") +
  theme_minimal()

# Extrair os valores das variáveis bioclimáticas ----

## Extrair ----

valores <- bioclim_recortado |>
  terra::extract(registros_sf)

## Visualizar ----

valores

## Identificando os pontos com valores faltantes ----

valores_faltantes <- valores |>
  dplyr::filter(dplyr::everything() |> if_any(is.na)) |>
  dplyr::pull(ID)

valores_faltantes

## Filtrar do shapefile de registros baseado nos valores faltantes ----

registros_sf_trat <- registros_sf |>
  dplyr::slice(-valores_faltantes)

registros_sf_trat

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = ma, color = "darkgreen", fill = "forestgreen") +
  geom_sf(data = registros_sf_trat, color = "red", size = 2)
