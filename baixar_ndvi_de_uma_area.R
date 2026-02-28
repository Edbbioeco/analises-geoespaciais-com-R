# Pacotes ----

library(sf)

library(tidyverse)

library(rgee)

library(googledrive)

library(terra)

library(tidyterra)

# Iniciando o projeto ----

ee$Initialize(project = "ee-edsonbbiologia")

# Dados ----

## Saltinho ----

### Importando ----

saltinho <- sf::st_read("./projeto mestrado/mestrado/Saltinho.shp")

### Visualizando ----

ggplot() +
  geom_sf(data = saltinho)

### Convertendo para geojson ----

saltinho_ee <- saltinho |> 
  rgee::sf_as_ee()

## Raster ----

### Importando ----

imagem_processada <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(saltinho_ee)$
  filterDate("2023-01-01", "2026-02-28")$
  filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 10))$
  median()$
  normalizedDifference(c("B8", "B4"))$ 
  rename("NDVI")$
  clip(saltinho_ee)

### Exportando para o Google Drive

task <- rgee::ee_image_to_drive(image = imagem_processada,
                                description = "Meu_NDVI_10m",
                                folder = "RGEE_Resultados",
                                scale = 10, 
                                region = saltinho_ee$geometry())

task$start()

# Baixar do Google Drive ----

googledrive::drive_download(file = googledrive::drive_find(pattern = "NDVI", n_max = 1),
                            path = "meu_ndvi_local.tif", 
                            overwrite = TRUE)

# NDVI de Saltinho ----

## Importando ----

saltinho_raster <- terra::rast("meu_ndvi_local.tif")

## Visualizando ----

saltinho_raster

ggplot() +
  tidyterra::geom_spatraster(data = saltinho_raster) +
  scale_fill_viridis_c(na.value = "transparent") +
  facet_wrap(~lyr)

## Calculo de NDVI ----

saltinho_ndvi <- (saltinho_raster$B8 - saltinho_raster$B4) / (saltinho_raster$B8 + saltinho_raster$B4)

ggplot() +
  tidyterra::geom_spatraster(data = saltinho_ndvi) +
  tidyterra::scale_fill_whitebox_c(palette = "atlas",
                                   na.value = "transparent",
                                   limits = c(-1, 1),
                                   direction = -1)
