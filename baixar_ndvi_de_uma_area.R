# Pacotes ----

library(sf)

library(tidyverse)

library(rgee)

library(googledrive)

library(terra)

library(tidyterra)

# Iniciando o projeto ----

ee_Initialize(project = "ee-edsonbbiologia",
              user = "edsonbbiologia@gmail.com",
              drive = TRUE)

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

saltinho_ee

## Área de Saltinho ----

saltinho_area_ee <- saltinho |> 
  sf::st_bbox() |> 
  sf::st_as_sfc() |>
  sf::st_as_sf() |> 
  rgee::sf_as_ee()

saltinho_area_ee

## Raster de NDVI ----

### Importando ----

imagem_processada <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(saltinho_ee)$
  filterDate("2023-01-01", "2026-03-01")$
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

## Raster de satélite de Saltinho ----

### Importando ----

imagem_satelite <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(saltinho_area_ee)$
  filterDate("2023-01-01", "2026-03-01")$
  filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 10))$ 
  median()$
  select("B4", "B3", "B2")$
  clip(saltinho_area_ee)

### Exportando para o Google Drive ----

task_satelite <- rgee::ee_image_to_drive(
  image = imagem_satelite,
  description = "Sentinel2_10m_Export",
  folder = "RGEE_Resultados",
  scale = 10, 
  region = saltinho_area_ee$geometry()
)

task_satelite$start()

# Arquivo do Google Drive ----

## NDVI ----

### Baixando ----

googledrive::drive_download(file = googledrive::drive_find(pattern = "NDVI",
                                                           n_max = 1),
                            path = "meu_ndvi_local.tif", 
                            overwrite = TRUE)

### Removendo ----

googledrive::drive_rm(file = googledrive::drive_find(pattern = "NDVI", 
                                                     n_max = 1))

## Imagem de satélite ----

### Baixando ----

googledrive::drive_download(file = googledrive::drive_find(pattern = "Sentinel2",
                                                           n_max = 1),
                            path = "meu_satelite_local.tif",
                            overwrite = TRUE)

### Removendo ----

googledrive::drive_rm(file = googledrive::drive_find(pattern = "Sentinel2"))

# Rasters ----

## NDVI de Saltinho ----

saltinho_ndvi <- terra::rast("meu_ndvi_local.tif")

saltinho_ndvi

## Imagem de Satélite de Saltinho ----

saltinho_sat <- terra::rast("meu_satelite_local.tif")

saltinho_sat

## Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = saltinho_sat,
                                 stretch = "lin") +
  tidyterra::geom_spatraster(data = saltinho_ndvi) +
  geom_sf(data = saltinho, color = "red", fill = NA, 
          linewidth = 1,
          linetype = "dashed") +
  scale_fill_gradientn(colours = c("#4C2915",
                                   "#784C1F",
                                   "#C5A51A", 
                                   "#37A336",
                                   "#0C632C"),
                       na.value = "transparent",
                       limits = c(-1, 1),
                       name = "NDVI") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"))
