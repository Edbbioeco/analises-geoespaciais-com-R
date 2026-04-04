# Pacotes ----

library(geobr)

library(tidyverse)

library(rgbif)

library(sf)

library(geodata)

library(terra)

library(tidyterra)

library(fields)

library(vegan)

library(reshape2)

# Dados ----

## Registros de ocorrência ----

### Baixar ----

registros <- rgbif::occ_data(scientificName = "Boana albomarginata",
                             hasCoordinate = TRUE,
                             limit = 1e6,
                             geometry = ma |> sf::st_bbox()) %>%
  .$data
