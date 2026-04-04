# Pacotes ----

library(geobr)

library(rgbif)

library(tidyverse)

library(geodata)

library(terra)

library(tidyterra)

library(sf)

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
