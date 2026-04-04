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

## Filtrar os registros de ocorrencia baseado nos valores faltantes ----

registros_trat <- registros |>
  dplyr::slice(-valores_faltantes)

registros_trat

## Filtrar os valores das variáveis bioclimáticas baseado nos valores faltantes ----

valores_trat <- valores |>
  dplyr::slice(-valores_faltantes)

valores_trat

# Função ----

## Definir as variáveis para testar ao longo da função ----

coords <- registros_trat

coords_var <- 3:4

envs <- valores_trat

envs_var <- 2:19

## Criar função ----

filtrar_dist <- function(coords, coords_var, envs, envs_var){

  coords_reg <- coords |>
    dplyr::select(dplyr::all_of(coords_var)) |>
    as.data.frame() |>
    fields::rdist.earth(miles = FALSE) |>
    reshape2::melt() |>
    dplyr::mutate(combinacao = paste0(Var1, "-", Var2)) |>
    dplyr::rename("distancia" = value)

  dist_ambs <- function(envs_var){

    nome_var <- envs |>
      dplyr::select(envs_var) |>
      names()

    env_dist <- valores_trat |>
      dplyr::select(n_vars) |>
      vegan::vegdist(method = "euclidean") |>
      as.matrix() |>
      reshape2::melt() |>
      dplyr::mutate(combinacao = paste0(Var1, "-", Var2),
                    var = nome_var)

    assign(paste0("env_dist_", nome_var),
           env_dist,
           envir = globalenv())

  }

  purrr::map(envs_var, dist_ambs)

  df <- coords_reg |>
    dplyr::left_join(ls(pattern = "^env_dist_") |>
                       mget(envir = globalenv()) |>
                       dplyr::bind_rows(),
                     by = "combinacao")

  df

}
