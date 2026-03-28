# Pacotes ----

library(gert)

library(tidyverse)

# Arquivos áptos ----

gert::git_status() |>
  as.data.frame()

# Adicionando arquivos ----

gert::git_add(files = "git_comandos.R") |>
  as.data.frame()

# Commit -----

gert::git_commit(message = "Script para comandos de Git")

# Push ----

gert::git_push(remote = "origin")

# Pull ----

gert::git_pull(remote = "origin")

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft()

# Excluindo arquivos -----

## Removendo ----

gert::git_rm(files = list.files(pattern = ".png$|.csv$|.shp$|.shx$|.dbf$|.prj$|dados")) |>
  as.data.frame()

## Commit -----

gert::git_commit(message = "Removendo arquivos")

## Push ----

gert::git_push(remote = "origin")

## Pull ----

gert::git_pull(remote = "origin")
