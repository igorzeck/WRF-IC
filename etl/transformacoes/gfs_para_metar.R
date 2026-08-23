# Script para transformar arquivos CSV GFS em formato fac-similé ao METAR
# 0. Setup ----
library(tidyverse)
library(lubridate)
library(janitor)

dt_path <- ""

# Ler argumentos de linha de comando, se fornecidos
args <- commandArgs(trailingOnly = TRUE)


if (length(args) > 0) {
  dt_path <- args[1]
}


if (interactive() && length(args) == 0) {
    in_dt_path <- readline(prompt = "Caminho para o arquivo GFS (CSV): ")
    if (in_dt_path != "") {
        dt_path <- in_dt_path
    }
}

if (!file.exists(dt_path)) {
  stop("ERRO: Arquivo não encontrado: ", dt_path)
}

# 1. Carregar Dados do GFS ----
gfs_raw <- read_csv(dt_path, show_col_types = FALSE)

# 2. Limpeza do dataset ----
janitor::clean_names(gfs_raw) %>%
  filter(!is.na(datetime)) -> gfs_clean

# 3. Transformações ----
gfs_metar_df <- gfs_clean %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    vel_vento = vel_vento * 1.94384, # m/s -> nós
    temp_ar = temp_ar,
    temp_orvalho = temp_orvalho,
    pressao = pressao,
    umidade_relativa = umidade_relativa * 100
  ) %>% select(datetime, vel_vento, temp_ar, temp_orvalho, pressao, umidade_relativa)

# 4. Exportar para CSV
gfs_metar_df %>%
  write_csv(paste0("datasets/metar_fac_simile/metar_", dt_path %>% basename()))