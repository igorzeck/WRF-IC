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
  select(datetime, temperature_htgl, dew_point_temperature_htgl, pressure, wind_speed, relative_humidity_htgl) %>%
  filter(!is.na(datetime)) -> gfs_clean

# 3. Transformações ----
gfs_metar_df <- gfs_clean %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    vel_vento_gfs = vel_vento * 1.94384, # m/s -> nós
    temp_ar_gfs = temp_ar,
    temp_orvalho_gfs = temp_orvalho,
    pressao_gfs = pressao,
    umidade_relativa_gfs = umidade_relativa * 100
  ) %>% select(datetime, ends_with("_gfs"))

# 4. Exportar para CSV
gfs_metar_df %>%
  write_csv(paste0("datasets/metar_fac_simile/metar_", dt_path %>% basename()))