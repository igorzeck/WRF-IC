# Script para transformar arquivos CSV WRFOUT em formato fac-similé ao METAR
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
    in_dt_path <- readline(prompt = "Caminho para o arquivo WRFOUT (CSV): ")
    if (in_dt_path != "") {
        dt_path <- in_dt_path
    }
}

if (!file.exists(dt_path)) {
  stop("ERRO: Arquivo não encontrado: ", dt_path)
}

# 1. Carregar Dados do WRFOUT ----
wrf_raw <- read_csv(dt_path, show_col_types = FALSE)

# 2. Limpeza do dataset ----
janitor::clean_names(wrf_raw) %>%
  filter(!is.na(datetime)) -> wrf_clean

# 3. Transformações ----
wrfout_metar_df <- wrf_clean %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    temp_ar = temperature_htgl - 273.15,
    temp_orvalho = dew_point_temperature_htgl,
    pressao = pressure / 100,
    vel_vento = wind_speed * 1.94384,
    umidade_relativa = relative_humidity_htgl
  ) %>% select(datetime, temp_ar, temp_orvalho, pressao, vel_vento, umidade_relativa)

# 4. Exportar para CSV
wrfout_metar_df %>%
  write_csv(paste0("datasets/metar_fac_simile/metar_", dt_path %>% basename()))