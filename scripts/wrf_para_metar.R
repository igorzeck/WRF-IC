# Script para converter variáveis de saída do WRF (em CSV) para a variáveis METAR ----
library(tidyverse)

# 1. Carregamento ----
dataset_file <- "wrf_raw_out2"

# Valores da linha de comando
args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) dataset_file <- args[1]

dataset_path <- paste0("datasets/", dataset_file, ".csv")


df_raw <- read_csv(dataset_path, show_col_types = FALSE)

# Se a coluna 'visibility' não existir (pois veio do netcdf direto), faz join com as observações reais de 2026
# NOTE: Esse é o ideal já que a visibilidade é a variável alvo!
if (!"visibility" %in% colnames(df_raw)) {
  metar_obs <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
    select(datetime, visibility)
  df_raw <- df_raw %>%
    left_join(metar_obs, by = "datetime")
}

# 2. Conversões Físicas ----
df_mos <- df_raw %>%
  filter(is.finite(visibility)) %>%
  mutate(
    # Wind Speed: m/s -> Knots
    vel_vento = wind_speed * 1.94384,
    
    # Wind Direction
    dir_vento = wind_angle,
    
    # Air Temp: Kelvin -> Celsius
    temp_ar = temperature_htgl - 273.15,
    
    # Dew Point: já em Celsius
    temp_orvalho = dew_point_temperature_htgl,
    
    # Pressure: Pa -> hPa
    pressao = pressure / 100,
    
    # Target: visiblide (m)
    vis = visibility,
    
    # Weather description: valor padrão
    clima = "Sem Info",
    
    # Cloud Category: Mapeia frações para categóricos
    categ_nuvem = case_when(
      total_cloud_cover <= 0 ~ "No",
      total_cloud_cover <= 0.25 ~ "Few",
      total_cloud_cover <= 0.50 ~ "Scattered",
      total_cloud_cover <= 0.875 ~ "Broken",
      TRUE ~ "Overcast"
    ),
    
    # Cloud height: já em metros
    altura_nuvem = geopotential_height_ceil,
    
    # Lake mix-layer temperature (SST): Kelvin -> Celsius
    lmlt = temperature_sea_temperature - 273.15,
    
    # Relative Humidity: porcentagem (0-100) -> fracao (0-1)
    umidade_relativa = relative_humidity_htgl / 100
  ) %>%
  select(datetime, vel_vento, dir_vento, temp_ar, temp_orvalho, pressao, vis, clima, categ_nuvem, altura_nuvem, lmlt, umidade_relativa)

# 3. Salvar ----
output_path <- paste0("datasets/wrf_emulated_", dataset_file, ".csv")
write_csv(df_mos, output_path)
message("Conversão concluída. Arquivo salvo como: ", output_path)
