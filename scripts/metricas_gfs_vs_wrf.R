# Script para calcular métricas (RMSE, Bias, Pearson r, R2) para GFS vs WRF 
# Focado exclusivamente no período da Magna Simulation (27-28 Junho 2026)
# 0. Setup ----
library(tidyverse)
library(lubridate)

# 1. Carregar Dados GFS (Magna) ----
# NOTE: Alternativamente poderia se utilizar o script feito para isso...
gfs_df <- read_csv("datasets/gfs_emulated_metar_raw2.csv", show_col_types = FALSE) %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    vel_vento_gfs = vel_vento * 1.94384, # m/s para nos
    temp_ar_gfs = temp_ar,
    temp_orvalho_gfs = temp_orvalho,
    pressao_gfs = pressao,
    umidade_relativa_gfs = umidade_relativa * 100
  ) %>% select(datetime, ends_with("_gfs"))

# 2. Carregar Dados WRF (Magna d04) ----
wrf_df <- read_csv("datasets/wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    temp_ar_wrf = temperature_htgl - 273.15,
    temp_orvalho_wrf = dew_point_temperature_htgl,
    pressao_wrf = pressure / 100,
    vel_vento_wrf = wind_speed * 1.94384,
    umidade_relativa_wrf = relative_humidity_htgl
  ) %>% select(datetime, ends_with("_wrf"))

# 3. Carregar METAR
metar_raw <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE)

calc_rh_magnus <- function(temp_c, dew_c) {
  es <- 6.112 * exp((17.67 * temp_c) / (temp_c + 243.5))
  e  <- 6.112 * exp((17.67 * dew_c) / (dew_c + 243.5))
  pmin(pmax(e / es, 0), 1) * 100
}

metar_df <- metar_raw %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    temp_ar_obs = temperature,
    temp_orvalho_obs = dew_point,
    pressao_obs = pressure,
    vel_vento_obs = wind_speed,
    umidade_relativa_obs = calc_rh_magnus(temperature, dew_point)
  ) %>% select(datetime, ends_with("_obs")) %>%
  filter(datetime >= min(wrf_df$datetime) & datetime <= max(wrf_df$datetime))

# Mesclar tudo
comp_df <- metar_df %>%
  inner_join(gfs_df, by = "datetime") %>%
  inner_join(wrf_df, by = "datetime")

vars <- c("temp_ar", "temp_orvalho", "umidade_relativa", "pressao", "vel_vento")
results <- list()

calc_r2 <- function(pred, obs) {
  valid <- !is.na(pred) & !is.na(obs)
  if(sum(valid) == 0) return(NA)
  pred <- pred[valid]
  obs <- obs[valid]
  1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
}

for (v in vars) {
  obs <- comp_df[[paste0(v, "_obs")]]
  gfs <- comp_df[[paste0(v, "_gfs")]]
  wrf <- comp_df[[paste0(v, "_wrf")]]
  
  valid_gfs <- !is.na(obs) & !is.na(gfs)
  valid_wrf <- !is.na(obs) & !is.na(wrf)
  
  res <- data.frame(
    Variable = v,
    # GFS
    GFS_RMSE = ifelse(sum(valid_gfs)>0, sqrt(mean((gfs[valid_gfs] - obs[valid_gfs])^2)), NA),
    GFS_Bias = ifelse(sum(valid_gfs)>0, mean(gfs[valid_gfs] - obs[valid_gfs]), NA),
    GFS_Pearson = ifelse(sum(valid_gfs)>0, cor(gfs[valid_gfs], obs[valid_gfs]), NA),
    GFS_R2 = calc_r2(gfs, obs),
    
    # WRF
    WRF_RMSE = ifelse(sum(valid_wrf)>0, sqrt(mean((wrf[valid_wrf] - obs[valid_wrf])^2)), NA),
    WRF_Bias = ifelse(sum(valid_wrf)>0, mean(wrf[valid_wrf] - obs[valid_wrf]), NA),
    WRF_Pearson = ifelse(sum(valid_wrf)>0, cor(wrf[valid_wrf], obs[valid_wrf]), NA),
    WRF_R2 = calc_r2(wrf, obs)
  )
  results[[v]] <- res
}

# 4. Finalização ----
final_df <- bind_rows(results)
write.csv2(final_df, "resources/metricas_gfs_vs_wrf.csv", row.names = FALSE, )
cat("Métricas Magna Simulation salvas em resources/metricas_gfs_vs_wrf.csv\n")
