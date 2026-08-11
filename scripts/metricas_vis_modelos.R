# Script para gerar métricas (RMSE, MAE, Bias, Pearson, R2) comparando todos os modelos
# de visibilidade avaliados (GFS Koschmieder, WRF Koschmieder, WRF MOS, WRF MOS Lagged)
# no período de 5 dias (22 a 28 de Junho de 2026)

library(tidyverse)
library(lubridate)
library(ncdf4)
library(lightgbm)

set.seed(42)

# 1. Carregar METAR (Observações)
metar_raw <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

metar_obs <- metar_raw %>%
  filter(datetime >= as.POSIXct("2026-06-22 00:00:00", tz = "UTC") &
         datetime <= as.POSIXct("2026-06-28 00:00:00", tz = "UTC")) %>%
  mutate(obs_vis = visibility) %>%
  select(datetime, obs_vis)

# 2. Carregar GFS e Calcular Koschmieder
gfs_df <- read_csv("datasets/gfs_emulated_metar_raw2.csv", show_col_types = FALSE) %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    beta_clean = 0.03912 / 1000,
    beta_rh = (0.03912 / 1000) * pmax(1, (1 / (1 - pmin(umidade_relativa, 0.99)))^1.2),
    gfs_koschmieder = 3.912 / (beta_clean + beta_rh)
  ) %>%
  select(datetime, gfs_koschmieder)

# 3. Extrair WRF (Koschmieder) via NetCDF
files <- list.files("/home/rf/WD/WRF/test/em_real", pattern = "^wrfout_d04_2026-06-2[2-8]", full.names = TRUE)
files <- sort(files)

records <- list()
for (f in files) {
  nc <- ncdf4::nc_open(f)
  
  lats <- ncdf4::ncvar_get(nc, "XLAT")
  lons <- ncdf4::ncvar_get(nc, "XLONG")
  if (length(dim(lats)) == 3) lats <- lats[,,1]
  if (length(dim(lons)) == 3) lons <- lons[,,1]
  
  target_lat <- -22.804944
  target_lon <- -43.256455
  dist_deg <- sqrt((lats - target_lat)^2 + (lons - target_lon)^2)
  min_idx <- which(dist_deg == min(dist_deg), arr.ind = TRUE)
  i <- min_idx[1, 1]
  j <- min_idx[1, 2]
  
  t2   <- ncdf4::ncvar_get(nc, "T2")[i, j] - 273.15
  psfc <- ncdf4::ncvar_get(nc, "PSFC")[i, j] / 100
  q2   <- ncdf4::ncvar_get(nc, "Q2")[i, j]
  e_hpa <- (psfc * q2) / (0.622 + 0.378 * q2)
  td   <- (243.5 * log(e_hpa / 6.112)) / (17.67 - log(e_hpa / 6.112))
  
  es <- 6.112 * exp((17.67 * t2) / (t2 + 243.5))
  e  <- 6.112 * exp((17.67 * td) / (td + 243.5))
  rh <- pmin(pmax(e / es, 0), 1)
  
  qcloud <- tryCatch(ncdf4::ncvar_get(nc, "QCLOUD")[i, j, 1], error = function(e) 0)
  rho <- (psfc * 100) / (287.058 * (t2 + 273.15))
  lwc <- qcloud * rho * 1000 # g/m3
  
  beta_clean <- 0.03912 / 1000
  beta_rh    <- 0.03912 / 1000 * pmax(1, (1 / (1 - min(rh, 0.99)))^1.2)
  beta_cloud <- ifelse(lwc > 0, 144.7 / 1000 * (lwc)^0.88, 0)
  
  beta_total <- beta_clean + beta_rh + beta_cloud
  wrf_koschmieder <- 3.912 / beta_total
  
  valores_t <- ncdf4::ncvar_get(nc, "XTIME")
  unid_t <- ncdf4::ncatt_get(nc, "XTIME", "units")$value
  t_ustr <- strsplit(unid_t, " ")
  t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])
  dt <- as.POSIXct(valores_t * 60, origin = t_origem, tz = "UTC")
  
  ncdf4::nc_close(nc)
  
  records[[f]] <- data.frame(datetime = dt, wrf_koschmieder = wrf_koschmieder)
}

wrf_df <- bind_rows(records)

# 4. WRF MOS e WRF MOS Lagged (Modelos LightGBM)
mos_model_base <- readRDS("models/wrf_5day_regression.rds")
mos_model_lagged <- readRDS("models/wrf_lagged_regression.rds")
factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem

feats_base <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa")
feats_lagged <- c(feats_base, "temp_ar_lag3", "temp_ar_lag6", "umidade_relativa_lag3", "umidade_relativa_lag6", "pressao_lag3", "pressao_lag6")

wrf_mos_raw <- read_csv("datasets/wrf_emulated_wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels))
  ) %>%
  arrange(datetime) %>%
  mutate(
    temp_ar_lag3 = dplyr::lag(temp_ar, 3),
    temp_ar_lag6 = dplyr::lag(temp_ar, 6),
    umidade_relativa_lag3 = dplyr::lag(umidade_relativa, 3),
    umidade_relativa_lag6 = dplyr::lag(umidade_relativa, 6),
    pressao_lag3 = dplyr::lag(pressao, 3),
    pressao_lag6 = dplyr::lag(pressao, 6)
  )

X_mat_base <- as.matrix(wrf_mos_raw[, feats_base])
wrf_mos_raw$wrf_mos_vis_base <- predict(mos_model_base, X_mat_base)

# Lagged features have NAs for the first 6 hours, predictions will be NA for them
X_mat_lagged <- as.matrix(wrf_mos_raw[, feats_lagged])
wrf_mos_raw$wrf_mos_vis_lagged <- predict(mos_model_lagged, X_mat_lagged)

wrf_mos_df <- wrf_mos_raw %>% select(datetime, wrf_mos_vis_base, wrf_mos_vis_lagged)

# 5. Avaliar Erros
df_eval <- metar_obs %>%
  inner_join(gfs_df, by = "datetime") %>%
  inner_join(wrf_df, by = "datetime") %>%
  inner_join(wrf_mos_df, by = "datetime") %>%
  filter(is.finite(obs_vis))

# 6. Calcular Métricas
models <- c("gfs_koschmieder", "wrf_koschmieder", "wrf_mos_vis_base", "wrf_mos_vis_lagged")
results <- list()

calc_r2 <- function(pred, obs) {
  valid <- !is.na(pred) & !is.na(obs)
  if(sum(valid) == 0) return(NA)
  pred <- pred[valid]
  obs <- obs[valid]
  1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
}

for (m in models) {
  obs <- df_eval$obs_vis
  pred <- df_eval[[m]]
  
  valid <- !is.na(obs) & !is.na(pred)
  
  obs <- obs[valid]
  pred <- pred[valid]
  
  res <- data.frame(
    Model = m,
    N_Valid_Samples = sum(valid),
    RMSE = sqrt(mean((pred - obs)^2)),
    MAE = mean(abs(pred - obs)),
    Bias = mean(pred - obs),
    Pearson = cor(pred, obs),
    R2 = calc_r2(pred, obs)
  )
  results[[m]] <- res
}

final_df <- bind_rows(results)
print(final_df)

write_csv(final_df, "resources/metricas_vis_modelos.csv")
cat("Métricas de Visibilidade salvas em resources/metricas_vis_modelos.csv\n")
