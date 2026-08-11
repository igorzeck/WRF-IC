# Script para gerar grafico de erros absolutos comparando
# Perfect Prog (LightGBM e Random Forest) vs Koschmieder (Física)

library(tidyverse)
library(lubridate)
library(ncdf4)
library(lightgbm)
library(ranger)

set.seed(42)

# 1. Carregar METAR
metar_raw <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

metar_obs <- metar_raw %>%
  filter(datetime >= as.POSIXct("2026-06-22 00:00:00", tz = "UTC") &
         datetime <= as.POSIXct("2026-06-28 00:00:00", tz = "UTC")) %>%
  mutate(obs_vis = visibility) %>%
  select(datetime, obs_vis)

# 2. Extrair WRF (Koschmieder Clássico com QCLOUD)
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

# 3. Modelos Perfect Prog
lgb_model <- readRDS("models/lightgbm_metar_historical_regression.rds")
rf_model <- readRDS("models/rf_regression_split.rds")
factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem
feats <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa")

wrf_mos_raw <- read_csv("datasets/wrf_emulated_wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels))
  )

X_mat <- as.matrix(wrf_mos_raw[, feats])
wrf_mos_raw$preds_lgb <- predict(lgb_model, X_mat)
wrf_mos_raw$preds_rf <- predict(rf_model, wrf_mos_raw[, feats])$predictions

wrf_mos_df <- wrf_mos_raw %>% select(datetime, preds_lgb, preds_rf)

# 4. Avaliar Erros
df_eval <- metar_obs %>%
  inner_join(wrf_df, by = "datetime") %>%
  inner_join(wrf_mos_df, by = "datetime") %>%
  mutate(
    hour = hour(datetime)
  ) %>%
  filter(hour %% 3 == 0) %>%
  mutate(
    WRF_Koschmieder_AE = abs(wrf_koschmieder - obs_vis),
    LGB_PerfectProg_AE = abs(preds_lgb - obs_vis),
    RF_PerfectProg_AE = abs(preds_rf - obs_vis)
  )

baseline_mae <- mean(abs(mean(df_eval$obs_vis) - df_eval$obs_vis))

plot_df <- df_eval %>%
  select(datetime, WRF_Koschmieder_AE, LGB_PerfectProg_AE, RF_PerfectProg_AE) %>%
  pivot_longer(cols = -datetime, names_to = "Method", values_to = "Absolute_Error")

# 5. Gerar Gráfico
p <- ggplot(plot_df, aes(x = datetime, y = Absolute_Error, color = Method)) +
  geom_hline(aes(yintercept = baseline_mae, linetype = "Média da Visibilidade"), color = "gray50", size = 1) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(alpha = 0.5) +
  scale_color_manual(
    values = c(
      "WRF_Koschmieder_AE" = "red",
      "LGB_PerfectProg_AE" = "cornflowerblue",
      "RF_PerfectProg_AE"  = "darkorchid"
    ),
    labels = c(
      "WRF_Koschmieder_AE" = "WRF Koschmieder",
      "LGB_PerfectProg_AE" = "Perfect Prog (LightGBM 15Y)",
      "RF_PerfectProg_AE"  = "Perfect Prog (Random Forest 15Y)"
    )
  ) +
  scale_linetype_manual(name = "", values = c("Média da Visibilidade" = "dashed")) +
  scale_y_continuous(labels = scales::comma_format(suffix = " m")) +
  labs(
    title = "Erro Absoluto: Formulação Física vs Perfect Prog (Histórico 15 Anos)",
    subtitle = "WRF a cada 3h (22 a 28 de Junho)",
    x = "Data",
    y = "Erro Absoluto (m)",
    color = "Método"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

img_path <- "resources/perfect_prog_vis_error_3h.png"
ggsave(img_path, plot = p, width = 11, height = 6, dpi = 300)

message("Gráfico salvo em: ", img_path)
