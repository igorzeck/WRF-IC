# Script para testar AR 24h vs MOS
library(tidyverse)
library(lubridate)
library(lightgbm)

set.seed(42)

cat("Carregando METAR de teste...\n")
metar_raw <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  arrange(datetime) %>%
  mutate(
    temp_ar = temperature,
    pressao = pressure,
    vel_vento = wind_speed,
    dir_vento = wind_direction,
    temp_orvalho = dew_point,
    umidade_relativa = exp((17.625 * temp_orvalho) / (243.04 + temp_orvalho)) / exp((17.625 * temp_ar) / (243.04 + temp_ar)),
    vis = visibility
  )

# Gerar features com base nas observações
lags <- c(0, 1, 2, 3, 6, 12, 18, 23)
vars_to_lag <- c("vis", "temp_ar", "umidade_relativa", "pressao", "vel_vento")

df_test <- metar_raw
for (v in vars_to_lag) {
  for (l in lags) {
    col_name <- paste0(v, "_lag", l)
    df_test[[col_name]] <- dplyr::lag(df_test[[v]], l)
  }
}

factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem
df_test <- df_test %>%
  mutate(categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)))

feats_ar <- readRDS("models/ar_feats.rds")
ar_models <- readRDS("models/ar_models_24h.rds")

# Filtramos os instantes em que faremos a previsão (00:00Z de cada dia de 22 a 27 de Junho)
# Note: no dia 22 00:00Z, o modelo prevê até 23 00:00Z.
init_times <- as.POSIXct(paste0("2026-06-", 22:27, " 00:00:00"), tz="UTC")

ar_preds <- data.frame(datetime=as.POSIXct(character()), ar_pred=numeric(), lead_time=numeric())

for (init_t in init_times) {
  init_t <- as.POSIXct(init_t, origin="1970-01-01", tz="UTC")
  
  # Pegamos a linha de T=00:00Z
  row_T0 <- df_test %>% filter(datetime == init_t)
  if(nrow(row_T0) == 0) next
  
  X_T0 <- as.matrix(row_T0[, feats_ar])
  
  # Fazemos a previsão para os 24 leads
  for (h in 1:24) {
    model_h <- ar_models[[paste0("lead", h)]]
    pred_val <- predict(model_h, X_T0)
    
    target_dt <- init_t + hours(h)
    ar_preds <- bind_rows(ar_preds, data.frame(datetime=target_dt, ar_pred=pred_val, lead_time=h))
  }
}

# Remover possiveis duplicatas (ex: o lead24 de um dia cai no lead0 do outro, mas só prevemos de 1 a 24, então o 24 cai em 00:00Z do outro dia).
ar_preds <- ar_preds %>% group_by(datetime) %>% summarise(ar_pred = mean(ar_pred), lead_time = mean(lead_time)) %>% ungroup()

# Agora carrega as previsões do WRF MOS
wrf_mos_raw <- read_csv("datasets/wrf_emulated_wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

mos_model_lagged <- readRDS("models/wrf_lagged_regression.rds")
feats_lagged <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa", "temp_ar_lag3", "temp_ar_lag6", "umidade_relativa_lag3", "umidade_relativa_lag6", "pressao_lag3", "pressao_lag6")

wrf_mos_raw <- wrf_mos_raw %>%
  arrange(datetime) %>%
  mutate(
    categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)),
    temp_ar_lag3 = dplyr::lag(temp_ar, 3),
    temp_ar_lag6 = dplyr::lag(temp_ar, 6),
    umidade_relativa_lag3 = dplyr::lag(umidade_relativa, 3),
    umidade_relativa_lag6 = dplyr::lag(umidade_relativa, 6),
    pressao_lag3 = dplyr::lag(pressao, 3),
    pressao_lag6 = dplyr::lag(pressao, 6)
  )

X_mat_mos <- as.matrix(wrf_mos_raw[, feats_lagged])
wrf_mos_raw$mos_pred <- predict(mos_model_lagged, X_mat_mos)

df_eval <- metar_raw %>% select(datetime, obs_vis=visibility) %>%
  inner_join(ar_preds, by="datetime") %>%
  inner_join(wrf_mos_raw %>% select(datetime, mos_pred), by="datetime") %>%
  mutate(
    AR_AE = abs(ar_pred - obs_vis),
    MOS_AE = abs(mos_pred - obs_vis)
  ) %>% filter(is.finite(obs_vis))

# Calcula MAE por Lead Time (1 a 24)
mae_by_lead <- df_eval %>%
  group_by(lead_time) %>%
  summarise(
    AR_MAE = mean(AR_AE, na.rm=TRUE),
    MOS_MAE = mean(MOS_AE, na.rm=TRUE), # media do MOS para aquele lead time
    AR_RMSE = sqrt(mean((ar_pred - obs_vis)^2, na.rm=TRUE)),
    MOS_RMSE = sqrt(mean((mos_pred - obs_vis)^2, na.rm=TRUE))
  )

write_csv(mae_by_lead, "resources/mae_by_lead_ar_vs_mos.csv")

# Plota RMSE e MAE em funcao do lead time
p_mae <- ggplot(mae_by_lead, aes(x=lead_time)) +
  geom_line(aes(y=AR_MAE, color="Autoregressivo (AR)"), size=1.2) +
  geom_point(aes(y=AR_MAE, color="Autoregressivo (AR)"), size=2) +
  geom_line(aes(y=MOS_MAE, color="WRF MOS Lagged"), size=1.2) +
  geom_point(aes(y=MOS_MAE, color="WRF MOS Lagged"), size=2) +
  scale_color_manual(values=c("Autoregressivo (AR)"="red", "WRF MOS Lagged"="blue")) +
  theme_minimal(base_size=14)

ggsave("resources/mae_lead_time_ar_vs_mos.png", plot=p_mae, width=10, height=6, bg="white")

p_ts <- ggplot(df_eval, aes(x=datetime)) +
  geom_line(aes(y=obs_vis, color="METAR Real"), alpha=0.5, size=1) +
  geom_line(aes(y=ar_pred, color="AR Forecast (24h)"), alpha=0.8, size=1) +
  geom_line(aes(y=mos_pred, color="WRF MOS Lagged (144h)"), alpha=0.8, size=1) +
  scale_color_manual(values=c("METAR Real"="black", "AR Forecast (24h)"="red", "WRF MOS Lagged (144h)"="blue")) +
  theme_minimal(base_size=14)

ggsave("resources/ts_ar_vs_mos.png", plot=p_ts, width=12, height=6, bg="white")

cat("Métricas e Gráficos gerados com sucesso!\n")
