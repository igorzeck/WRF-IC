# Script para reverter a adição na classificação e adicionar nas métricas de regressão
library(tidyverse)
library(lubridate)
library(lightgbm)

# 1. Reverter classificação
csv_class <- "resources/metricas_classificacao_modelos.csv"
if (file.exists(csv_class)) {
  df_c <- read_csv(csv_class, show_col_types = FALSE)
  df_c <- df_c %>% filter(Model != "fog_autoregressive_24h_avg")
  write_csv(df_c, csv_class)
}

# 2. Calcular previsões do AR
metar_raw <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  arrange(datetime) %>%
  mutate(
    temp_ar = temperature, pressao = pressure, vel_vento = wind_speed,
    dir_vento = wind_direction, temp_orvalho = dew_point,
    umidade_relativa = exp((17.625 * temp_orvalho) / (243.04 + temp_orvalho)) / exp((17.625 * temp_ar) / (243.04 + temp_ar)),
    vis = visibility
  )

lags <- c(0, 1, 2, 3, 6, 12, 18, 23)
vars_to_lag <- c("vis", "temp_ar", "umidade_relativa", "pressao", "vel_vento")
df_test <- metar_raw
for (v in vars_to_lag) {
  for (l in lags) {
    df_test[[paste0(v, "_lag", l)]] <- dplyr::lag(df_test[[v]], l)
  }
}

factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem
df_test <- df_test %>% mutate(categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)))
feats_ar <- readRDS("models/ar_feats.rds")
ar_models <- readRDS("models/ar_models_24h.rds")

init_times <- as.POSIXct(paste0("2026-06-", 22:27, " 00:00:00"), tz="UTC")
ar_preds <- data.frame(datetime=as.POSIXct(character()), ar_pred=numeric())

for (init_t in init_times) {
  init_t <- as.POSIXct(init_t, origin="1970-01-01", tz="UTC")
  row_T0 <- df_test %>% filter(datetime == init_t)
  if(nrow(row_T0) == 0) next
  X_T0 <- as.matrix(row_T0[, feats_ar])
  for (h in 1:24) {
    pred_val <- predict(ar_models[[paste0("lead", h)]], X_T0)
    ar_preds <- bind_rows(ar_preds, data.frame(datetime=init_t + hours(h), ar_pred=pred_val))
  }
}
ar_preds <- ar_preds %>% group_by(datetime) %>% summarise(ar_pred = mean(ar_pred)) %>% ungroup()

# 3. Calcular e salvar métricas de regressão
df_eval <- metar_raw %>%
  select(datetime, obs_vis = visibility) %>%
  inner_join(ar_preds, by="datetime") %>%
  filter(is.finite(obs_vis))

obs <- df_eval$obs_vis
pred <- df_eval$ar_pred

rmse_val <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
mae_val <- mean(abs(obs - pred), na.rm = TRUE)
bias_val <- mean(pred - obs, na.rm = TRUE)
pearson_val <- cor(obs, pred)
r2_val <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

res <- data.frame(
  Model = "autoregressive_24h_avg",
  N_Valid_Samples = length(obs),
  RMSE = rmse_val,
  MAE = mae_val,
  Bias = bias_val,
  Pearson = pearson_val,
  R2 = r2_val
)

csv_vis <- "resources/metricas_vis_modelos.csv"
exist_df <- read_csv(csv_vis, show_col_types = FALSE) %>%
  filter(Model != "autoregressive_24h_avg")

final_df <- bind_rows(exist_df, res)
write_csv(final_df, csv_vis)

print(res)
cat("Concluído!\n")
