# Script para treinar e avaliar o Perfect Prog Lagged (LightGBM)
# Treino: Histórico METAR (2011-2025) + lags de observações
# Teste: WRF MOS Lagged features (22-28 Jun 2026)

library(tidyverse)
library(caret)
library(lightgbm)

set.seed(42)

# 1. Carregar Dados Históricos
cat("Carregando e processando histórico METAR (2011-2025)...\n")
df_raw <- read_csv("datasets/metar_SBGL_2011_2025_lmlt.csv", show_col_types = FALSE)

# Gerar variáveis defasadas (lags)
df_hist <- df_raw %>%
  arrange(datetime) %>%
  mutate(
    temp_ar_lag3 = dplyr::lag(temp_ar, 3),
    temp_ar_lag6 = dplyr::lag(temp_ar, 6),
    umidade_relativa_lag3 = dplyr::lag(umidade_relativa, 3),
    umidade_relativa_lag6 = dplyr::lag(umidade_relativa, 6),
    pressao_lag3 = dplyr::lag(pressao, 3),
    pressao_lag6 = dplyr::lag(pressao, 6)
  ) %>%
  filter(is.finite(vis)) %>%
  drop_na(temp_ar_lag6) # Remove as primeiras horas com NA devido aos lags

factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem

df_hist <- df_hist %>%
  mutate(categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)))

feats_lagged <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", 
                  "categ_nuvem", "lmlt", "umidade_relativa",
                  "temp_ar_lag3", "temp_ar_lag6", 
                  "umidade_relativa_lag3", "umidade_relativa_lag6", 
                  "pressao_lag3", "pressao_lag6")

X_train <- as.matrix(df_hist[, feats_lagged])
y_train <- df_hist$vis

# 2. Treinar Modelo LightGBM (Perfect Prog Lagged)
cat("Treinando LightGBM Perfect Prog Lagged (CV 5-fold)...\n")
dtrain <- lgb.Dataset(data = X_train, label = y_train)

params <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.1,
  num_leaves = 63,
  num_threads = parallel::detectCores() - 1
)

cv_lgb <- lgb.cv(
  params = params,
  data = dtrain,
  nrounds = 300,
  nfold = 5,
  early_stopping_rounds = 20,
  verbose = -1
)

cat("Perfect Prog Lagged - 5-Fold CV RMSE:", cv_lgb$best_score, "\n")

model_pp_lagged <- lgb.train(params = params, data = dtrain, nrounds = cv_lgb$best_iter)
saveRDS(model_pp_lagged, "models/lightgbm_pp_lagged.rds")

# 3. Avaliar no WRF (O "Domínio" Alvo)
cat("\nAvaliando previsões usando saídas defasadas do WRF...\n")

# Carregar WRF com Lags
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

# Previsões Perfect Prog Lagged
X_test <- as.matrix(wrf_mos_raw[, feats_lagged])
wrf_mos_raw$pp_lagged_vis <- predict(model_pp_lagged, X_test)

# Comparar com METAR (Ouro) e MOS Lagged original
metar_obs <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  filter(datetime >= as.POSIXct("2026-06-22 00:00:00", tz = "UTC") &
         datetime <= as.POSIXct("2026-06-28 00:00:00", tz = "UTC")) %>%
  select(datetime, obs_vis = visibility)

# Modelos anteriores
mos_lagged_model <- readRDS("models/wrf_lagged_regression.rds")
wrf_mos_raw$mos_lagged_vis <- predict(mos_lagged_model, X_test)
lgb_pp_base <- readRDS("models/lightgbm_metar_historical_regression.rds")
wrf_mos_raw$pp_base_vis <- predict(lgb_pp_base, as.matrix(wrf_mos_raw[, feats_lagged[1:8]]))


df_eval <- metar_obs %>%
  inner_join(wrf_mos_raw, by = "datetime") %>%
  filter(is.finite(obs_vis) & !is.na(temp_ar_lag6))

obs <- df_eval$obs_vis

calc_metrics <- function(pred, name) {
  rmse_val <- sqrt(mean((obs - pred)^2, na.rm = TRUE))
  mae_val <- mean(abs(obs - pred), na.rm = TRUE)
  bias_val <- mean(pred - obs, na.rm = TRUE)
  r2_val <- cor(obs, pred)^2
  
  cat(sprintf("\n%s\n", name))
  cat(sprintf(" RMSE: %.2f m\n", rmse_val))
  cat(sprintf(" MAE : %.2f m\n", mae_val))
  cat(sprintf(" Bias: %.2f m\n", bias_val))
  cat(sprintf(" R²  : %.4f\n", r2_val))
}

calc_metrics(df_eval$pp_base_vis, "Perfect Prog (Base - Sem Lags)")
calc_metrics(df_eval$pp_lagged_vis, "Perfect Prog Lagged (Testando a Hipótese)")
calc_metrics(df_eval$mos_lagged_vis, "WRF MOS Lagged (Treinado no WRF)")

cat("\nScript Finalizado.\n")
