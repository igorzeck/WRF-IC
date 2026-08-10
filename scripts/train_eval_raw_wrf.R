# Utilização de modelo com dados brutos dos WRf para 5 dias
# NOTE: usa time-lagged variables (defasado)

# 0. Setup ----
library(tidyverse)
library(caret)
library(lightgbm)

set.seed(42)

df_wrf <- read_csv("datasets/wrf_emulated_wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

metar_obs <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  select(datetime, obs_vis = visibility)

# Criação de time-lagged
df_wrf_lagged <- df_wrf %>%
  arrange(datetime) %>%
  mutate(
    temp_ar_lag3 = dplyr::lag(temp_ar, 3),
    temp_ar_lag6 = dplyr::lag(temp_ar, 6),
    umidade_relativa_lag3 = dplyr::lag(umidade_relativa, 3),
    umidade_relativa_lag6 = dplyr::lag(umidade_relativa, 6),
    pressao_lag3 = dplyr::lag(pressao, 3),
    pressao_lag6 = dplyr::lag(pressao, 6)
  )

feats_base <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa")
feats_lagged <- c(feats_base, "temp_ar_lag3", "temp_ar_lag6", "umidade_relativa_lag3", "umidade_relativa_lag6", "pressao_lag3", "pressao_lag6")

df_eval <- df_wrf_lagged %>%
  inner_join(metar_obs, by = "datetime") %>%
  filter(is.finite(obs_vis)) %>%
  drop_na(all_of(c(feats_lagged, "obs_vis"))) # dropa as primeiras 6h e lida apenas com colunas usadas

factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem

df_eval <- df_eval %>%
  mutate(categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)))

# 1. Modelo base -----
cat("\n--- Modelo base (defasado) ---\n")
X_train_base <- as.matrix(df_eval[, feats_base])
y_train <- df_eval$obs_vis

dtrain_base <- lgb.Dataset(data = X_train_base, label = y_train)
params <- list(objective = "regression", metric = "rmse", learning_rate = 0.05, num_leaves = 15, verbose = -1)

cv_base <- lgb.cv(params = params, data = dtrain_base, nfold = 5, nrounds = 200, early_stopping_rounds=10, verbose = -1)
cat("Modelo base - 5-Fold CV RMSE:", cv_base$best_score, "\n")

model_base <- lgb.train(params = params, data = dtrain_base, nrounds = cv_base$best_iter)
saveRDS(model_base, "models/wrf_5day_regression.rds")

# 2. Time-lagged -----
cat("\n--- Modelo defasado (com Spin-up Lags) ---\n")
X_train_lagged <- as.matrix(df_eval[, feats_lagged])

dtrain_lagged <- lgb.Dataset(data = X_train_lagged, label = y_train)
cv_lagged <- lgb.cv(params = params, data = dtrain_lagged, nfold = 5, nrounds = 200, early_stopping_rounds=10, verbose = -1)
cat("Modelo defasado - 5-Fold CV RMSE:", cv_lagged$best_score, "\n")

model_lagged <- lgb.train(params = params, data = dtrain_lagged, nrounds = cv_lagged$best_iter)
saveRDS(model_lagged, "models/wrf_lagged_regression.rds")
imp <- lgb.importance(model_lagged)
print(imp)

# 3. Finalização ----
sink("models/raw_wrf_lagged_results.txt")
cat("--- Modelo base 5-dias WRF ---\n")
cat("Modelo base CV RMSE:", cv_base$best_score, "\n")
cat("Modelo defasado CV RMSE:", cv_lagged$best_score, "\n\n")
cat("Feature Importance (Modelo defasado):\n")
print(imp)
sink()

message("WRF bruto - com e sem defasagem- completo")
