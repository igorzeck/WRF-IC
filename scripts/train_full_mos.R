# Script para treinar o WRF MOS usando todas as variáveis da física do WRF

library(tidyverse)
library(lightgbm)

set.seed(42)

# 1. Carregar os Dados
df_wrf <- read_csv("datasets/wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

metar_obs <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  select(datetime, obs_vis = visibility)

# 2. Limpeza do WRF (remover colunas 100% NA)
na_cols <- c("geopotential_height_0deg", "geopotential_height_ceil", 
             "geopotential_height_htfl", "parcel_lifted_index_to_500_h_pa", 
             "probability_of_precipitation", "surface_lifted_index", "surface_roughness")
df_wrf <- df_wrf %>% select(-any_of(na_cols))

# 3. Definir as Features
feats_base <- setdiff(names(df_wrf), "datetime")

# 4. Criar Lags (Defasagens) de T-3h e T-6h para TODAS as variáveis
df_wrf_lagged <- df_wrf %>% arrange(datetime)
for (f in feats_base) {
  df_wrf_lagged[[paste0(f, "_lag3")]] <- dplyr::lag(df_wrf[[f]], 3)
  df_wrf_lagged[[paste0(f, "_lag6")]] <- dplyr::lag(df_wrf[[f]], 6)
}

feats_lagged <- setdiff(names(df_wrf_lagged), "datetime")

# 5. Juntar com as observações do METAR (Nosso alvo - Target)
df_eval <- df_wrf_lagged %>%
  inner_join(metar_obs, by = "datetime") %>%
  filter(is.finite(obs_vis)) %>%
  drop_na() # Dropa as primeiras 6h para alinhar todos os Lags e qualquer eventual NA perdido

# 6. Treinar o MOS Base (Somente variáveis instantâneas de Física)
cat("\n--- Treinando WRF MOS Full Physics (Base) ---\n")
X_train_base <- as.matrix(df_eval[, feats_base])
y_train <- df_eval$obs_vis

dtrain_base <- lgb.Dataset(data = X_train_base, label = y_train)

# Hiperparâmetros bem regularizados para evitar Overfitting (pois temos muitos preditores e poucas linhas)
params <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 15,         # Árvore mais rasa para não decorar a base pequena
  min_data_in_leaf = 10,   # Mais restrição
  verbose = -1
)

cv_base <- lgb.cv(
  params = params, data = dtrain_base, nfold = 5, nrounds = 300, 
  early_stopping_rounds = 20, verbose = -1
)
cat("WRF MOS Full Base - 5-Fold CV RMSE:", cv_base$best_score, "\n")
model_base <- lgb.train(params = params, data = dtrain_base, nrounds = cv_base$best_iter)
saveRDS(model_base, "models/wrf_full_mos_base.rds")

# 7. Treinar o MOS Lagged (Variáveis + Evolução Temporal)
cat("\n--- Treinando WRF MOS Full Physics (Lagged) ---\n")
X_train_lagged <- as.matrix(df_eval[, feats_lagged])

dtrain_lagged <- lgb.Dataset(data = X_train_lagged, label = y_train)
cv_lagged <- lgb.cv(
  params = params, data = dtrain_lagged, nfold = 5, nrounds = 300, 
  early_stopping_rounds = 20, verbose = -1
)
cat("WRF MOS Full Lagged - 5-Fold CV RMSE:", cv_lagged$best_score, "\n")
model_lagged <- lgb.train(params = params, data = dtrain_lagged, nrounds = cv_lagged$best_iter)
saveRDS(model_lagged, "models/wrf_full_mos_lagged.rds")

# 8. Extrair Importância
imp <- lgb.importance(model_lagged)

sink("models/raw_wrf_full_results.txt")
cat("--- WRF MOS Full Physics (25 Variáveis WRF x METAR) ---\n")
cat("MOS Base CV RMSE:", cv_base$best_score, "\n")
cat("MOS Lagged CV RMSE:", cv_lagged$best_score, "\n\n")
cat("Top 20 Features (Modelo Lagged):\n")
print(head(imp, 20))
sink()

cat("Concluído! Modelos salvos.\n")
