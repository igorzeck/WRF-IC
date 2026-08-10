# Script para treinar modelos LightGBM (Regressão e Classificação) em dados históricos METAR ----
# Setup ----
library(tidyverse)
library(caret)
library(lightgbm)
library(doParallel)

set.seed(42)

# Processamento paralelo
num_cores <- parallel::detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
message("N Cores: ", num_cores)

## 1. Carregamento & Limpeza ----
dataset_path <- "datasets/metar_SBGL_2011_2025_lmlt.csv"
df_raw <- read_csv(dataset_path, show_col_types = FALSE)

# Limpeza e filtro de visibilidade finita
df_clean <- df_raw %>%
  filter(is.finite(vis)) %>%
  select(-datetime)

# Definir níveis de fatores e salvar metadados para mapeamento no teste
categ_nuvem_levels <- c("No", "Few", "Scattered", "Broken", "Overcast")

# NOTE: esse modelo será utilizado de referência para categorização
#       precisa ser nessa ordem as categorias para comparações entre
#       outros modelos
saveRDS(
  list(categ_nuvem = categ_nuvem_levels), 
  "models/factor_levels.rds"
)

# Converter strings para inteiros com base nos fatores
df_clean <- df_clean %>%
  mutate(
    categ_nuvem = as.integer(factor(categ_nuvem, levels = categ_nuvem_levels))
  )

# Definir os recursos (excluindo clima e altura_nuvem devido à inconsistência/NAs no WRF)
feats <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa")

## 2. Treino do Modelo de Regressão ----
message("\n--- Treinando Modelo de Regressão ---")
X_reg <- as.matrix(df_clean[, feats])
y_reg <- df_clean$vis

dtrain_reg <- lgb.Dataset(data = X_reg, label = y_reg)

# Grid search rápido para regressão
grid_reg <- expand.grid(
  learning_rate = c(0.05, 0.1),
  num_leaves = c(31, 63)
)

best_rmse <- Inf
best_params_reg <- NULL
best_iter_reg <- 100

for (i in 1:nrow(grid_reg)) {
  params <- list(
    objective = "regression",
    metric = "rmse",
    learning_rate = grid_reg$learning_rate[i],
    num_leaves = grid_reg$num_leaves[i],
    num_threads = num_cores
  )
  
  cv <- lgb.cv(
    params = params,
    data = dtrain_reg,
    nrounds = 300,
    nfold = 5,
    early_stopping_rounds = 20,
    verbose = -1
  )
  
  if (cv$best_score < best_rmse) {
    best_rmse <- cv$best_score
    best_params_reg <- params
    best_iter_reg <- cv$best_iter
  }
}

message("Melhor Regressão CV RMSE: ", best_rmse)
reg_model <- lgb.train(params = best_params_reg, data = dtrain_reg, nrounds = best_iter_reg)
saveRDS(reg_model, "models/lightgbm_metar_historical_regression.rds")
message("Modelo de regressão salvo.")

## 3. Treino do Modelo de Classificação ----
message("\n--- Treinando Modelo de Classificação ---")
# Criar label de nevoeiro (visibilidade <= 1000 metros)
df_class <- df_clean %>%
  mutate(fog = ifelse(vis <= 1000, 1, 0)) %>%
  select(-vis)

# Resolver desbalanceamento extremo usando superamostragem
idx_pos <- which(df_class$fog == 1)
idx_neg <- which(df_class$fog == 0)

# Sobreamostrar positivos para igualar negativos (1:1)
oversampled_idx_pos <- sample(idx_pos, length(idx_neg), replace = TRUE)
balanced_idx <- c(idx_neg, oversampled_idx_pos)

X_class_bal <- as.matrix(df_class[balanced_idx, feats])
y_class_bal <- df_class$fog[balanced_idx]

dtrain_class <- lgb.Dataset(data = X_class_bal, label = y_class_bal)

# Hiperparâmetros base para classificação balanceada
params_class <- list(
  objective = "binary",
  metric = "binary_logloss",
  learning_rate = 0.05,
  num_leaves = 31,
  feature_pre_filter = FALSE,
  num_threads = num_cores
)

# Treinar com cross-validation rápida no dataset balanceado
cv_class <- lgb.cv(
  params = params_class,
  data = dtrain_class,
  nrounds = 300,
  nfold = 5,
  early_stopping_rounds = 20,
  verbose = -1
)

message("Melhor Classificação CV Logloss: ", cv_class$best_score)
class_model <- lgb.train(params = params_class, data = dtrain_class, nrounds = cv_class$best_iter)
saveRDS(class_model, "models/lightgbm_metar_historical_classification.rds")
message("Modelo de classificação salvo.")

## 4. Limpeza ----
stopCluster(cl)
registerDoSEQ()
message("Treinamento finalizado.")
