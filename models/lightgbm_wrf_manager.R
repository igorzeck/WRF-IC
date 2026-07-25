# Script para o treino de um modelo LightGBM com output do WRF Manager de períodos diversos ----
# Target: metar_vis_m (visibilidade em metros de metar_vis_km)

## 1. Setup & Pacotes ----
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

## 2. Carregamento e pré-processamento ----
dataset_path <- "datasets/wrf_manager/wrf_ml_dataset_completo_SBGL.csv"
df_raw <- read_csv(dataset_path, show_col_types = FALSE)

# Preprocessamento:
df_clean <- df_raw %>%
  filter(!is.na(metar_vis_km)) %>%
  mutate(
    metar_vis_m = metar_vis_km * 1000,
    wrf_vis_raw_m = wrf_vis_raw_km * 1000,
    wrf_vis_norm_m = wrf_vis_norm_km * 1000
  ) %>%
  select(-metar_vis_km, -wrf_vis_raw_km, -wrf_vis_norm_km) %>%
  select(-time_utc, -time_local_sp, -run_id, -run_start) %>%
  select(where(~length(unique(.)) > 1))

## 3. Data Splitting ----
# Split 80/20
train_index <- createDataPartition(df_clean$metar_vis_m, p = 0.8, list = FALSE)
train_data  <- df_clean[train_index, ]
test_data   <- df_clean[-train_index, ]

# O LightGBM exige matrizes numéricas para as features (X) e um vetor numérico para o target (y)
X_train <- as.matrix(train_data %>% select(-metar_vis_m))
y_train <- train_data$metar_vis_m
X_test  <- as.matrix(test_data %>% select(-metar_vis_m))
y_test  <- test_data$metar_vis_m

dtrain <- lgb.Dataset(data = X_train, label = y_train)

## 4. Treino ----
# Busca por hiperparâmetros (Grid Search manual com validação cruzada 10-fold)
tuning_grid <- expand.grid(
  learning_rate = c(0.01, 0.05, 0.1),
  num_leaves = c(15, 31),
  min_data_in_leaf = c(5, 10, 20)
)

best_rmse <- Inf
best_params <- NULL
best_nrounds <- 100

start_time <- Sys.time()
message("Tuning dos hiperparâmetros do LightGBM...")
for (i in 1:nrow(tuning_grid)) {
  params <- list(
    objective = "regression",
    metric = "rmse",
    learning_rate = tuning_grid$learning_rate[i],
    num_leaves = tuning_grid$num_leaves[i],
    min_data_in_leaf = tuning_grid$min_data_in_leaf[i],
    feature_pre_filter = FALSE,
    num_threads = num_cores
  )
  
  # Cross-validation
  cv_results <- lgb.cv(
    params = params,
    data = dtrain,
    nrounds = 500,
    nfold = 10,
    early_stopping_rounds = 30,
    verbose = -1
  )
  
  cv_rmse <- cv_results$best_score
  
  if (cv_rmse < best_rmse) {
    best_rmse <- cv_rmse
    best_params <- params
    best_nrounds <- cv_results$best_iter
  }
}

message("Melhor CV RMSE: ", best_rmse)
message("Melhor número de rodadas: ", best_nrounds)

# Treinamento do modelo final com os melhores hiperparâmetros encontrados
message("Treinando modelo LightGBM final...")
lgb_model <- lgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_nrounds
)
end_time <- Sys.time()
train_time <- end_time - start_time

## 5. Avaliação ----
message("Avaliando modelo...")
predictions <- predict(lgb_model, X_test)

test_metrics <- postResample(pred = predictions, obs = y_test)
print(test_metrics)

# Performance
cat("\n--- Performance no set de testes ---\n")
cat(sprintf("RMSE: %.2f m\n", test_metrics["RMSE"]))
cat(sprintf("R2: %.4f\n", test_metrics["Rsquared"]))
cat(sprintf("MAE: %.2f m\n", test_metrics["MAE"]))
cat(sprintf("Tempo de treinamento: %.2f segundos\n", as.numeric(train_time, units = "secs")))

## 6. Salva modelo ----
model_path <- "models/lightgbm_wrf_model.rds"
saveRDS(lgb_model, file = model_path)
message("Modelo salvo como: ", model_path)

## 7. Limpeza ----
stopCluster(cl)
registerDoSEQ()
message("Treino finalizado.")
