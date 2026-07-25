# Script para o treino de um modelo WRF com output do WRF Manager de períodos diversos ----
# Target: metar_vis_m (visibilidade em metros de metar_vis_km)

## 1. Setup & Pacotes ----
library(tidyverse)
library(caret)
library(ranger)
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

## 4. Treino ----
# Com cross-validation
cv_control <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE
)

tuning_grid <- expand.grid(
  mtry = c(2, 5, 10, 15),
  splitrule = "variance",
  min.node.size = c(5, 10)
)

message("Treinando modelo RF...")
rf_model <- train(
  metar_vis_m ~ .,
  data = train_data,
  method = "ranger",
  trControl = cv_control,
  tuneGrid = tuning_grid,
  importance = "permutation"  # para importância de variáveis
)

## 5. Avaliação ----
message("Avaliando modelo...")
predictions <- predict(rf_model, newdata = test_data)

test_metrics <- postResample(pred = predictions, obs = test_data$metar_vis_m)
print(test_metrics)

# Performance
cat("\n--- Performance no set de testes ---\n")
cat(sprintf("RMSE: %.2f m\n", test_metrics["RMSE"]))
cat(sprintf("R2: %.4f\n", test_metrics["Rsquared"]))
cat(sprintf("MAE: %.2f m\n", test_metrics["MAE"]))

## 6. Salva modelo ----
model_path <- "models/rf_wrf_model.rds"
saveRDS(rf_model, file = model_path)
message("Modelo salvo como: ", model_path)

## 7. Limpeza ----
stopCluster(cl)
registerDoSEQ()
message("Treino finalizado.")
