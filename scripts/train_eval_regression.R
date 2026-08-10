# Script de treino de para o modelo LightGBM e RF e comparar suas métricas

# 0. Setup ----
library(tidyverse)
library(caret)
library(lightgbm)
library(ranger)
library(doParallel)

set.seed(42)

# Ativação de paralelismo
num_cores <- parallel::detectCores() - 1
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

# 1. Carregamento de dados -----
dataset_path <- "datasets/metar_SBGL_2011_2025_lmlt.csv"
df_raw <- read_csv(dataset_path, show_col_types = FALSE)

df_clean <- df_raw %>%
  filter(is.finite(vis)) %>%
  select(-datetime)

categ_nuvem_levels <- readRDS("models/factor_levels.rds")$categ_nuvem

df_clean <- df_clean %>%
  mutate(
    categ_nuvem = as.integer(factor(categ_nuvem, levels = categ_nuvem_levels))
  )

feats <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa")

# Split de treino e teste (80/20)
train_idx <- createDataPartition(df_clean$vis, p = 0.8, list = FALSE)
train_data <- df_clean[train_idx, ]
test_data <- df_clean[-train_idx, ]

X_train <- as.matrix(train_data[, feats])
y_train <- train_data$vis
X_test <- as.matrix(test_data[, feats])
y_test <- test_data$vis

# 2. LightGBM ----
dtrain <- lgb.Dataset(data = X_train, label = y_train)
params_lgb <- list(objective = "regression", metric = "rmse", learning_rate = 0.1, num_leaves = 63, num_threads = num_cores)
lgb_model <- lgb.train(params = params_lgb, data = dtrain, nrounds = 200)

preds_lgb <- predict(lgb_model, X_test)

# 3. RF ----
rf_model <- ranger(
  x = train_data[, feats],
  y = y_train,
  num.trees = 100,
  mtry = 3,
  num.threads = num_cores,
  importance = "impurity"
)
preds_rf <- predict(rf_model, test_data[, feats])$predictions

# 4. Métricas ----
calc_metrics <- function(preds, obs) {
  rmse <- sqrt(mean((preds - obs)^2))
  mae <- mean(abs(preds - obs))
  bias <- mean(preds - obs)
  r2 <- 1 - sum((obs - preds)^2) / sum((obs - mean(obs))^2)
  pearson <- cor(preds, obs)
  
  data.frame(RMSE = rmse, MAE = mae, Bias = bias, R2_SkillScore = r2, Pearson = pearson)
}

# 5. Finalização -----
metrics_lgb <- calc_metrics(preds_lgb, y_test)
metrics_rf <- calc_metrics(preds_rf, y_test)

results <- bind_rows(
  metrics_lgb %>% mutate(Model = "LightGBM Regressão"),
  metrics_rf %>% mutate(Model = "Random Forest Regressão")
)

write_csv(results, "models/regression_metrics_comparison.csv")
print(results)

saveRDS(lgb_model, "models/lightgbm_regression_split.rds")
saveRDS(rf_model, "models/rf_regression_split.rds")

stopCluster(cl)
registerDoSEQ()
message("Regressão completa")
