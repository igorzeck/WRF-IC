# Script para treinar 24 modelos Autoregressivos (1 a 24 horas no futuro)
# Utiliza apenas dados METAR do passado (dia anterior).

library(tidyverse)
library(lightgbm)

set.seed(42)

cat("Carregando METAR histórico...\n")
df_raw <- read_csv("datasets/metar_SBGL_2011_2025_lmlt.csv", show_col_types = FALSE)

# Organiza por data
df_hist <- df_raw %>%
  arrange(datetime) %>%
  filter(is.finite(vis))

# Cria features de atraso (Lags) referentes às últimas 24h
cat("Criando Lags (Features passadas)...\n")
lags <- c(0, 1, 2, 3, 6, 12, 18, 23)
vars_to_lag <- c("vis", "temp_ar", "umidade_relativa", "pressao", "vel_vento")

for (v in vars_to_lag) {
  for (l in lags) {
    col_name <- paste0(v, "_lag", l)
    df_hist[[col_name]] <- dplyr::lag(df_hist[[v]], l)
  }
}

# Cria variáveis alvo (Leads) para as próximas 24h
cat("Criando Leads (Targets futuros de 1h a 24h)...\n")
for (h in 1:24) {
  col_name <- paste0("target_vis_lead", h)
  df_hist[[col_name]] <- dplyr::lead(df_hist$vis, h)
}

# Remove NAs gerados pelos Lags e Leads extremos
df_hist <- df_hist %>% drop_na()

# Features preditoras
factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem
df_hist <- df_hist %>%
  mutate(categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)))

feats_ar <- c("categ_nuvem", "dir_vento")
for (v in vars_to_lag) {
  for (l in lags) {
    feats_ar <- c(feats_ar, paste0(v, "_lag", l))
  }
}

cat("Treinando 24 modelos...\n")
ar_models <- list()

params <- list(
  objective = "regression",
  metric = "mae",
  learning_rate = 0.05,
  num_leaves = 31,
  num_threads = parallel::detectCores() - 1
)

X_train <- as.matrix(df_hist[, feats_ar])

for (h in 1:24) {
  cat(sprintf("Treinando modelo para T+%dh...\n", h))
  target_col <- paste0("target_vis_lead", h)
  y_train <- df_hist[[target_col]]
  
  dtrain <- lgb.Dataset(data = X_train, label = y_train)
  
  # Usaremos um num_iterations fixo curto por rapidez (são 24 modelos) ou podemos usar CV
  # Como a base é enorme, 150 árvores é razoável.
  model <- lgb.train(params = params, data = dtrain, nrounds = 150, verbose = -1)
  
  ar_models[[paste0("lead", h)]] <- model
}

saveRDS(ar_models, "models/ar_models_24h.rds")
saveRDS(feats_ar, "models/ar_feats.rds")
cat("Treinamento finalizado. Modelos salvos em models/ar_models_24h.rds\n")
