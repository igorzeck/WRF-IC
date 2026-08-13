# Script para avaliar o WRF MOS usando todas as variáveis da física do WRF

library(tidyverse)
library(lightgbm)

# 1. Carregar Dados e Preparar Features
df_wrf <- read_csv("datasets/wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  select(-any_of(c("geopotential_height_0deg", "geopotential_height_ceil", 
             "geopotential_height_htfl", "parcel_lifted_index_to_500_h_pa", 
             "probability_of_precipitation", "surface_lifted_index", "surface_roughness")))

feats_base <- setdiff(names(df_wrf), "datetime")
df_wrf_lagged <- df_wrf %>% arrange(datetime)
for (f in feats_base) {
  df_wrf_lagged[[paste0(f, "_lag3")]] <- dplyr::lag(df_wrf[[f]], 3)
  df_wrf_lagged[[paste0(f, "_lag6")]] <- dplyr::lag(df_wrf[[f]], 6)
}
feats_lagged <- setdiff(names(df_wrf_lagged), "datetime")

metar_obs <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>% select(datetime, obs_vis = visibility)

df_eval <- df_wrf_lagged %>% inner_join(metar_obs, by = "datetime") %>% filter(is.finite(obs_vis)) %>% drop_na()

# 2. Previsões
model_base <- readRDS("models/wrf_full_mos_base.rds")
model_lagged <- readRDS("models/wrf_full_mos_lagged.rds")

preds_base <- predict(model_base, as.matrix(df_eval[, feats_base]))
preds_lagged <- predict(model_lagged, as.matrix(df_eval[, feats_lagged]))
obs <- df_eval$obs_vis

# 3. Métricas de Regressão
calc_reg_metrics <- function(preds, obs, name) {
  rmse <- sqrt(mean((preds - obs)^2))
  mae <- mean(abs(preds - obs))
  bias <- mean(preds - obs)
  r2 <- 1 - sum((obs - preds)^2) / sum((obs - mean(obs))^2)
  pearson <- cor(preds, obs)
  data.frame(Model=name, N_Valid_Samples=length(obs), RMSE=rmse, MAE=mae, Bias=bias, Pearson=pearson, R2=r2)
}

res_reg <- bind_rows(
  calc_reg_metrics(preds_base, obs, "wrf_full_mos_base"),
  calc_reg_metrics(preds_lagged, obs, "wrf_full_mos_lagged")
)

csv_reg <- "resources/metricas_vis_modelos.csv"
if(file.exists(csv_reg)) {
  df_reg <- read_csv(csv_reg, show_col_types = FALSE) %>% filter(!Model %in% res_reg$Model)
  write_csv(bind_rows(df_reg, res_reg), csv_reg)
}

# 4. Métricas de Classificação (Nevoeiro/Restrição <= 3000m)
calc_class_metrics <- function(preds, obs, name) {
  p <- ifelse(preds <= 3000, 1, 0)
  o <- ifelse(obs <= 3000, 1, 0)
  a <- sum(p == 1 & o == 1); b <- sum(p == 1 & o == 0)
  c <- sum(p == 0 & o == 1); d <- sum(p == 0 & o == 0)
  N <- a + b + c + d
  acc <- (a + d) / N
  csi <- ifelse((a + b + c) > 0, a / (a + b + c), 0)
  hss_num <- 2 * (a * d - b * c)
  hss_den <- ((a + c) * (c + d) + (a + b) * (b + d))
  hss <- ifelse(hss_den > 0, hss_num / hss_den, 0)
  f1 <- ifelse((2 * a + b + c) > 0, (2 * a) / (2 * a + b + c), 0)
  data.frame(Model=name, Accuracy=acc, CSI_ThreatScore=csi, HeidkeSkillScore=hss, F1_Score=f1, TP=a, FP=b, FN=c, TN=d)
}

res_class <- bind_rows(
  calc_class_metrics(preds_base, obs, "fog_wrf_full_mos_base"),
  calc_class_metrics(preds_lagged, obs, "fog_wrf_full_mos_lagged")
)

csv_class <- "resources/metricas_classificacao_modelos.csv"
if(file.exists(csv_class)) {
  df_class <- read_csv(csv_class, show_col_types = FALSE) %>% filter(!Model %in% res_class$Model)
  write_csv(bind_rows(df_class, res_class), csv_class)
}

cat("Métricas calculadas e salvas em CSV!\n")
print(res_reg)
print(res_class)
