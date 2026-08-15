# Script para análise de limitações do AR e Performance do Perfect Prog em dados perfeitos
library(tidyverse)
library(lubridate)
library(lightgbm)
library(ggplot2)

# --- PARTE 1: AR Model em Julho 2025 ---
cat("Iniciando Análise do Modelo AR (Julho 2025)...\n")

# Carregar dados históricos (2011-2025)
df_hist <- read_csv("datasets/metar_SBGL_2011_2025_lmlt.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  arrange(datetime)

# Filtro de tempo: Pegar um dia antes (para criar Lags) até o final do período
df_hist <- df_hist %>%
  filter(datetime >= as.POSIXct("2025-07-20 00:00:00", tz="UTC") & datetime <= as.POSIXct("2025-07-28 00:00:00", tz="UTC"))

lags <- c(0, 1, 2, 3, 6, 12, 18, 23)
vars_to_lag <- c("vis", "temp_ar", "umidade_relativa", "pressao", "vel_vento")
for (v in vars_to_lag) {
  for (l in lags) {
    df_hist[[paste0(v, "_lag", l)]] <- dplyr::lag(df_hist[[v]], l)
  }
}

factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem
df_hist <- df_hist %>% mutate(categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)))

feats_ar <- readRDS("models/ar_feats.rds")
ar_models <- readRDS("models/ar_models_24h.rds")

# Período de predição (Inicializações à 00Z)
init_times <- seq(as.POSIXct("2025-07-21 00:00:00", tz="UTC"), as.POSIXct("2025-07-27 00:00:00", tz="UTC"), by="days")
ar_preds <- data.frame(datetime=as.POSIXct(character()), pred_vis=numeric())

for (init_t in init_times) {
  init_t <- as.POSIXct(init_t, origin="1970-01-01", tz="UTC")
  row_T0 <- df_hist %>% filter(datetime == init_t)
  if(nrow(row_T0) == 0) next
  X_T0 <- as.matrix(row_T0[, feats_ar])
  
  # Para cada inicialização, os 24 modelos projetam as 24h seguintes
  for (h in 1:24) {
    target_dt <- init_t + hours(h)
    pred_val <- predict(ar_models[[paste0("lead", h)]], X_T0)
    ar_preds <- bind_rows(ar_preds, data.frame(datetime=target_dt, pred_vis=pred_val))
  }
}

# Juntar predição com as observações
df_plot <- ar_preds %>%
  inner_join(df_hist %>% select(datetime, obs_vis = vis), by="datetime") %>%
  filter(datetime <= as.POSIXct("2025-07-27 23:00:00", tz="UTC"))

df_plot_long <- df_plot %>%
  pivot_longer(cols = c(obs_vis, pred_vis), names_to = "Serie", values_to = "Visibilidade") %>%
  mutate(Serie = ifelse(Serie == "obs_vis", "Observado (METAR)", "Previsão AR (00Z Init)"))

# Gráfico da Série Temporal do AR com linhas pontilhadas
p_ar <- ggplot(df_plot_long, aes(x = datetime, y = Visibilidade, color = Serie)) +
  geom_line(linewidth = 1.2) +
  # Adicionar linha vertical em toda meia noite, exceto a primeira
  geom_vline(xintercept = as.numeric(init_times[-1]), linetype = "dotted", color = "black", linewidth = 0.8) +
  scale_color_manual(values = c("Observado (METAR)" = "black", "Previsão AR (00Z Init)" = "red")) +
  labs(
    title = "Desempenho Diário do Modelo Autoregressivo (AR)",
    subtitle = "Treinamento Histórico (21 a 27 Julho 2025): Linhas pontilhadas representam inicialização 00Z",
    x = "Data/Hora", y = "Visibilidade (m)", color = "Série"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("resources/limitacao_ar_2025_grafico.png", plot = p_ar, width=12, height=6, bg="white")


# --- PARTE 2: Perfect Prog em Observações Perfeitas (METAR 2026_lmlt) ---
cat("\nIniciando Análise do Perfect Prog no METAR 2026_lmlt...\n")

pp_model <- readRDS("models/lightgbm_regression_split.rds")
metar_2026 <- read_csv("datasets/metar_SBGL_2026_lmlt.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz="UTC")) %>%
  filter(datetime >= as.POSIXct("2026-06-22 00:00:00", tz="UTC") & datetime <= as.POSIXct("2026-06-28 00:00:00", tz="UTC")) %>%
  mutate(
    categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels))
  ) %>%
  filter(is.finite(vis))

feats_base <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa")

X_pp <- as.matrix(metar_2026[, feats_base])
preds_pp <- predict(pp_model, X_pp)
obs_pp <- metar_2026$vis

# Cálculo de Métricas
rmse <- sqrt(mean((preds_pp - obs_pp)^2, na.rm=TRUE))
mae <- mean(abs(preds_pp - obs_pp), na.rm=TRUE)
bias <- mean(preds_pp - obs_pp, na.rm=TRUE)
pearson <- cor(preds_pp, obs_pp, use="complete.obs")
r2 <- 1 - sum((obs_pp - preds_pp)^2, na.rm=TRUE) / sum((obs_pp - mean(obs_pp, na.rm=TRUE))^2, na.rm=TRUE)

res_pp <- data.frame(
  Model = "Perfect_Prog_on_True_METAR",
  N_Samples = length(obs_pp),
  RMSE = rmse,
  MAE = mae,
  Bias = bias,
  Pearson_Corr = pearson,
  R2_Score = r2
)

write_csv(res_pp, "resources/limitacao_pp_metar_metrics.csv")

cat("\n--- Resultados do Perfect Prog sobre Dados Observados (METAR 2026) ---\n")
print(res_pp)
cat("\nScript executado com sucesso. Arquivos exportados em 'resources/'\n")
