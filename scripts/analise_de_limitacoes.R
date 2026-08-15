# Script para análise de limitações do AR, PP e MOS (incluindo painéis combinados)
library(tidyverse)
library(lubridate)
library(lightgbm)
library(ggplot2)
library(patchwork)

# --- PARTE 1: AR Model em Junho 2025 (21/06 a 27/06/2025) ---
cat("Iniciando Análise do Modelo AR (Junho 2025)...\n")

# Carregar dados históricos (2011-2025)
df_hist <- read_csv("datasets/metar_SBGL_2011_2025_lmlt.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  arrange(datetime)

# Filtro de tempo: Pegar 20/06 (para criar Lags) até 28/06/2025
df_hist <- df_hist %>%
  filter(datetime >= as.POSIXct("2025-06-20 00:00:00", tz="UTC") & datetime <= as.POSIXct("2025-06-28 00:00:00", tz="UTC"))

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

# Período de predição (Inicializações à 00Z de 21 a 27 de Junho 2025)
init_times_2025 <- seq(as.POSIXct("2025-06-21 00:00:00", tz="UTC"), as.POSIXct("2025-06-27 00:00:00", tz="UTC"), by="days")
ar_preds <- data.frame(datetime=as.POSIXct(character()), pred_vis=numeric(), lead_h=numeric())

for (init_t in init_times_2025) {
  init_t <- as.POSIXct(init_t, origin="1970-01-01", tz="UTC")
  row_T0 <- df_hist %>% filter(datetime == init_t)
  if(nrow(row_T0) == 0) next
  X_T0 <- as.matrix(row_T0[, feats_ar])
  
  for (h in 1:24) {
    target_dt <- init_t + hours(h)
    pred_val <- predict(ar_models[[paste0("lead", h)]], X_T0)
    ar_preds <- bind_rows(ar_preds, data.frame(datetime=target_dt, pred_vis=pred_val, lead_h=h))
  }
}

# Juntar predição com as observações
df_plot_ar <- ar_preds %>%
  inner_join(df_hist %>% select(datetime, obs_vis = vis), by="datetime") %>%
  filter(datetime <= as.POSIXct("2025-06-27 23:00:00", tz="UTC")) %>%
  mutate(abs_error = abs(pred_vis - obs_vis))

df_plot_ar_long <- df_plot_ar %>%
  pivot_longer(cols = c(obs_vis, pred_vis), names_to = "Serie", values_to = "Visibilidade") %>%
  mutate(Serie = ifelse(Serie == "obs_vis", "Observado (METAR)", "Previsão AR (00Z Init)"))

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

rmse_pp <- sqrt(mean((preds_pp - obs_pp)^2, na.rm=TRUE))
mae_pp <- mean(abs(preds_pp - obs_pp), na.rm=TRUE)
bias_pp <- mean(preds_pp - obs_pp, na.rm=TRUE)
pearson_pp <- cor(preds_pp, obs_pp, use="complete.obs")
r2_pp <- 1 - sum((obs_pp - preds_pp)^2, na.rm=TRUE) / sum((obs_pp - mean(obs_pp, na.rm=TRUE))^2, na.rm=TRUE)

res_pp <- data.frame(
  Model = "Perfect_Prog_on_True_METAR",
  N_Samples = length(obs_pp),
  RMSE = rmse_pp,
  MAE = mae_pp,
  Bias = bias_pp,
  Pearson_Corr = pearson_pp,
  R2_Score = r2_pp
)

write_csv(res_pp, "resources/limitacao_pp_metar_metrics.csv")
print(res_pp)


# --- PARTE 3: WRF MOS Lagged Model no Treinamento (Simulação 2026) ---
cat("\nIniciando Análise do WRF MOS Lagged (Junho 2026)...\n")

mos_model_lagged <- readRDS("models/wrf_lagged_regression.rds")
feats_lagged <- c(feats_base, "temp_ar_lag3", "temp_ar_lag6", "umidade_relativa_lag3", "umidade_relativa_lag6", "pressao_lag3", "pressao_lag6")

df_wrf <- read_csv("datasets/wrf_emulated_wrf_raw_out2.csv", show_col_types = FALSE) %>%
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

df_eval_mos <- df_wrf %>%
  inner_join(metar_2026 %>% select(datetime, obs_vis = vis), by = "datetime") %>%
  drop_na(all_of(c(feats_lagged, "obs_vis")))

X_mos <- as.matrix(df_eval_mos[, feats_lagged])
preds_mos <- predict(mos_model_lagged, X_mos)
df_eval_mos$pred_vis <- preds_mos
df_eval_mos$abs_error <- abs(preds_mos - df_eval_mos$obs_vis)

init_times_2026 <- seq(as.POSIXct("2026-06-22 00:00:00", tz="UTC"), as.POSIXct("2026-06-28 00:00:00", tz="UTC"), by="days")

df_plot_mos_long <- df_eval_mos %>%
  select(datetime, obs_vis, pred_vis) %>%
  pivot_longer(cols = c(obs_vis, pred_vis), names_to = "Serie", values_to = "Visibilidade") %>%
  mutate(Serie = ifelse(Serie == "obs_vis", "Observado (METAR)", "Previsão WRF MOS Lagged"))


# --- PARTE 4: Gerar Painéis Combinados (AR no topo com (a), MOS em baixo com (b)) ---
cat("\nGerando Painéis Combinados (Sem Título Principal, com Subtítulos (a) e (b))...\n")

# 4.1 Séries Temporais Combinadas
p_ar_ts <- ggplot(df_plot_ar_long, aes(x = datetime, y = Visibilidade, color = Serie)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = init_times_2025[-1], linetype = "dotted", color = "black", linewidth = 0.7) +
  scale_color_manual(values = c("Observado (METAR)" = "black", "Previsão AR (00Z Init)" = "red")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d/%m %H:00") +
  labs(
    title = NULL,
    subtitle = "(a) Modelo Autoregressivo (AR): Inicialização Diária à 00:00Z (21 a 27 Junho 2025)",
    x = "Data/Hora", y = "Visibilidade (m)", color = "Série"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))

p_mos_ts <- ggplot(df_plot_mos_long, aes(x = datetime, y = Visibilidade, color = Serie)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = init_times_2026[-c(1, length(init_times_2026))], linetype = "dotted", color = "black", linewidth = 0.7) +
  scale_color_manual(values = c("Observado (METAR)" = "black", "Previsão WRF MOS Lagged" = "blue")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d/%m %H:00") +
  labs(
    title = NULL,
    subtitle = "(b) Modelo WRF MOS Lagged: Simulação Contínua de 144h (22 a 28 Junho 2026)",
    x = "Data/Hora", y = "Visibilidade (m)", color = "Série"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 30, hjust = 1))

p_ts_combined <- p_ar_ts / p_mos_ts

ggsave("resources/limitacao_ar_vs_mos_serie_temporal_combined.png", plot = p_ts_combined, width = 11, height = 9, bg = "white")


# 4.2 Erro Absoluto Combinado
p_ar_err <- ggplot(df_plot_ar, aes(x = datetime, y = abs_error)) +
  geom_area(fill = "firebrick", alpha = 0.3) +
  geom_line(color = "firebrick", linewidth = 0.9) +
  geom_vline(xintercept = init_times_2025[-1], linetype = "dotted", color = "black", linewidth = 0.7) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d/%m %H:00") +
  labs(
    title = NULL,
    subtitle = "(a) Erro Absoluto - Modelo Autoregressivo (AR): Queda de erro na inicialização 00:00Z",
    x = "Data/Hora", y = "Erro Absoluto (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_mos_err <- ggplot(df_eval_mos, aes(x = datetime, y = abs_error)) +
  geom_area(fill = "steelblue", alpha = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_vline(xintercept = init_times_2026[-c(1, length(init_times_2026))], linetype = "dotted", color = "black", linewidth = 0.7) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d/%m %H:00") +
  labs(
    title = NULL,
    subtitle = "(b) Erro Absoluto - Modelo WRF MOS Lagged: Erro estável na simulação contínua",
    x = "Data/Hora", y = "Erro Absoluto (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_err_combined <- p_ar_err / p_mos_err

ggsave("resources/limitacao_ar_vs_mos_erro_absoluto_combined.png", plot = p_err_combined, width = 11, height = 9, bg = "white")

cat("\nScript executado com sucesso! Gráficos combinados salvos em 'resources/'.\n")
