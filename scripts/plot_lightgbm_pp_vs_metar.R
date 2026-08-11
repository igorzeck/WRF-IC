# Script para plotar Visibilidade: LightGBM Perfect Prog vs METAR
library(tidyverse)
library(lubridate)
library(lightgbm)

set.seed(42)

# 1. Carregar METAR (Observações Horárias)
metar_raw <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

metar_obs <- metar_raw %>%
  filter(datetime >= as.POSIXct("2026-06-22 00:00:00", tz = "UTC") &
         datetime <= as.POSIXct("2026-06-28 00:00:00", tz = "UTC")) %>%
  mutate(obs_vis = visibility) %>%
  select(datetime, obs_vis)

# 2. Carregar Dados WRF Extraídos para as features
wrf_mos_raw <- read_csv("datasets/wrf_emulated_wrf_raw_out2.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

# 3. Modelos Perfect Prog (LightGBM)
lgb_model <- readRDS("models/lightgbm_metar_historical_regression.rds")
factor_levels <- readRDS("models/factor_levels.rds")$categ_nuvem
feats <- c("vel_vento", "dir_vento", "temp_ar", "temp_orvalho", "pressao", "categ_nuvem", "lmlt", "umidade_relativa")

wrf_mos_raw <- wrf_mos_raw %>%
  mutate(categ_nuvem = as.integer(factor(categ_nuvem, levels = factor_levels)))

X_mat <- as.matrix(wrf_mos_raw[, feats])
wrf_mos_raw$preds_lgb <- predict(lgb_model, X_mat)

wrf_mos_df <- wrf_mos_raw %>% select(datetime, preds_lgb)

# 4. Mesclar (Avaliação Horária, 145 amostras)
df_eval <- metar_obs %>%
  inner_join(wrf_mos_df, by = "datetime") %>%
  filter(is.finite(obs_vis))

# 5. Formatar para o ggplot
plot_df <- df_eval %>%
  select(datetime, obs_vis, preds_lgb) %>%
  pivot_longer(cols = -datetime, names_to = "Method", values_to = "Visibility")

# 6. Gerar Gráfico
p <- ggplot(plot_df, aes(x = datetime, y = Visibility, color = Method)) +
  geom_line(alpha = 0.8, linewidth = 1.2) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "red", alpha = 0.6) + 
  annotate("text", x = min(plot_df$datetime) + hours(6), y = 1500, label = "Limiar Nevoeiro (1000m)", color = "red") +
  scale_color_manual(
    values = c(
      "obs_vis" = "black",
      "preds_lgb" = "cornflowerblue"
    ),
    labels = c(
      "obs_vis" = "METAR Observado",
      "preds_lgb" = "LightGBM Perfect Prog"
    )
  ) +
  scale_y_continuous(labels = scales::comma_format(suffix = " m"), limits = c(0, 15000)) +
  labs(
    title = "Previsão de Visibilidade: LightGBM Perfect Prog vs Realidade",
    subtitle = "Avaliação Horária - Saída WRF vs METAR (22-28 de Junho)",
    x = "Data",
    y = "Visibilidade (m)",
    color = "Fonte"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

img_path <- "resources/vis_lightgbm_pp_vs_metar.png"
ggsave(img_path, plot = p, width = 12, height = 6, dpi = 300)

message("Gráfico salvo em: ", img_path)
