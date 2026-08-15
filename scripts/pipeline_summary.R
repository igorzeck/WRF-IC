# Script para gerar a tabela de resumo do pipeline (GFS -> WRF -> WRF MOS)
library(tidyverse)
library(ggplot2)

# 1. Carregar as métricas calculadas
df_reg <- read_csv("resources/metricas_vis_modelos.csv", show_col_types = FALSE)
df_class <- read_csv("resources/metricas_classificacao_modelos.csv", show_col_types = FALSE)

# 2. Filtrar os modelos alvo do pipeline
# Regressão
reg_models <- c("gfs_koschmieder", "wrf_koschmieder", "wrf_mos_vis_lagged")
df_reg_filtered <- df_reg %>% 
  filter(Model %in% reg_models) %>%
  mutate(Step = case_when(
    Model == "gfs_koschmieder" ~ "1. Global (GFS)",
    Model == "wrf_koschmieder" ~ "2. Regional (WRF)",
    Model == "wrf_mos_vis_lagged" ~ "3. Correção (WRF MOS Lagged)"
  )) %>%
  select(Step, RMSE, MAE, Bias)

# Classificação
class_models <- c("fog_gfs_koschmieder", "fog_wrf_koschmieder", "fog_wrf_mos_lagged")
df_class_filtered <- df_class %>% 
  filter(Model %in% class_models) %>%
  mutate(Step = case_when(
    Model == "fog_gfs_koschmieder" ~ "1. Global (GFS)",
    Model == "fog_wrf_koschmieder" ~ "2. Regional (WRF)",
    Model == "fog_wrf_mos_lagged" ~ "3. Correção (WRF MOS Lagged)"
  )) %>%
  select(Step, Accuracy, CSI_ThreatScore, F1_Score, HeidkeSkillScore)

# 3. Juntar tabelas
df_pipeline <- df_reg_filtered %>%
  inner_join(df_class_filtered, by = "Step") %>%
  arrange(Step)

# Salvar tabela
write_csv(df_pipeline, "resources/pipeline_summary_metrics.csv")

# 4. Gerar Gráfico de barras (Melhoria do Erro - RMSE e MAE)
df_plot <- df_pipeline %>%
  select(Step, RMSE, MAE) %>%
  pivot_longer(cols = c(RMSE, MAE), names_to = "Metric", values_to = "Error_Meters")

p1 <- ggplot(df_plot, aes(x = Step, y = Error_Meters, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.9) +
  geom_text(aes(label = round(Error_Meters, 0)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size=4) +
  scale_fill_manual(values = c("RMSE" = "#d95f02", "MAE" = "#1b9e77")) +
  labs(
    x = "Etapa do Pipeline",
    y = "Erro (Metros)",
    fill = "Métrica"
  ) +
  theme_minimal(base_size = 14)

ggsave("resources/pipeline_error_reduction.png", plot = p1, width = 9, height = 6, bg="white")

# 5. Gerar Gráfico de Evolução da Classificação (F1 Score)
p2 <- ggplot(df_pipeline, aes(x = Step, y = F1_Score, group=1)) +
  geom_line(color="steelblue", size=1.5) +
  geom_point(color="red", size=4) +
  geom_text(aes(label = round(F1_Score, 3)), vjust = -1.5, size=4.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Etapa do Pipeline",
    y = "F1 Score (0 a 1)"
  ) +
  theme_minimal(base_size = 14)

ggsave("resources/pipeline_f1_improvement.png", plot = p2, width = 9, height = 6, bg="white")

print(df_pipeline)
