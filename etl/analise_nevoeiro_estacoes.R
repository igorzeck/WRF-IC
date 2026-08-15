#!/usr/bin/env Rscript
# ==============================================================================
# analise_nevoeiro_estacoes.R
# Analisa a incidência de nevoeiro por estação e por mês no dataset METAR SBGL (2011-2024)
# Gera visualizações em PT-BR e exporta tabelas/gráficos para o diretório `resources/`.
# ==============================================================================

library(tidyverse)
library(lubridate)

# 1. Carregamento e tratamento dos dados ----
dataset_path <- "datasets/metar_SBGL_2011_2025_lmlt.csv"

if (!file.exists(dataset_path)) {
  stop("Erro: Dataset não encontrado em: ", dataset_path)
}

cat("Carregando dataset METAR:", dataset_path, "\n")
df <- read_csv(dataset_path, show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  filter(year(datetime) >= 2011 & year(datetime) <= 2024)

num_anos <- length(unique(year(df$datetime)))

# Definir estações do ano para o Hemisfério Sul
obter_estacao <- function(mes) {
  case_when(
    mes %in% c(12, 1, 2) ~ "Verão",
    mes %in% c(3, 4, 5)  ~ "Outono",
    mes %in% c(6, 7, 8)  ~ "Inverno",
    mes %in% c(9, 10, 11) ~ "Primavera"
  )
}

df <- df %>%
  mutate(
    mes_num = month(datetime),
    hora = hour(datetime),
    estacao = factor(obter_estacao(mes_num), levels = c("Verão", "Outono", "Inverno", "Primavera")),
    mes_nome = factor(
      mes_num,
      levels = 1:12,
      labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    ),
    nevoeiro_1000 = as.integer(vis < 1000)
  )

# 2. Resumo estatístico e exportação de tabelas ----
cat("\n=== INCIDÊNCIA DE NEVOEIRO (< 1000m) POR ESTAÇÃO (2011-2024 -", num_anos, "Anos) ===\n")
resumo_estacao <- df %>%
  group_by(estacao) %>%
  summarise(
    horas_nevoeiro_acumuladas = sum(nevoeiro_1000, na.rm = TRUE),
    media_horas_por_ano = round(horas_nevoeiro_acumuladas / num_anos, 2),
    total_registros = n(),
    percentual = round((horas_nevoeiro_acumuladas / total_registros) * 100, 3)
  )
print(resumo_estacao)

cat("\n=== INCIDÊNCIA DE NEVOEIRO (< 1000m) POR MÊS (2011-2024) ===\n")
resumo_mes <- df %>%
  group_by(mes_nome, estacao) %>%
  summarise(
    horas_nevoeiro_acumuladas = sum(nevoeiro_1000, na.rm = TRUE),
    media_horas_por_ano = round(horas_nevoeiro_acumuladas / num_anos, 2),
    .groups = "drop"
  )
print(resumo_mes)

# Exporta as tabelas para arquivos CSV
out_tab_estacao <- "resources/incidencia_nevoeiro_estacao.csv"
out_tab_mes <- "resources/incidencia_nevoeiro_mes.csv"

write_csv(resumo_estacao, out_tab_estacao)
write_csv(resumo_mes, out_tab_mes)

cat("\nTabela de estações exportada para:", out_tab_estacao, "\n")
cat("Tabela mensal exportada para:", out_tab_mes, "\n")

# 3. Visualização 1: Heatmap Diurno-Sazonal (Mês vs Hora do Dia UTC) ----
dados_heatmap <- df %>%
  group_by(mes_nome, hora) %>%
  summarise(horas_nevoeiro = sum(nevoeiro_1000, na.rm = TRUE), .groups = "drop")

p_heatmap <- ggplot(dados_heatmap, aes(x = hora, y = fct_rev(mes_nome), fill = horas_nevoeiro)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", name = "Horas com\nNevoeiro") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = NULL,
    x = "Hora do Dia (UTC)",
    y = "Mês"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    legend.position = "right"
  )

out_heatmap <- "resources/nevoeiro_heatmap_mes_hora.png"
ggsave(out_heatmap, plot = p_heatmap, width = 10, height = 6, bg = "white", dpi = 300)
cat("Heatmap exportado para:", out_heatmap, "\n")

# 4. Visualização 2: Gráfico de Barras Por Estação e Mês ----
p_barras <- ggplot(resumo_mes, aes(x = mes_nome, y = horas_nevoeiro_acumuladas, fill = estacao)) +
  geom_col(show.legend = TRUE) +
  scale_fill_manual(
    values = c(
      "Verão" = "#E69F00",
      "Outono" = "#56B4E9",
      "Inverno" = "#0072B2",
      "Primavera" = "#CC79A7"
    ),
    name = "Estação"
  ) +
  labs(
    title = NULL,
    x = "Mês",
    y = "Total de Horas com Nevoeiro (< 1000m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

out_barras <- "resources/nevoeiro_por_estacao_mes.png"
ggsave(out_barras, plot = p_barras, width = 9, height = 5.5, bg = "white", dpi = 300)
cat("Gráfico de barras exportado para:", out_barras, "\n")
