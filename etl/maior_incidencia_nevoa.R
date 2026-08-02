# Script para encontrar o período de X horas com a maior quantidade 
# de casos de visibilidade inferior a um limite especificado.
#
# Uso: Rscript maior_incidencia_nevoa.R [janela_horas] [limite_vis] [ano] [estacao]
# Ex:  Rscript maior_incidencia_nevoa.R 24 1000 2026 SBGL

library(tidyverse)
library(lubridate)
library(ggplot2)

# Valores padrão
window_hours <- 24
vis_ref <- 1000
ano <- "2026"
local <- "SBGL"

# Ler argumentos de linha de comando, se fornecidos
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) window_hours <- as.numeric(args[1])
if (length(args) >= 2) vis_ref <- as.numeric(args[2])
if (length(args) >= 3) ano <- args[3]
if (length(args) >= 4) local <- args[4]

# Modo interativo se executado no RStudio ou console (sem argumentos)
if (interactive() && length(args) == 0) {
  cat("Modo interativo. Pressione Enter para usar os valores padrão.\n")
  
  in_window <- readline(prompt = paste0("Tamanho da janela em horas [", window_hours, "]: "))
  if (in_window != "") window_hours <- as.numeric(in_window)
  
  in_vis <- readline(prompt = paste0("Visibilidade limite (m) [", vis_ref, "]: "))
  if (in_vis != "") vis_ref <- as.numeric(in_vis)
  
  in_ano <- readline(prompt = paste0("Ano [", ano, "]: "))
  if (in_ano != "") ano <- in_ano
  
  in_local <- readline(prompt = paste0("Local (ICAO) [", local, "]: "))
  if (in_local != "") local <- in_local
}

file_path <- paste0("datasets/metar_", local, "_", ano, ".csv")

if (!file.exists(file_path)) {
  stop("ERRO: Arquivo não encontrado: ", file_path)
}

cat(sprintf("Lendo dados de %s...\n", file_path))
df_metar <- read_csv(file_path, show_col_types = FALSE)

# Pre-processamento
df_metar <- df_metar %>% 
  arrange(datetime) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

# Identificar eventos e inicializar variaveis
is_fog <- as.integer(!is.na(df_metar$visibility) & df_metar$visibility < vis_ref)
times <- df_metar$datetime
n <- nrow(df_metar)

# Algoritmo de Window Sliding
fog_counts <- numeric(n)
right <- 1
current_fog <- 0

for (left in 1:n) {
  # Avancar o ponteiro da direita ate atingir o limite de horas da janela
  while (right <= n && times[right] <= times[left] + hours(window_hours)) {
    current_fog <- current_fog + is_fog[right]
    right <- right + 1
  }
  fog_counts[left] <- current_fog
  # Ao avancar o ponteiro da esquerda (no loop for), subtraimos o valor que ficou pra tras
  current_fog <- current_fog - is_fog[left]
}

# Encontrar o indice inicial com maior quantidade de casos
best_left <- which.max(fog_counts)
max_cases <- fog_counts[best_left]

# Reconstruir o limite direito do melhor periodo
best_right <- best_left
while (best_right <= n && times[best_right] <= times[best_left] + hours(window_hours)) {
  best_right <- best_right + 1
}
best_right <- best_right - 1

df_p <- df_metar[best_left:best_right, ]

# Apresentacao dos resultados
cat(sprintf("\n=======================================\n"))
cat(sprintf(" RESULTADOS DA ANÁLISE\n"))
cat(sprintf("=======================================\n"))
cat(sprintf("Maior concentracao de casos (< %dm) em janela de %dh!\n", vis_ref, window_hours))
cat(sprintf("Total de observacoes na janela: %d\n", nrow(df_p)))
cat(sprintf("Total de casos positivos (< %dm): %d\n", vis_ref, max_cases))
cat(sprintf("Periodo: de %s ate %s\n", min(df_p$datetime), max(df_p$datetime)))

# Grafico
max_vis_plot <- max(c(df_p$visibility, vis_ref), na.rm = TRUE)
p <- df_p %>% 
  ggplot(aes(x = datetime, y = visibility)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = vis_ref, color = "red", linetype = "dashed", linewidth = 1) +
  scale_y_continuous(limits = c(0, max_vis_plot + 1000)) +
  theme_minimal() +
  labs(title = paste0("Maior incidência de visibilidade < ", vis_ref, "m em ", window_hours, "h"),
       subtitle = paste0(local, " - ", ano, " | Eventos detectados: ", max_cases),
       x = "Data e Hora (UTC)", y = "Visibilidade (m)") +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"))

# Exportacao
out_dir <- "resources/ocorrencias_vis_baixa"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_name <- paste0("df_", local, "_", ano, "_max_casos_", window_hours, "h_menor_vis_", vis_ref, "m")
csv_path <- file.path(out_dir, paste0(out_name, ".csv"))
png_path <- file.path(out_dir, paste0(out_name, ".png"))

write_csv(df_p, csv_path)
ggsave(png_path, plot = p, width = 10, height = 6, dpi = 300)

cat(sprintf("\nArquivos salvos:\n"))
cat(sprintf(" - Dados (CSV): %s\n", csv_path))
cat(sprintf(" - Grafico (PNG): %s\n", png_path))
cat(sprintf("=======================================\n"))
