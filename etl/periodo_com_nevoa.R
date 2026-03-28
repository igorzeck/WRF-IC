# Script para achar períodos com baixa visibilidade para um dado METAR
# Setup ----
library(dplyr)
library(lubridate)
library(ggplot2)

df_metar <- read_csv("datasets/metar_SBGL_2026.csv")
df_metar

head(df_metar)
tail(df_metar)

# Análise ----
# Procura um período CONTÍNUO de baixa visibilidade
# 1. Agrupa baseado em diferenças de visibilidade em relação a um valor base
# 2. Utiliza esse agrupamento para coletar o maior agrupamento com valores abaixo base

achar_p_mais_longo <- function(vis_ref) {
  df_metar <- df_metar %>% arrange(datetime)
  
  low_vis <- df_metar$visibility < vis_ref
  
  r <- rle(low_vis)
  
  # Find the longest TRUE run
  idx <- which(r$values)
  longest_run <- idx[which.max(r$lengths[idx])]
  
  # Get row indices
  start <- sum(r$lengths[seq_len(longest_run - 1)]) + 1
  end   <- start + r$lengths[longest_run] - 1
  
  longest_period <- df_metar[start:end, ]
  longest_period
}

df_p <- achar_p_mais_longo(7000)
df_p
max(df_p$datetime) - min(df_p$datetime)
# 16h de diferença

# Graficamente ----
df_p %>% 
  ggplot(aes(datetime, visibility)) +
  geom_line() +
  ylim(0,1e4)

max_vis <- max(df_p$visibility)
paste0("Max: ", max_vis, "m")
min_vis <- min(df_p$visibility)
paste0("Min: ", min_vis, "m")
paste0("Range: ", max_vis - min_vis, "m")
# 