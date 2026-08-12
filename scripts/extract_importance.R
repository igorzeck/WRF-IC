# Script para extrair a importância das variáveis do modelo WRF MOS Lagged
library(lightgbm)
library(tidyverse)

# Carregar o modelo WRF MOS Lagged (Regressão)
model <- readRDS("models/wrf_lagged_regression.rds")

# Extrair a importância
imp <- lgb.importance(model, percentage = TRUE)

# Salvar em CSV
write_csv(imp, "resources/mos_lagged_feature_importance.csv")

# Gerar Gráfico de barras
p <- ggplot(imp, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importância das Variáveis (Feature Importance)",
    subtitle = "Modelo: WRF MOS Lagged (Regressão)",
    x = "Variável (Feature)",
    y = "Ganho de Informação (Gain %)"
  ) +
  theme_minimal(base_size = 14)

ggsave("resources/mos_lagged_feature_importance.png", plot = p, width = 8, height = 6, bg = "white")

print(imp)
