# Scripts para prever tempo de execução do WRF
# Intervalo de tempo em horas (assume-se entrada do GFS 6h a 6h)
# Setup ----
library(lubridate)
# Modelo ----
# Teoricamente poderia só utilizar a fórmula, mas assim é mais modular
modelo <- readRDS("datasets/modelos/modelo_lm.rds")

# Predição temporal ----
tempo <- readline(prompt="Período total de input (XhYmZs): ")
#  Período de previsão apenas para o modelo WRF!
prev <- predict(modelo, newdata = data.frame(input_s = period_to_seconds(hms(tempo))))
print(seconds_to_period(prev))

