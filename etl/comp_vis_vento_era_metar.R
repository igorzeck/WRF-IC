# EDA dos METARs e arquivos Grib (em NetCDF4 - NC) para as regiões relevantes
# 0. Setup ----
library(tidyverse)
library(ncdf4)
library(lubridate)

# 0.1 Tempo ----
# Abre um NC qualquer para pegar as unidades de tempo (horas desde de 1900)
nc_arq <- nc_open("datasets/unico_rj.nc")

valores_t <- ncvar_get(nc_arq, "time")
unid_t <- ncatt_get(nc_arq, "time", "units")$value

t_ustr <- strsplit(unid_t, " ")
t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])

# valores_t * 3600 -> de Segundos para Horas
seq_h <- as.POSIXct(valores_t * 3600, origin = t_origem, tz = "UTC")

nc_close(nc_arq)

# 0.2 Funções
# GET para uma variável do ERA5
get_era_var <- function(arq, variavel) {
  # As variávies estão divididas em 2
  # var_0001 para jan - nov e var_0005 para dez
  era_jan_nov = ncvar_get(nc_arq, paste0(variavel,"_0001"))
  era_dez = ncvar_get(nc_arq, paste0(variavel,"_0005"))
  
  # Como são complementares é possível utilziar o coalesce (pega primeiro não NA dos dois)
  era_variavel <- coalesce(era_jan_nov, era_dez)
  
  # Retorno
  tibble(
    datetime = seq_h,
    variavel_simul = era_variavel
  )
}

# Normalização min-máx
min_maximizar <- function(valores) {
  (valores - min(valores))/(max(valores) - min(valores))
}

# 1. BA - Nível único ----
# METAR
df_metar = read_csv("datasets/metar_SBSV_2025.csv")

# Converte visibilidade para uma escala de 0 a 1
# E inverte para que 0 indice visibilidade máxima e 1 visibilidade nenhuma
df_metar <- df_metar |> 
  mutate(vis = 1 - min_maximizar(visibility))

df_metar |> 
  ggplot(aes(x = datetime, y = vis)) +
  geom_line(colour="green")

nc_arq = nc_open("datasets/unico_ba.nc")

df_era_variavel <- get_era_var(nc_arq, 'lcc')

df_era_variavel |> 
  ggplot(aes(x = datetime, y = variavel_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_variavel |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_variavel <- df_era_variavel |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_variavel, df_metar, by = "datetime")

df_comp |> 
  ggplot() +
  geom_line(aes(datetime, vis), colour = "gray") +
  geom_line(aes(datetime, variavel_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$vis, df_comp$variavel_simul)
Aproximadamente 0.19... BEM BAiXA!
nc_close(nc_arq)


# 2. BA - Nível de pressão ----
nc_arq = nc_open("datasets/press_ba.nc")

df_era_variavel <- get_era_var(nc_arq, 'cc')

df_era_variavel |> 
  ggplot(aes(x = datetime, y = variavel_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_variavel |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_variavel <- df_era_variavel |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_variavel, df_metar, by = "datetime")

df_comp |> 
  ggplot() +
  geom_line(aes(datetime, vis), colour = "gray") +
  geom_line(aes(datetime, variavel_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$vis, df_comp$variavel_simul)
# Aproximadamente 0.06... Menor ainda...
nc_close(nc_arq)
# Conclusão: Necessário algo mais para saber a visibilidade!

# 3. RJ - Nível único ----
# METAR
df_metar = read_csv("datasets/metar_SBRJ_2025.csv")

# Converte visibilidade para uma escala de 0 a 1
# E inverte para que 0 indice visibilidade máxima e 1 visibilidade nenhuma
df_metar <- df_metar |> 
  mutate(vis = 1 - min_maximizar(visibility))

df_metar |> 
  ggplot(aes(x = datetime, y = vis)) +
  geom_line(colour="green")

nc_arq = nc_open("datasets/unico_rj.nc")

df_era_variavel <- get_era_var(nc_arq, 'lcc')

df_era_variavel |> 
  ggplot(aes(x = datetime, y = variavel_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_variavel |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_variavel <- df_era_variavel |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_variavel, df_metar, by = "datetime")

df_comp |> 
  ggplot() +
  geom_line(aes(datetime, vis), colour = "gray") +
  geom_line(aes(datetime, variavel_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$vis, df_comp$variavel_simul)
# Aproximadamente 0.30, baixa
nc_close(nc_arq)

# 4. RJ - Nível de pressão ----
# METAR
df_metar = read_csv("datasets/metar_SBRJ_2025.csv")

# Converte visibilidade para uma escala de 0 a 1
# E inverte para que 0 indice visibilidade máxima e 1 visibilidade nenhuma
df_metar <- df_metar |> 
  mutate(vis = 1 - min_maximizar(visibility))

df_metar |> 
  ggplot(aes(x = datetime, y = vis)) +
  geom_line(colour="green")

nc_arq = nc_open("datasets/press_rj.nc")

df_era_variavel <- get_era_var(nc_arq, 'cc')

df_era_variavel |> 
  ggplot(aes(x = datetime, y = variavel_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_variavel |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_variavel <- df_era_variavel |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_variavel, df_metar, by = "datetime")

df_comp |> 
  ggplot() +
  geom_line(aes(datetime, vis), colour = "gray") +
  geom_line(aes(datetime, variavel_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$vis, df_comp$variavel_simul)
# Aproximadamente 0.14, baixa
nc_close(nc_arq)

# 5. RJ - Nível único - Vento ----
# METAR
df_metar = read_csv("datasets/metar_SBRJ_2025.csv")

df_metar |> 
  ggplot(aes(x = datetime, y = wind_speed)) +
  geom_line(colour="green")

nc_arq = nc_open("datasets/unico_rj.nc")

df_era_u <- get_era_var(nc_arq, 'u10')
df_era_v <- get_era_var(nc_arq, 'v10')

df_era_variavel <- tibble(
    datetime = df_era_u$datetime,
    variavel_simul = sqrt(df_era_u$variavel_simul ** 2 + df_era_v$variavel_simul ** 2)
  )

df_era_variavel |> 
  ggplot(aes(x = datetime, y = variavel_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_variavel |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_variavel <- df_era_variavel |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_variavel, df_metar, by = "datetime")

df_comp |> 
  ggplot() +
  geom_line(aes(datetime, wind_speed), colour = "gray") +
  geom_line(aes(datetime, variavel_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$wind_speed, df_comp$variavel_simul)
# Aproximadamente 0.50, mediano
nc_close(nc_arq)

# 5. RJ - Nível presssão - Vento ----
# METAR
df_metar = read_csv("datasets/metar_SBRJ_2025.csv")

df_metar |> 
  ggplot(aes(x = datetime, y = wind_speed)) +
  geom_line(colour="green")

nc_arq = nc_open("datasets/press_rj.nc")

df_era_u <- get_era_var(nc_arq, 'u')
df_era_v <- get_era_var(nc_arq, 'v')

df_era_variavel <- tibble(
  datetime = df_era_u$datetime,
  variavel_simul = sqrt(df_era_u$variavel_simul ** 2 + df_era_v$variavel_simul ** 2)
)

df_era_variavel |> 
  ggplot(aes(x = datetime, y = variavel_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_variavel |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_variavel <- df_era_variavel |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_variavel, df_metar, by = "datetime")

df_comp |> 
  ggplot() +
  geom_line(aes(datetime, wind_speed), colour = "gray") +
  geom_line(aes(datetime, variavel_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$wind_speed, df_comp$variavel_simul)
# Aproximadamente 0.37, ruim
nc_close(nc_arq)
