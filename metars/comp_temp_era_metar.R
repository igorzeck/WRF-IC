# Comparação da temperatura dos METARs e arquivos Grib
# (em NetCDF4 - NC) para as regiões relevantes
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

# GET para a temperatura (do ERA5)
get_era_temp <- function(arq, tipo) {
  # As variávies estão divididas em 2
  # var_0001 para jan - nov e var_0005 para dez
  if (tipo == 'press') {
    era_temp_jan_nov = ncvar_get(nc_arq, "t_0001")
    era_temp_dez = ncvar_get(nc_arq, "t_0005")
  }
  if (tipo == 'unico') {
    era_temp_jan_nov = ncvar_get(nc_arq, "t2m_0001")
    era_temp_dez = ncvar_get(nc_arq, "t2m_0005")
  }
  
  # Como são complementares é possível utilziar o coalesce (pega primeiro não NA dos dois)
  era_temp <- coalesce(era_temp_jan_nov, era_temp_dez)
  
  # Conversão para Celsius
  era_temp <- as.vector(era_temp - 273.15)
  
  # Retorno
  tibble(
    datetime = seq_h,
    temp_simul = era_temp
  )
}

# 1. BA - Nível único ----
# METAR
df_metar = read_csv("datasets/metar_SBSV_2025.csv")

df_metar |> 
  ggplot(aes(datetime, temperature)) +
  geom_line()

# NC Único
nc_arq = nc_open("datasets/unico_ba.nc")

df_era_temp <- get_era_temp(nc_arq, 'unico')

df_era_temp |> 
  ggplot(aes(x = datetime, y = temp_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_temp |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_temp <- df_era_temp |> 
  drop_na()

# Junção dos tibbles
# Por algum motivio 2-25-03-10 00:00:00 é uma entrada dupĺicada
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

glimpse(df_comp)

# 1.1 Comparação ----
df_comp |> 
  ggplot() +
  geom_line(aes(datetime, temperature), colour = "gray") +
  geom_line(aes(datetime, temp_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$temperature, df_comp$temp_simul)
# Aproximadamente 0.81, relativamente preciso... Pode ser relacionado a um arredondamento

# 2. BA - Nível de pressão ----
nc_arq = nc_open("datasets/press_ba.nc")
df_era_temp <- get_era_temp(nc_arq, 'press')

df_era_temp |> 
  ggplot(aes(x = datetime, y = temp_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_temp |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_temp <- df_era_temp |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

glimpse(df_comp)

# 2.1 Comparação ----
df_comp |> 
  ggplot() +
  geom_line(aes(datetime, temperature), colour = "gray") +
  geom_line(aes(datetime, temp_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$temperature, df_comp$temp_simul)
# Aproximadamente 0.74, médio-alto...
# Novamente, pode ser relacioado a uma quetão de arredondamentos

# 3. RJ Dumont - Nível único ----
# METAR
df_metar = read_csv("datasets/metar_SBRJ_2025.csv")

df_metar |> 
  ggplot(aes(datetime, temperature)) +
  geom_line()

# NC Único
nc_arq = nc_open("datasets/unico_rj.nc")
df_era_temp <- get_era_temp(nc_arq, 'unico')

df_era_temp |> 
  ggplot(aes(x = datetime, y = temp_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_temp |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_temp <- df_era_temp |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

glimpse(df_comp)

# 3.1 Comparação ----
df_comp |> 
  ggplot() +
  geom_line(aes(datetime, temperature), colour = "gray") +
  geom_line(aes(datetime, temp_simul), colour = "green")

# Métrica de correlação
cor(df_comp$temperature, df_comp$temp_simul)
# Aproximadamente 0.93! Bem alta!

# 4. RJ Dumont - Nível de pressão ----
# Galeão está a aproximadamente 15km, relativamente perto,
# mas dentro de um grid, o que pode servir para comparar a interpolação
nc_arq = nc_open("datasets/press_rj.nc")
df_era_temp <- get_era_temp(nc_arq, 'press')

# Verifica se há NAs
df_era_temp |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_temp <- df_era_temp |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

glimpse(df_comp)

# 4.1 Comparação ----
df_comp |> 
  ggplot() +
  geom_line(aes(datetime, temperature), colour = "gray") +
  geom_line(aes(datetime, temp_simul), colour = "green")

# Os valores simulados são mais estáveis e menos caóticos!
# Métrica de correlação
cor(df_comp$temperature, df_comp$temp_simul)
# Aproximadamente 0.92, alto! Basicamente semelhante ao de nível único

# 5. RJ Galeão - Níveis únicos de Dumont
# METAR
df_metar = read_csv("datasets/metar_SBGL_2025.csv")

df_metar |> 
  ggplot(aes(datetime, temperature)) +
  geom_line()

# NC Único
nc_arq = nc_open("datasets/unico_rj.nc")
df_era_temp <- get_era_temp(nc_arq, 'unico')

df_era_temp |> 
  ggplot(aes(x = datetime, y = temp_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_temp |> 
  filter(if_any(everything(), is.na))


# Se houver NAs são dropados
df_era_temp <- df_era_temp |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

glimpse(df_comp)

# 5.1 Comparação ----
df_comp |> 
  ggplot() +
  geom_line(aes(datetime, temperature), colour = "gray") +
  geom_line(aes(datetime, temp_simul), colour = "green")

# Métrica de correlação
cor(df_comp$temperature, df_comp$temp_simul)
# Aproximadamente 0.97%! Bem alta, mais até do que o ponto em Santos Dumont!

# 6. RJ Galeão - Níveis únicos de Dumont
# METAR
df_metar = read_csv("datasets/metar_SBGL_2025.csv")

df_metar |> 
  ggplot(aes(datetime, temperature)) +
  geom_line()

# NC Único
nc_arq = nc_open("datasets/press_rj.nc")
df_era_temp <- get_era_temp(nc_arq, 'press')

df_era_temp |> 
  ggplot(aes(x = datetime, y = temp_simul)) +
  geom_line(colour="green")

# Verifica se há NAs
df_era_temp |> 
  filter(if_any(everything(), is.na))

# Se houver NAs são dropados
df_era_temp <- df_era_temp |> 
  drop_na()

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

glimpse(df_comp)

# 6.1 Comparação ----
df_comp |> 
  ggplot() +
  geom_line(aes(datetime, temperature), colour = "gray") +
  geom_line(aes(datetime, temp_simul), colour = "green")

# Métrica de correlação
cor(df_comp$temperature, df_comp$temp_simul)
# Aproximadamente 0.89%! Ainda bem alta levando em conta a distância de 15km!

# 7. Interlúdio ----
# A correlação em BA foi relaivamente alta para níveis únicos, sendo de 81.0! 
# No RJ os valores foram mais altos (> 90%), por questões de arredondamente (provavlmente)
# Os valores de Galeão para o ERA-5 de Santos Dumont foram mais altos

# 8. Comparação METARs de BA e ERA-5 de RJ
# Comparação de controle
# METAR
df_metar = read_csv("datasets/metar_SBSV_2025.csv")

# NC Único
# Único, por ser mais preciso
nc_arq = nc_open("datasets/unico_rj.nc")
df_era_temp <- get_era_temp(nc_arq, 'unico')

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

cor(df_comp$temperature, df_comp$temp_simul)
# 54% de correlação. Usando esse valor de base, temos um valor de controle
# para locais próximos e costeiros

# 8. Comparação METARs de RJ e ERA-5 de BA
# Comparação de controle
# METAR
df_metar = read_csv("datasets/metar_SBRJ_2025.csv")

# NC Único
# Único, por ser mais preciso
nc_arq = nc_open("datasets/unico_ba.nc")
df_era_temp <- get_era_temp(nc_arq, 'unico')

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

cor(df_comp$temperature, df_comp$temp_simul)
# 65% de correlação. Usando esse valor de base, temos um valor de controle
# para locais próximos e costeiros

# 9. Comparação METARs de RJ (Galeão) e ERA-5 de BA
# Comparação de controle
# METAR
df_metar = read_csv("datasets/metar_SBGL_2025.csv")

# NC Único
# Único, por ser mais preciso
nc_arq = nc_open("datasets/unico_ba.nc")
df_era_temp <- get_era_temp(nc_arq, 'unico')

# Junção dos tibbles
df_comp <- left_join(df_era_temp, df_metar, by = "datetime")

cor(df_comp$temperature, df_comp$temp_simul)
# 65% de correlação. Usando esse valor de base, temos um valor de controle
# para locais próximos e costeiros

# Por inspecção: se menor que 70% são regiões distintas