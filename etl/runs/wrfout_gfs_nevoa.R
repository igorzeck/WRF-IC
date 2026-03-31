# Comparação entre dados simulados WRFOUT com METARs
# RUN: 
# TODO: Consertar comentários
#  Data: 2026-01-24 (00:00 - 12:00)
#  Local: SBGL
# Setup ----
library(tidyverse)
library(ncdf4)
library(lubridate)

# Variáveis auxilires ----
# Para o WRF (em metros)
r_terra <- 6.370 * 1e6
# Tamanho de 1 grau terrestre médio em metros
g_medio <- r_terra * 2 * pi / 360
g_medio / 1e3
# 111km! Correto.

## Metar ----
df_metar <- read_csv("datasets/metar_SBGL_2026.csv")

df_metar

## Tempo ----
nc_arq <- nc_open("datasets/gfs/gfs_2026-01-24_surface.nc")

valores_t <- ncvar_get(nc_arq, "time")
unid_t <- ncatt_get(nc_arq, "time", "units")$value
unid_t
# "seconds since 1970-01-01 00:00:00.0 0:00"

# Extrai a data de origem
t_ustr <- strsplit(unid_t, " ")
t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])

seq_h <- as.POSIXct(valores_t, origin = t_origem, tz = "UTC")

nc_close(nc_arq)

seq_h

## Funções ----
### Coordenadas ----
get_coord_ids <- function(nc_arq) {
  # nc_arq assumido já aberto com nc_open
  # Variáveis:
  # (Tempo, Latitude (X), Longitude (Y))
  # double latitude(latitude) ;
  # latitude:units = "degrees_north" ;
  # latitude:long_name = "latitude" ;
  # double longitude(longitude) ;
  # longitude:units = "degrees_east" ;
  # longitude:long_name = "longitude" ;
  # Pega index da coordenada de interesse (a.k.a de Galeão)
  # **Lat/Lon**: -22.805151097556816, -43.2566277050208
  lat_ref <- -22.805151097556816
  # Aqui sempre será negativo, então é seguro fazer essa "conversão"
  long_ref <- -43.2566277050208 + 360
  # Utilizando variáveis XLAT e XLONG procura ids do local
  lat <- ncvar_get(nc_arq, "latitude")
  long <- ncvar_get(nc_arq, "longitude")
  
  # Abs deve ser usado com cautela em contextos globais
  lat_diff <- abs(lat - lat_ref)
  # Longitude não tem negativos aqui!
  long_diff <- abs(long - long_ref)

  # Note que é possível dois lugar equidistarem de um ponto
  # Necessário cautela!
  min_lat_id <- which(lat_diff == min(lat_diff))
  min_lat_id
  min_long_id <- which(long_diff == min(long_diff))
  min_long_id
  
  # Latitude mínima
  # Coluna 2 é id longitude (para qual o latitude (id 1) é constante)
  # Estritamente falando o id vai ser o mesmo nas duas matrizes!
  iy <- min_lat_id[1]
  min_lat <- lat[iy]
  
  # print(paste0("Erro absoluto (lat): ", round(abs(min_lat - lat_ref) * g_medio, 3), "m"))
  # "Erro absoluto (lat): 6131.56m"
  
  # Longitude mínima
  # Coluna 1 é id latitude (para qual a longitude (id 2) é constante)
  ix <- min_long_id[1]
  min_long <- long[ix]
  
  # print(paste0("Erro absoluto (long): ", round(abs(min_long - long_ref) * g_medio, 3), "m"))
  # "Erro absoluto (long): 736.851m"
  
  return(c(ix, iy))
}

## Funções ----
# GET para uma variável do GFS
# Por agora abre o arquivo
get_wrf_var <- function(nc_arq, variavel) {
  # Assume arq já aberto com nc_open
  coords <- get_coord_ids(nc_arq)
  
  # Pega variável relevante e retorna seu valor para todos os horários na coordenada relevante
  nc_var <- ncvar_get(nc_arq, variavel)
  # Dimensão geralmente: LAT, LONG, BOTTOM_TOP, TIME
  # [, coords] para pegar todos os "tempos"
  nc_var[coords[1],coords[2],]
}

## Análise ----
### Temperatura 2m acima da superfície ----
# float TMP_2maboveground(time, latitude, longitude) ;
# TMP_2maboveground:_FillValue = 9.999e+20f ;
# TMP_2maboveground:short_name = "TMP_2maboveground" ;
# TMP_2maboveground:long_name = "Temperature" ;
# TMP_2maboveground:level = "2 m above ground" ;
# TMP_2maboveground:units = "K" ;
df_var <- tibble(datetime = seq_h)
path_ <- paste0("datasets/gfs/gfs_2026-01-24_aground.nc")
nc_arq <- nc_open(path_)
nc_var <- get_wrf_var(nc_arq, "TMP_2maboveground")
df_var <- df_var %>% 
  mutate(nc_var = nc_var - 273.15)
nc_close(nc_arq)

df_comp <- left_join(df_var, df_metar, by = "datetime")

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = temperature)) +
  geom_line(aes(y = nc_var), color = "green") 

# Aparenta haver uma diferença absoluta constante!
mean(df_comp$temperature - df_comp$nc_var)
# 2.135602ºC de diferença!

#### Correlação -----
df_comp %>% 
  summarise(cor = cor(temperature, nc_var))
# cor
# <dbl>
#   1 0.996
# Altíssima correlação! Mas ajuste ruim

### Pressão na superfície ----
# float PRES_surface(time, latitude, longitude) ;
# PRES_surface:_FillValue = 9.999e+20f ;
# PRES_surface:short_name = "PRES_surface" ;
# PRES_surface:long_name = "Pressure" ;
# PRES_surface:level = "surface" ;
# PRES_surface:units = "Pa" ;
df_var <- tibble(datetime = seq_h)
path_ <- paste0("datasets/gfs/gfs_2026-01-24_aground.nc")
nc_arq <- nc_open(path_)
nc_var <- get_wrf_var(nc_arq, "PRES_surface")
df_var <- df_var %>% 
  mutate(nc_var = nc_var / 100)
nc_close(nc_arq)

df_comp <- left_join(df_var, df_metar, by = "datetime")

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = pressure)) +
  geom_line(aes(y = nc_var), color = "green")

# O domńio 3 é o mais parelho
#### Correlação -----
df_comp %>% 
  summarise(cor = cor(pressure, nc_var))
# cor
# <dbl>
#   1 0.965
# Basicamente perfeita!

### Velocidade do vento ----
# float UGRD_10maboveground(time, latitude, longitude) ;
# UGRD_10maboveground:_FillValue = 9.999e+20f ;
# UGRD_10maboveground:short_name = "UGRD_10maboveground" ;
# UGRD_10maboveground:long_name = "U-Component of Wind" ;
# UGRD_10maboveground:level = "10 m above ground" ;
# UGRD_10maboveground:units = "m/s" ;
# float VGRD_10maboveground(time, latitude, longitude) ;
# VGRD_10maboveground:_FillValue = 9.999e+20f ;
# VGRD_10maboveground:short_name = "VGRD_10maboveground" ;
# VGRD_10maboveground:long_name = "V-Component of Wind" ;
# VGRD_10maboveground:level = "10 m above ground" ;
# VGRD_10maboveground:units = "m/s" ;
df_var <- tibble(datetime = seq_h)
path_ <- paste0("datasets/gfs/gfs_2026-01-24_aground.nc")
nc_arq <- nc_open(path_)
nc_var_u <- get_wrf_var(nc_arq, "UGRD_10maboveground")
nc_var_v <- get_wrf_var(nc_arq, "VGRD_10maboveground")
df_var <- df_var %>% 
  mutate(nc_var = sqrt(nc_var_u ** 2 + nc_var_v ** 2 ))
nc_close(nc_arq)

df_comp <- left_join(df_var, df_metar, by = "datetime")

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = wind_speed)) +
  geom_line(aes(y = nc_var), color = "green")

#### Correlação -----
df_comp %>% 
  summarise(cor = cor(wind_speed, nc_var))
# A tibble: 1 × 1
# cor
# <dbl>
#   1 -0.916
# Bem alta, mas, negativa?

### Visibilidade ----
# float VIS_surface(time, latitude, longitude) ;
# VIS_surface:_FillValue = 9.999e+20f ;
# VIS_surface:short_name = "VIS_surface" ;
# VIS_surface:long_name = "Visibility" ;
# VIS_surface:level = "surface" ;
# VIS_surface:units = "m" ;
# TODO: Corrigir min max, ele não usa o max nas colunas, mas im o valor máximo 
# Possível (3e4)
df_var <- tibble(datetime = seq_h)
path_ <- paste0("datasets/gfs/gfs_2026-01-24_aground.nc")
nc_arq <- nc_open(path_)
nc_var <- get_wrf_var(nc_arq, "VIS_surface")
df_var <- df_var %>% 
  mutate(nc_var = nc_var)
nc_close(nc_arq)

df_comp <- left_join(df_var, df_metar, by = "datetime")

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = visibility)) +
  geom_line(aes(y = nc_var), color = "green")

# Todos são igualmente imprecisos
#### Correlação -----
df_comp %>% 
  summarise(cor = cor(visibility, nc_var))
# # A tibble: 1 × 1
# cor
# <dbl>
#   1 -0.763
# Relativamente alta!
