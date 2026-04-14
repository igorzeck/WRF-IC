# Comparação entre dados simulados WRFOUT com METARs
# RUN: 
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
# Abre um NC qualquer para pegar as unidades de tempo
nc_arq <- nc_open("datasets/wrfout/wrfout_d01_2026-01-24.nc")

valores_t <- ncvar_get(nc_arq, "XTIME")
unid_t <- ncatt_get(nc_arq, "XTIME", "units")$value
unid_t
# "minutes since 2026-01-24 00:00:00"

# Extrai a data de origem
t_ustr <- strsplit(unid_t, " ")
t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])

# valores_t * 60 -> de minutos para segundos
seq_h <- as.POSIXct(valores_t * 60, origin = t_origem, tz = "UTC")

nc_close(nc_arq)

seq_h

## Funções ----
### Coordenadas ----
get_coord_ids <- function(nc_arq) {
  # nc_arq assumido já aberto com nc_open
  # Variáveis:
  # (Tempo, Latitude (X), Longitude (Y))
  # float XLAT(Time, south_north, west_east) ;
  # XLAT:FieldType = 104 ;
  # XLAT:MemoryOrder = "XY " ;
  # XLAT:description = "LATITUDE, SOUTH IS NEGATIVE" ;
  # XLAT:units = "degree_north" ;
  # XLAT:stagger = "" ;
  # XLAT:coordinates = "XLONG XLAT" ;
  # float XLONG(Time, south_north, west_east) ;
  # XLONG:FieldType = 104 ;
  # XLONG:MemoryOrder = "XY " ;
  # XLONG:description = "LONGITUDE, WEST IS NEGATIVE" ;
  # XLONG:units = "degree_east" ;
  # XLONG:stagger = "" ;
  # Pega index da coordenada de interesse (a.k.a de Galeão)
  # **Lat/Lon**: -22.805151097556816, -43.2566277050208
  lat_ref <- -22.805151097556816
  long_ref <- -43.2566277050208
  # Utilizando variáveis XLAT e XLONG procura ids do local
  xlat <- ncvar_get(nc_arq, "XLAT")
  xlong <- ncvar_get(nc_arq, "XLONG")
  
  xlat_diff <- abs(abs(xlat) - abs(lat_ref))
  xlong_diff <- abs(abs(xlong) - abs(long_ref))

  min_lat_id <- which(xlat_diff == min(xlat_diff), arr.ind = TRUE)
  min_lat_id
  min_long_id <- which(xlong_diff == min(xlong_diff), arr.ind = TRUE)
  min_long_id
  which(xlat_diff == min(xlat_diff), arr.ind = TRUE)
  
  # Latitude mínima
  # Coluna 2 é id longitude (para qual o latitude (id 1) é constante)
  # Estritamente falando o id vai ser o mesmo nas duas matrizes!
  iy <- min_lat_id[1,2]
  iy
  min_lat <- xlat[1,iy]
  min_lat
  paste0("Erro absoluto: ", round(abs(min_lat - lat_ref) * g_medio, 3), "m")
  # Longitude mínima
  # Coluna 1 é id latitude (para qual a longitude (id 2) é constante)
  ix <- min_long_id[1,1]
  ix
  min_long <- xlong[ix,1]
  paste0("Erro absoluto: ", round(abs(min_long - long_ref) * g_medio, 3), "m")
  
  return(c(ix, iy))
}

nc_arq <- nc_open("datasets/wrfout/wrfout_d01_2026-01-24.nc")
get_coord_ids(nc_arq)
nc_close(nc_arq)
## Funções ----
# GET para uma variável do WRFOUT
# Por agora abre oa rquiv
get_wrf_var <- function(nc_arq, variavel, n_vert = -1) {
  # Assume arq já aberto com nc_open
  coords <- get_coord_ids(nc_arq)
  
  # Pega variável relevante e retorna seu valor para todos os horários na coordenada relevante
  nc_var <- ncvar_get(nc_arq, variavel)
  # Dimensão geralmente: LAT, LONG, BOTTOM_TOP, TIME
  # [, coords] para pegar todos os "tempos"
  if (n_vert < 0) {
    nc_var[coords[1],coords[2],]
  } else {
    nc_var[coords[1],coords[2],n_vert,]
  }
}

## Análise ----
### Temperatura a 2m ----
# float T2(Time, south_north, west_east) ;
# T2:FieldType = 104 ;
# T2:MemoryOrder = "XY " ;
# T2:description = "TEMP at 2 M" ;
# T2:units = "K" ;
# T2:stagger = "" ;
# T2:coordinates = "XLONG XLAT XTIME" ;
df_var <- tibble(datetime = seq_h)
for (dom in 1:4) {
  path_ <- paste0("datasets/wrfout/wrfout_d0", dom, "_2026-01-24.nc")
  nc_arq <- nc_open(path_)
  nc_var <- get_wrf_var(nc_arq, "T2")
  df_var <- df_var %>% 
    mutate("{paste0('d0',dom)}" := nc_var - 273.15)
  nc_close(nc_arq)
}

df_comp <- left_join(df_var, df_metar, by = "datetime")
df_comp <- df_comp %>% 
  pivot_longer(starts_with("d0"),
               names_to = "dom",
               values_to = "nc_var") %>% 
  relocate(datetime, dom, nc_var)

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = temperature)) +
  geom_line(aes(y = nc_var), color = "green") +
  facet_wrap(~ dom)

# Aparenta haver uma diferença absoluta constante!
# Para o d01
# ... Calcular
# 1º C de diferença!
#### Correlação -----
df_comp %>% 
  group_by(dom) %>% 
  summarise(cor = cor(temperature, nc_var))
# dom     cor
# <chr> <dbl>
# 1 d01   0.851
# 2 d02   0.799
# 3 d03   0.802
# 4 d04   0.732
# O domínio não aparenta estar centrado no aeroporto de Galeão ou alguma outra coisa ocorre!

### Pressão na superfície ----
# float PSFC(Time, south_north, west_east) ;
# PSFC:FieldType = 104 ;
# PSFC:MemoryOrder = "XY " ;
# PSFC:description = "SFC PRESSURE" ;
# PSFC:units = "Pa" ;
# PSFC:stagger = "" ;
# PSFC:coordinates = "XLONG XLAT XTIME" ;
df_var <- tibble(datetime = seq_h)
for (dom in 1:4) {
  path_ <- paste0("datasets/wrfout/wrfout_d0", dom, "_2026-01-24.nc")
  nc_arq <- nc_open(path_)
  nc_var <- get_wrf_var(nc_arq, "PSFC")
  df_var <- df_var %>% 
    mutate("{paste0('d0',dom)}" := nc_var / 1e2)
  nc_close(nc_arq)
}

df_comp <- left_join(df_var, df_metar, by = "datetime")
df_comp <- df_comp %>% 
  pivot_longer(starts_with("d0"),
               names_to = "dom",
               values_to = "nc_var") %>% 
  relocate(datetime, dom, nc_var)

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = pressure)) +
  geom_line(aes(y = nc_var), color = "green") +
  facet_wrap(~ dom)

# O domńio 3 é o mais parelho
#### Correlação -----
df_comp %>% 
  group_by(dom) %>% 
  summarise(cor = cor(pressure, nc_var))
# A tibble: 4 × 2
# dom     cor
# <chr> <dbl>
# 1 d01   0.915
# 2 d02   0.928
# 3 d03   0.925
# 4 d04   0.882
# O mais alto foi para o d01 e d02
# Aparenta reforçar a decentralização do modelo d04

### Intermisssão - coordenadas centrais do d04
# TODO: Verificar erro das corrdenadas para cada domínio
get_coord_ids(nc_open("datasets/wrfout/wrfout_d04_2026-01-24.nc"))
# Para latitude:
# "Erro absoluto: 2556.725m"
# Erro verificado. Para longitude:
# "Erro absoluto: 2563.136m"

# Maior que o erro para d01!

### Velocidade do vento ----
# float U(Time, bottom_top, south_north, west_east_stag) ;
# U:FieldType = 104 ;
# U:MemoryOrder = "XYZ" ;
# U:description = "x-wind component" ;
# U:units = "m s-1" ;
# U:stagger = "X" ;
# U:coordinates = "XLONG_U XLAT_U XTIME" ;
# float V(Time, bottom_top, south_north_stag, west_east) ;
# V:FieldType = 104 ;
# V:MemoryOrder = "XYZ" ;
# V:description = "y-wind component" ;
# V:units = "m s-1" ;
# V:stagger = "Y" ;
# V:coordinates = "XLONG_V XLAT_V XTIME" ;
# float W(Time, bottom_top_stag, south_north, west_east) ;
# W:FieldType = 104 ;
# W:MemoryOrder = "XYZ" ;
# W:description = "z-wind component" ;
# W:units = "m s-1" ;
# W:stagger = "Z" ;
# W:coordinates = "XLONG XLAT XTIME" ;

df_var <- tibble(datetime = seq_h)
for (dom in 1:4) {
  path_ <- paste0("datasets/wrfout/wrfout_d0", dom, "_2026-01-24.nc")
  nc_arq <- nc_open(path_)
  nc_var_u <- get_wrf_var(nc_arq, "U", n_vert = 1)
  nc_var_v <- get_wrf_var(nc_arq, "V", n_vert = 1)
  nc_var_w <- get_wrf_var(nc_arq, "W", n_vert = 1)
  df_var <- df_var %>% 
    mutate("{paste0('d0',dom)}" := sqrt(nc_var_u ** 2 + nc_var_v ** 2 + nc_var_w ** 2))
  nc_close(nc_arq)
}

df_comp <- left_join(df_var, df_metar, by = "datetime")
df_comp <- df_comp %>% 
  pivot_longer(starts_with("d0"),
               names_to = "dom",
               values_to = "nc_var") %>% 
  relocate(datetime, dom, nc_var)

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = wind_speed)) +
  geom_line(aes(y = nc_var), color = "green") +
  facet_wrap(~ dom)

#### Correlação -----
df_comp %>% 
  group_by(dom) %>% 
  summarise(cor = cor(wind_speed, nc_var))
# A tibble: 4 × 2
# dom      cor
# <chr>  <dbl>
# 1 d01   -0.160
# 2 d02    0.533
# 3 d03    0.668
# 4 d04    0.598
# Relativamente decente para todos os domínio menos o d01

### Velocidade do vento ----
# float U(Time, bottom_top, south_north, west_east_stag) ;
# U:FieldType = 104 ;
# U:MemoryOrder = "XYZ" ;
# U:description = "x-wind component" ;
# U:units = "m s-1" ;
# U:stagger = "X" ;
# U:coordinates = "XLONG_U XLAT_U XTIME" ;
# float V(Time, bottom_top, south_north_stag, west_east) ;
# V:FieldType = 104 ;
# V:MemoryOrder = "XYZ" ;
# V:description = "y-wind component" ;
# V:units = "m s-1" ;
# V:stagger = "Y" ;
# V:coordinates = "XLONG_V XLAT_V XTIME" ;
# float W(Time, bottom_top_stag, south_north, west_east) ;
# W:FieldType = 104 ;
# W:MemoryOrder = "XYZ" ;
# W:description = "z-wind component" ;
# W:units = "m s-1" ;
# W:stagger = "Z" ;
# W:coordinates = "XLONG XLAT XTIME" ;
# TODO: TBA
df_var <- tibble(datetime = seq_h)
for (dom in 1:4) {
  path_ <- paste0("datasets/wrfout/wrfout_d0", dom, "_2026-01-24.nc")
  nc_arq <- nc_open(path_)
  nc_var_u <- get_wrf_var(nc_arq, "U", n_vert = 1)
  nc_var_v <- get_wrf_var(nc_arq, "V", n_vert = 1)
  nc_var_w <- get_wrf_var(nc_arq, "W", n_vert = 1)
  nc_var_s <- sqrt(nc_var_u ** 2 + nc_var_v ** 2 + nc_var_w ** 2)
  df_var <- df_var %>% 
    mutate("{paste0('d0',dom)}" := sqrt(nc_var_u ** 2 + nc_var_v ** 2 + nc_var_w ** 2))
  nc_close(nc_arq)
}


df_comp <- left_join(df_var, df_metar, by = "datetime")
df_comp <- df_comp %>% 
  pivot_longer(starts_with("d0"),
               names_to = "dom",
               values_to = "nc_var") %>% 
  relocate(datetime, dom, nc_var)

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = wind_speed)) +
  geom_line(aes(y = nc_var), color = "green") +
  facet_wrap(~ dom)

#### Correlação -----
df_comp %>% 
  group_by(dom) %>% 
  summarise(cor = cor(wind_speed, nc_var))
# A tibble: 4 × 2
# dom      cor
# <chr>  <dbl>
# 1 d01   -0.160
# 2 d02    0.533
# 3 d03    0.668
# 4 d04    0.598

### AFWA_VIS ----
# float AFWA_VIS(Time, south_north, west_east) ;
# AFWA_VIS:FieldType = 104 ;
# AFWA_VIS:MemoryOrder = "XY " ;
# AFWA_VIS:description = "AFWA Diagnostic: Visibility" ;
# AFWA_VIS:units = "m" ;
# AFWA_VIS:stagger = "" ;
# AFWA_VIS:coordinates = "XLONG XLAT XTIME" ;
df_var <- tibble(datetime = seq_h)
for (dom in 1:4) {
  path_ <- paste0("datasets/wrfout/wrfout_d0", dom, "_2026-01-24.nc")
  nc_arq <- nc_open(path_)
  nc_var <- get_wrf_var(nc_arq, "AFWA_VIS")
  # Normalização min-max (para teto de 1e4)
  df_var <- df_var %>% 
    mutate("{paste0('d0',dom)}" :=  (nc_var - min(nc_var))/(max(nc_var) - min(nc_var)) * 1e4)
  nc_close(nc_arq)
}

df_comp <- left_join(df_var, df_metar, by = "datetime")
df_comp <- df_comp %>% 
  pivot_longer(starts_with("d0"),
               names_to = "dom",
               values_to = "nc_var") %>% 
  relocate(datetime, dom, nc_var)

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = visibility)) +
  geom_line(aes(y = nc_var), color = "green") +
  facet_wrap(~ dom)

# Todos são igualmente imprecisos
#### Correlação -----
df_comp %>% 
  group_by(dom) %>% 
  summarise(cor = cor(visibility, nc_var))
# A tibble: 4 × 2
# dom     cor
# <chr> <dbl>
# 1 d01   0.147
# 2 d02   0.167
# 3 d03   0.161
# 4 d04   0.220
# Igualmente imprecisos