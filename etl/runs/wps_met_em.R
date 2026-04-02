# Comparação da acurácia dos dados gerado pelo met_em (previsões interpoladas pelo met_em*)
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
nc_arq <- nc_open("datasets/wrfout/wrf_manager_like_met_em/met_em.d01_2026-01-24.nc")
# Mais fácil sabendo previamente a data
# Usando timezeone de UTC por padronização! Mas, teoricamente seria nossa data
t_origem <- as.POSIXct("2026-01-24 00:00:00", tz = "UTC")
seq_h <- seq(from = t_origem, 
                by = "3 hours", 
                length.out = 5)

nc_close(nc_arq)

seq_h

## Funções ----
### Coordenadas ----
get_coord_ids <- function(nc_arq) {
  # nc_arq assumido já aberto com nc_open
  # Variáveis:
  # (Tempo, Latitude (X), Longitude (Y))
  # float CLONG(Times, south_north, west_east) ;
  # CLONG:units = "degrees longitude" ;
  # CLONG:FieldType = 104 ;
  # CLONG:MemoryOrder = "XY " ;
  # CLONG:description = "Computational longitude on mass grid" ;
  # CLONG:stagger = "M" ;
  # CLONG:sr_x = 1 ;
  # CLONG:sr_y = 1 ;
  # float CLAT(Times, south_north, west_east) ;
  # CLAT:units = "degrees latitude" ;
  # CLAT:FieldType = 104 ;
  # CLAT:MemoryOrder = "XY " ;
  # CLAT:description = "Computational latitude on mass grid" ;
  # CLAT:stagger = "M" ;
  # CLAT:sr_x = 1 ;
  # CLAT:sr_y = 1 ;
  # Pega index da coordenada de interesse (a.k.a de Galeão)
  # **Lat/Lon**: -22.805151097556816, -43.2566277050208
  lat_ref <- -22.805151097556816
  long_ref <- -43.2566277050208
  # Utilizando variáveis clat e clong procura ids do local
  clat <- ncvar_get(nc_arq, "CLAT")
  clong <- ncvar_get(nc_arq, "CLONG")
  
  clat_diff <- abs(abs(clat) - abs(lat_ref))
  clong_diff <- abs(abs(clong) - abs(long_ref))
  
  min_lat_id <- which(clat_diff == min(clat_diff), arr.ind = TRUE)
  min_lat_id
  min_long_id <- which(clong_diff == min(clong_diff), arr.ind = TRUE)
  min_long_id
  which(clat_diff == min(clat_diff), arr.ind = TRUE)
  
  # Latitude mínima
  # Coluna 2 é id longitude (para qual o latitude (id 1) é constante)
  # Estritamente falando o id vai ser o mesmo nas duas matrizes!
  iy <- min_lat_id[1,2]
  # Pega do último id de tempo, já que não faz diferença
  min_lat <- clat[1,iy,5]
  min_lat
  paste0("Erro absoluto: ", round(abs(min_lat - lat_ref) * g_medio, 3), "m")
  # Longitude mínima
  # Coluna 1 é id latitude (para qual a longitude (id 2) é constante)
  ix <- min_long_id[1,1]
  ix
  min_long <- clong[ix,1,5]
  paste0("Erro absoluto: ", round(abs(min_long - long_ref) * g_medio, 3), "m")
  
  return(c(ix, iy))
}

nc_arq <- nc_open("datasets/wrfout/wrf_manager_like_met_em/met_em.d01_2026-01-24.nc")
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
### Temperatura ----
# float TT(Times, num_metgrid_levels, south_north, west_east) ;
# TT:units = "K" ;
# TT:FieldType = 104 ;
# TT:MemoryOrder = "XYZ" ;
# TT:description = "Temperature" ;
# TT:stagger = "M" ;
# TT:sr_x = 1 ;
# TT:sr_y = 1 ;
df_var <- tibble(datetime = c(seq_h))
for (dom in 1:4) {
  path_ <- paste0("datasets/wrfout/wrf_manager_like_met_em/met_em.d0", dom, "_2026-01-24.nc")
  nc_arq <- nc_open(path_)
  nc_var <- get_wrf_var(nc_arq, "TT", 1)
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

# Aparenta haver uma diferença absoluta constante de 2ºC
#### Correlação -----
df_comp %>% 
  group_by(dom) %>% 
  summarise(cor = cor(temperature, nc_var))
# Correlação altíssima
# A tibble: 4 × 2
# dom     cor
# <chr> <dbl>
#   1 d01   0.998
# 2 d02   0.998
# 3 d03   0.998
# 4 d04   0.998
# As melhores são para o domínio 1...

### Pressão na superfície ----
# float PSFC(Times, south_north, west_east) ;
# PSFC:units = "Pa" ;
# PSFC:FieldType = 104 ;
# PSFC:MemoryOrder = "XY " ;
# PSFC:description = "Surface Pressure" ;
# PSFC:stagger = "M" ;
# PSFC:sr_x = 1 ;
# PSFC:sr_y = 1 ;
df_var <- tibble(datetime = c(seq_h))
for (dom in 1:4) {
  path_ <- paste0("datasets/wrfout/wrf_manager_like_met_em/met_em.d0", dom, "_2026-01-24.nc")
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
d
# Diferença abasoluta constante de 5 Pa

# O domńio 3 é o mais parelho
#### Correlação -----
df_comp %>% 
  group_by(dom) %>% 
  summarise(cor = cor(pressure, nc_var))
# A tibble: 4 × 2
# dom     cor
# <chr> <dbl>
#   1 d01   0.967
# 2 d02   0.967
# 3 d03   0.967
# 4 d04   0.969
# Exceto pelo domínio 4 todo acima de 90%!

### Velocidade e direção do vento
# Aparentemente não há!