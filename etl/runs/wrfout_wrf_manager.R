# Comparação entre dados simulados WRFOUT com METARs pelo WRF Manager
# RUN: 
#  Data: 2025-06-04 (00:00 - 23:00)
#  Local: proximidades de SBGL
# :CEN_LAT = -22.80998f ;
# :CEN_LON = -43.25f ;
# :TRUELAT1 = -23.96f ;
# :TRUELAT2 = -23.96f ;

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
df_metar <- read_csv("datasets/metar_SBGL_2025.csv")

df_metar

## Tempo ----
# Abre um NC qualquer para pegar as unidades de tempo
nc_arq <- nc_open("datasets/wrfout/wrf_manager/wrfout_2025-06-04.nc")

valores_t <- ncvar_get(nc_arq, "XTIME")
unid_t <- ncatt_get(nc_arq, "XTIME", "units")$value
unid_t
# "minutes since 2025-06-02 12:00:00"

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

nc_arq <- nc_open("datasets/wrfout/wrf_manager/wrfout_2025-06-04.nc")
get_coord_ids(nc_arq)
# 25 25 (no centro)
nc_close(nc_arq)
## Funções ----
# GET para uma variável do WRFOUT
# Por agora abre oa rquiv
get_wrf_var <- function(nc_arq, variavel, n_vert = -1) {
  # Assume arq já aberto com nc_open
  coords <- get_coord_ids(nc_arq)
  variavel = "T2"
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
path_ <- paste0("datasets/wrfout/wrf_manager/wrfout_2025-06-04.nc")
nc_arq <- nc_open(path_)
nc_var <- get_wrf_var(nc_arq, "T2")
df_var <- df_var %>% 
  mutate("nc_var" = nc_var - 273.15)
nc_close(nc_arq)

df_comp <- left_join(df_var, df_metar, by = "datetime")

#### Gráfico ----
df_comp %>% 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = temperature)) +
  geom_line(aes(y = nc_var), color = "green")

# Aparenta haver uma diferença absoluta constante!
mean(df_comp$temperature - df_comp$nc_var)
# Por volta de -1.4ºC de diferena
#### Correlação -----
df_comp %>% 
  summarise(cor = cor(temperature, nc_var))
# A tibble: 1 × 1
# cor
# <dbl>
#   1 0.981

### AFWA_VIS ----
# float AFWA_VIS(Time, south_north, west_east) ;
# AFWA_VIS:FieldType = 104 ;
# AFWA_VIS:MemoryOrder = "XY " ;
# AFWA_VIS:description = "AFWA Diagnostic: Visibility" ;
# AFWA_VIS:units = "m" ;
# AFWA_VIS:stagger = "" ;
# AFWA_VIS:coordinates = "XLONG XLAT XTIME" ;
df_var <- tibble(datetime = seq_h)
path_ <- paste0("datasets/wrfout/wrf_manager/wrfout_2025-06-04.nc")
nc_arq <- nc_open(path_)
nc_var <- get_wrf_var(nc_arq, "AFWA_VIS")
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
# A tibble: 1 × 1
# cor
# <dbl>
#   1 0.553
# Espetacular!
df_comp %>% 
  select(visibility) %>% 
  view()
