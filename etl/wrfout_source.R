# Funções para extração de dados do WRF
# Serve de source para análise geral dos scripts
# Setup ----
library(tidyverse)
library(ncdf4)
library(lubridate)

# Funções ----
## Tempo ----
get_seq_h <- function(nc_arq) {
  # Abre um NC qualquer para pegar as unidades de tempo
  valores_t <- ncvar_get(nc_arq, "XTIME")
  unid_t <- ncatt_get(nc_arq, "XTIME", "units")$value
  unid_t
  # "minutes since 2026-01-24 00:00:00"
  
  # Extrai a data de origem
  t_ustr <- strsplit(unid_t, " ")
  t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])
  
  # valores_t * 60 -> de minutos para segundos
  seq_h <- as.POSIXct(valores_t * 60, origin = t_origem, tz = "UTC")
}

## Coordenadas ----
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

# GET para uma variável do WRFOUT
# Por agora abre o arquivo
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

# GET par uma variável do WRFOUT
# Versão para dados matriciais
get_wrf_var_matriz <- function(nc_arq, variavel) {
  # Para dados com níveis de pressão E tempo
  coords <- get_coord_ids(nc_arq)
  nc_var <- ncvar_get(nc_arq, variavel)
  nc_var[coords[1],coords[2],,]
}

# Equivalente ao clip do numpy (clamp vetor a vetor com reciclagem)
clamp <- function(val, mininmo, maximo) {
  return(pmin(pmax(val, mininmo), maximo))
}
