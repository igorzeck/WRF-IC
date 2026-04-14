# Funções para extração de dados do WRF
# Serve de source para análise geral dos scripts
# Setup ----
library(tidyverse)
library(ncdf4)
library(lubridate)

# Funções ----
## Tempo ----
get_seq_h <- function(nc_arq, multiplo) {
  valores_t <- ncvar_get(nc_arq, "XTIME")
  unid_t <- ncatt_get(nc_arq, "XTIME", "units")$value
  
  # Extrai a data de origem
  t_ustr <- strsplit(unid_t, " ")
  t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])
  
  # valores_t * multiplo -> de unidade para segundos
  seq_h <- as.POSIXct(valores_t * multiplo, origin = t_origem, tz = "UTC")
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


# GET para visiiblidade pela fórmula de visibilidade de Koschmeider
calc_wrf_vis <- function(nc_arq, seq_h) {
  # -- Detalhes da fórmula comentados em etl/calcular_vis.R --
  
  # Variáveis "core" ----
  # Etapa 1: Coleta variáveis core
  # t2 = nc.variables['T2'][:, iy, ix] if 'T2' in nc.variables else None
  # q2 = nc.variables['Q2'][:, iy, ix] if 'Q2' in nc.variables else None
  # psfc = nc.variables['PSFC'][:, iy, ix] if 'PSFC' in nc.variables else None
  # u10 = nc.variables['U10'][:, iy, ix] if 'U10' in nc.variables else None
  # v10 = nc.variables['V10'][:, iy, ix] if 'V10' in nc.variables else None
  # sstsk = nc.variables['SSTSK'][:, iy, ix] if 'SSTSK' in nc.variables else t2
  
  ## Nível único ----
  t2 <- get_wrf_var(nc_arq, "T2")
  q2 <- get_wrf_var(nc_arq, "Q2")
  psfc <- get_wrf_var(nc_arq, "PSFC")
  u10 <- get_wrf_var(nc_arq, "U10")
  v10 <- get_wrf_var(nc_arq, "V10")
  sstsk <- get_wrf_var(nc_arq, "SSTSK")
  # TODO: Mover todas as variáveis coletadas diretamente pra cá
  ## Por nível de pressão ----
  # ...
  
  # Cálculo para RH_2m ----
  # Umidade relativa
  es <- 6.112 * exp((17.67 * (t2 - 273.15)) / (t2 - 29.65))
  es_pa <- es * 100.0
  qs <- (0.622 * es_pa) / (psfc - 0.378 * es_pa)
  rh_2m <- clamp(q2 / qs, 0.0, 1.0) * 100.0
  rh_2m
  
  # Dewpt 
  # Ponto de orvalho
  t2_c <- t2 - 273.15 # K -> ºC
  rh_frac <- clamp(rh_2m / 100.0, 1e-6, 1.0)
  ln_rh <- log(rh_frac)
  alpha <- (17.625 * t2_c) / (243.04 + t2_c) + ln_rh
  dewpt <- (243.04 * alpha) / (17.625 - alpha)
  dewpt
  
  # Wspd
  # Wind Speed - Velocidade do Vento
  wspd <- sqrt(u10 ** 2 + v10**2)
  wspd
  
  # Vis_WRF (simplificado)
  # O cálulo até poderia ser feito utilizando as funções
  # mas, decidiu-se pegar diretamente aqui o range de níveis
  coords <- get_coord_ids(nc_arq)
  # Pega variável relevante e retorna seu valor para todos os horários na coordenada relevante
  nc_var <- ncvar_get(nc_arq, 'PH')
  
  
  
  dim(nc_var)
  ph <- get_wrf_var_matriz(nc_arq, 'PH')
  phb <- get_wrf_var_matriz(nc_arq, 'PHB')
  z_full <- (ph + phb) / 9.81
  # dz = z_full[:, 1:] - z_full[:, :-1] for reordenadoa aqui (pela ordem que o ncdf4 abre)
  dz <- z_full[2:dim(z_full)[1],] - z_full[1:(dim(z_full)[1] - 1),]
  z_mid <- 0.5 * (z_full[2:dim(z_full)[1],] + z_full[1:(dim(z_full)[1] - 1),])
  z_sfc <- z_full[1,]
  z_agl <- z_mid - z_sfc
  mask_100m <- z_agl <= 120.0
  mask_100m[0,] <- T
  
  p_3d <- get_wrf_var_matriz(nc_arq, 'P') + get_wrf_var_matriz(nc_arq, 'PB')
  # + 300 pois o base_temp está como 290 nos arquivos (deveria ser 300!)
  theta_3d <- get_wrf_var_matriz(nc_arq, 'T') + 300.0
  tk_3d <- theta_3d * ((p_3d / 100000.0) ** 0.2854)
  rho_3d <- p_3d / (287.05 * tk_3d)
  
  qc <- get_wrf_var_matriz(nc_arq, 'QCLOUD')
  qr <- get_wrf_var_matriz(nc_arq, 'QRAIN')
  qi <- get_wrf_var_matriz(nc_arq, 'QICE')
  qs_snow <- get_wrf_var_matriz(nc_arq, 'QSNOW')
  
  beta_hydro <- (
    130.0 * (rho_3d * qc * 1000.0)**0.85 +
      1.2 * (rho_3d * qr * 1000.0)**0.75 +
      150.0 * (rho_3d * qi * 1000.0)**1.0 +
      9.0 * (rho_3d * qs_snow * 1000.0)**0.80 + 0.012
  )
  # beta_col = np.sum(beta_hydro * dz * mask_100m, axis=1) / np.sum(dz * mask_100m, axis=1)
  # Como a soma é row-wise... em R seria:
  beta_col <- colSums(beta_hydro * dz * mask_100m) / colSums(dz * mask_100m)
  # Teoricamente seria colRows em uma transformação direta, mas as dimensões são reversas aqui!
  
  # NOTE: "rh_2m / 100.0 if rh_2m is not None else np.ones_like(t2) * 0.5" Tem o if ignorado aqui
  rh_sfc <- rh_2m / 100.0
  growth <- (1.0 / (1.0 - clamp(rh_sfc, 0.0, 0.999)))**0.5
  beta_marine <- 0.08 * (rh_sfc**3)
  beta_fog <- if_else(rh_sfc > 0.85, 0.01 * exp(53.0 * (rh_sfc - 0.85)), 0.0)
  beta_total <- beta_col * growth + beta_marine + beta_fog
  vis_wrf <- clamp(3.5 / beta_total, 0.0, 20.0)
  
  return(vis_wrf * 1e3)
}
