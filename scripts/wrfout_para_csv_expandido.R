# Script de conversão de arquivos WRFOUT (NetCDF4) para CSV
# Setup ----
library(tidyverse)
library(ncdf4)
library(lubridate)
library(janitor)

# Config ----
# Coordenada de referência (Lat, Lon)
# Santos
# LAT_REF  <- -23.993138612371308
# LONG_REF <- -46.3068174272036
# Galeão
LAT_REF  <- -22.805151097556816
LONG_REF <- -43.2566277050208

DIR_DADOS <- "/home/rf/WD/WRF/test/em_real"
PATTERN_DADOS <- "^wrfout_d04_2026-06-2"
ARQ_ALVOS <- "scripts/var_targets.txt"
ARQ_SAIDA <- "datasets/wrf_raw_out2.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) DIR_DADOS <- args[1]
if (length(args) >= 2) PATTERN_DADOS <- args[2]
if (length(args) >= 3) ARQ_SAIDA <- args[3]

# Obter lista de arquivos
arquivos_wrf <- list.files(DIR_DADOS, pattern = PATTERN_DADOS, full.names = TRUE)
if (length(arquivos_wrf) == 0) stop("Nenhum arquivo encontrado em ", DIR_DADOS, " com padrao ", PATTERN_DADOS)
cat("Encontrados", length(arquivos_wrf), "arquivos para processar.\n")


## Funções ----
### Coordenadas ----
get_coord_ids <- function(nc_arq, lat_ref, long_ref) {
  # nc_arq assumido já aberto com nc_open
  xlat <- ncvar_get(nc_arq, "XLAT")
  xlong <- ncvar_get(nc_arq, "XLONG")
  
  xlat_diff <- abs(abs(xlat) - abs(lat_ref))
  xlong_diff <- abs(abs(xlong) - abs(long_ref))

  min_lat_id <- which(xlat_diff == min(xlat_diff), arr.ind = TRUE)
  min_long_id <- which(xlong_diff == min(xlong_diff), arr.ind = TRUE)

  # Coluna 2 é id longitude (para qual o latitude, id 1, é constante)
  iy <- min_lat_id[1, 2]
  min_lat <- if (length(dim(xlat)) == 3) xlat[1, iy, 1] else xlat[1, iy]

  # Coluna 1 é id latitude (para qual a longitude, id 2, é constante)
  ix <- min_long_id[1, 1]
  min_long <- if (length(dim(xlong)) == 3) xlong[ix, 1, 1] else xlong[ix, 1]

  cat("Ponto de grade mais próximo -> lat:", min_lat, "lon:", min_long, "\n")
  return(c(ix, iy))
}

### Extração genérica ----
# GET para uma variável do WRFOUT na coordenada de referência.
# n_vert < 0  -> variável 2D (LAT, LONG, TIME)
# n_vert >= 1 -> variável 3D (LAT, LONG, BOTTOM_TOP, TIME), nível específico
get_wrf_var <- function(nc_arq, coords, variavel, n_vert = -1) {
  nc_var <- ncvar_get(nc_arq, variavel)
  dims <- length(dim(nc_var))
  if (n_vert < 0) {
    if (dims == 2) return(nc_var[coords[1], coords[2]])
    if (dims == 3) return(nc_var[coords[1], coords[2], ])
  } else {
    if (dims == 3) return(nc_var[coords[1], coords[2], n_vert])
    if (dims == 4) return(nc_var[coords[1], coords[2], n_vert, ])
  }
}

### Variáveis derivadas ----
# Pressão de vapor de saturação (hPa) - aproximação de Magnus/Bolton
tens_vapor_sat <- function(temp_c) {
  6.112 * exp((17.67 * temp_c) / (temp_c + 243.5))
}

# Umidade relativa (%) a partir da razão de mistura (kg/kg), temp (K) e pressão (Pa)
calc_rh <- function(qv, temp_k, pres_pa) {
  temp_c <- temp_k - 273.15
  pres_hpa <- pres_pa / 100
  es <- tens_vapor_sat(temp_c)
  e <- (qv * pres_hpa) / (0.622 + qv)
  rh <- 100 * (e / es)
  pmin(pmax(rh, 0), 100)
}

# Ponto de orvalho (°C) a partir da razão de mistura (kg/kg) e pressão (Pa)
calc_dewpoint <- function(qv, pres_pa) {
  pres_hpa <- pres_pa / 100
  e <- (qv * pres_hpa) / (0.622 + qv)
  (243.5 * log(e / 6.112)) / (17.67 - log(e / 6.112))
}

# Temperatura real (K) a partir da temp. potencial perturbada (T) e pressão total (Pa)
# theta = T + 300 (T0 padrão do WRF); T_real = theta * (p/p0)^(R/cp)
calc_temp_real <- function(t_pert, p_total) {
  theta <- t_pert + 300
  theta * (p_total / 100000)^0.2854
}

calc_wind_speed <- function(u, v) sqrt(u^2 + v^2)

# Direção meteorológica do vento (de onde vem, 0-360°)
calc_wind_dir <- function(u, v) {
  (270 - (atan2(v, u) * 180 / pi)) %% 360
}

# Água de nuvem/precipitação integrada na coluna (kg/m^2), via peso de massa seca
# dp_k = (MU + MUB) * DNW_k  (Pa por camada); integral = sum(q_k * dp_k) / g
calc_agua_coluna <- function(nc_arq, coords) {
  q_cld <- ncvar_get(nc_arq, "QCLOUD")
  dims <- length(dim(q_cld))
  
  if (dims == 3) {
    q_total <- q_cld[coords[1], coords[2], ] +
      ncvar_get(nc_arq, "QRAIN")[coords[1], coords[2], ] +
      ncvar_get(nc_arq, "QICE")[coords[1], coords[2], ] +
      ncvar_get(nc_arq, "QSNOW")[coords[1], coords[2], ] +
      ncvar_get(nc_arq, "QGRAUP")[coords[1], coords[2], ]
    mu <- ncvar_get(nc_arq, "MU")[coords[1], coords[2]]
    mub <- ncvar_get(nc_arq, "MUB")[coords[1], coords[2]]
    dnw <- ncvar_get(nc_arq, "DNW")
    dp <- abs((mu + mub) * dnw)
    return(sum(q_total * dp) / 9.81)
  } else {
    q_total <- q_cld[coords[1], coords[2], , ] +
      ncvar_get(nc_arq, "QRAIN")[coords[1], coords[2], , ] +
      ncvar_get(nc_arq, "QICE")[coords[1], coords[2], , ] +
      ncvar_get(nc_arq, "QSNOW")[coords[1], coords[2], , ] +
      ncvar_get(nc_arq, "QGRAUP")[coords[1], coords[2], , ]
    mu <- ncvar_get(nc_arq, "MU")[coords[1], coords[2], ]
    mub <- ncvar_get(nc_arq, "MUB")[coords[1], coords[2], ]
    dnw <- ncvar_get(nc_arq, "DNW")
    
    n_t <- length(mu)
    resultado <- numeric(n_t)
    for (t in seq_len(n_t)) {
      dp <- abs((mu[t] + mub[t]) * dnw)
      resultado[t] <- sum(q_total[, t] * dp) / 9.81
    }
    return(resultado)
  }
}

### Mapeamento alvo -> extrator ----
# Cada entrada de var_targets.txt é mapeada para a variável (ou combinação de
# variáveis) mais próxima disponível em var_names.txt. Para variáveis com
# múltiplos níveis, é usado o nível mais próximo do solo (NIVEL_SOLO).
#
# ATENÇÃO (limitações conhecidas):
# - U/V (3D) estão em grade staggered; usar o índice de coordenada da grade de
#   massa é uma aproximação (mesmo ponto de grade, sem interpolação).
# - Itens marcados como NA_real_ não têm variável equivalente direta no wrfout
#   e exigiriam cálculo adicional (ex.: ascensão de parcela, perfil vertical de
#   nível de congelamento, detecção de base de nuvem).
###
extratores <- list(
  "Dew point temperature (HTGL)" = function(nc, co) {
    calc_dewpoint(get_wrf_var(nc, co, "Q2"), get_wrf_var(nc, co, "PSFC"))
  },
  "Geopotential height (0DEG)" = function(nc, co) NA_real_, # TODO: requer perfil vertical T x altura
  "Geopotential height (CEIL)" = function(nc, co) NA_real_, # TODO: requer detecção de base de nuvem (QCLOUD/CLDFRA)
  "Geopotential height (HTFL)" = function(nc, co) NA_real_, # TODO: nível mais alto de congelamento, idem 0DEG
  "Geopotential height (SFC)" = function(nc, co) get_wrf_var(nc, co, "HGT"),
  "High level cloud cover" = function(nc, co) {
    n_niv <- nc$var[["CLDFRA"]]$varsize[3]
    get_wrf_var(nc, co, "CLDFRA", n_niv)
  },
  "Low level cloud cover" = function(nc, co) get_wrf_var(nc, co, "CLDFRA", NIVEL_SOLO),
  "Mean sea level pressure (ETA model)" = function(nc, co) get_wrf_var(nc, co, "AFWA_MSLP"),
  "Mid level cloud cover" = function(nc, co) {
    n_niv <- nc$var[["CLDFRA"]]$varsize[3]
    get_wrf_var(nc, co, "CLDFRA", round(n_niv / 2))
  },
  "Parcel lifted index (to 500 hPa)" = function(nc, co) NA_real_, # TODO: ascensão de parcela até 500hPa
  "Precipitable water" = function(nc, co) get_wrf_var(nc, co, "AFWA_PWAT"),
  "Pressure" = function(nc, co) get_wrf_var(nc, co, "PSFC"),
  "Probability of precipitation" = function(nc, co) NA_real_, # N/A: rodada determinística, sem PoP
  "Relative humidity (HTGL)" = function(nc, co) {
    calc_rh(get_wrf_var(nc, co, "Q2"), get_wrf_var(nc, co, "T2"), get_wrf_var(nc, co, "PSFC"))
  },
  "Relative humidity (HYBL)" = function(nc, co) {
    p_total <- get_wrf_var(nc, co, "P", NIVEL_SOLO) + get_wrf_var(nc, co, "PB", NIVEL_SOLO)
    t_real <- calc_temp_real(get_wrf_var(nc, co, "T", NIVEL_SOLO), p_total)
    calc_rh(get_wrf_var(nc, co, "QVAPOR", NIVEL_SOLO), t_real, p_total)
  },
  "Specific humidity (HTGL)" = function(nc, co) get_wrf_var(nc, co, "Q2"),
  "Specific humidity (HYBL)" = function(nc, co) get_wrf_var(nc, co, "QVAPOR", NIVEL_SOLO),
  "Specific humidity (SPDY)" = function(nc, co) get_wrf_var(nc, co, "QVAPOR", NIVEL_SOLO),
  "Surface lifted index" = function(nc, co) NA_real_, # TODO: ascensão de parcela de superfície
  "Surface roughness" = function(nc, co) NA_real_, # Indisponível no wrfout (sem variável Z0/ZNT)
  "Temperature (HTGL)" = function(nc, co) get_wrf_var(nc, co, "T2"),
  "Temperature (SFC)" = function(nc, co) get_wrf_var(nc, co, "TSK"),
  "Temperature - sea Temperature" = function(nc, co) get_wrf_var(nc, co, "SST"),
  "Total cloud cover" = function(nc, co) {
    cldfra <- ncvar_get(nc, "CLDFRA")
    if (length(dim(cldfra)) == 3) {
      max(cldfra[co[1], co[2], ])
    } else {
      apply(cldfra[co[1], co[2], , ], 2, max)
    }
  },
  "Total column-integrated cloud water" = function(nc, co) calc_agua_coluna(nc, co),
  "Water temperature" = function(nc, co) get_wrf_var(nc, co, "SSTSK"),
  "u-component of wind (HTGL)" = function(nc, co) get_wrf_var(nc, co, "U10"),
  "u-component of wind (SPDY)" = function(nc, co) get_wrf_var(nc, co, "U", NIVEL_SOLO),
  "v-component of wind (HTGL)" = function(nc, co) get_wrf_var(nc, co, "V10"),
  "v-component of wind (SPDY)" = function(nc, co) get_wrf_var(nc, co, "V", NIVEL_SOLO),
  "wind_angle" = function(nc, co) {
    calc_wind_dir(get_wrf_var(nc, co, "U10"), get_wrf_var(nc, co, "V10"))
  },
  "wind_speed" = function(nc, co) {
    calc_wind_speed(get_wrf_var(nc, co, "U10"), get_wrf_var(nc, co, "V10"))
  }
)

# Conversão ----
alvos <- read_lines(ARQ_ALVOS)
todos_dados <- list()

# Convenção WRF: ZNU (eta, mass levels) decresce de ~1 (solo) para ~0 (topo),
# logo o índice 1 do bottom_top é sempre o nível mais próximo da superfície.
NIVEL_SOLO <- 1

for (arquivo in arquivos_wrf) {
  cat("\nProcessando arquivo:", basename(arquivo), "\n")
  nc_arq <- nc_open(arquivo)
  
  # Tempo
  valores_t <- ncvar_get(nc_arq, "XTIME")
  unid_t <- ncatt_get(nc_arq, "XTIME", "units")$value
  t_ustr <- strsplit(unid_t, " ")
  t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])
  seq_h <- as.POSIXct(valores_t * 60, origin = t_origem, tz = "UTC")
  
  coords <- get_coord_ids(nc_arq, LAT_REF, LONG_REF)
  
  tibble_csv <- tibble(datetime = c(seq_h))
  
  for (alvo in alvos) {
    extrator <- extratores[[alvo]]
    if (is.null(extrator)) {
      next
    }
    valor <- tryCatch(
      extrator(nc_arq, coords),
      error = function(e) {
        cat("  -> Erro ao extrair", alvo, ":", conditionMessage(e), "\n")
        rep(NA_real_, length(seq_h))
      }
    )
    tibble_csv <- tibble_csv %>%
      mutate("{alvo}" := valor)
  }
  
  nc_close(nc_arq)
  todos_dados[[length(todos_dados) + 1]] <- tibble_csv
}

# Combinar e salvar
df_final <- bind_rows(todos_dados) %>%
  clean_names()

glimpse(df_final)
write_csv(df_final, ARQ_SAIDA)
cat("\nArquivo salvo com sucesso em:", ARQ_SAIDA, "\n")
