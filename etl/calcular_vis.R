# Script para calcular visibilidade por meio dos dados do WRF
# Versão em R
# Setup ----
#TODO: Mover isso aqui para wrfout_source como uma série de funções
source("etl/wrfout_source.R")

arq_path <- "/home/rf/WD/WRF/test/em_real/wrfout_d01_2026-04-04.nc"

nc_arq <- nc_open(arq_path)
seq_h <- get_seq_h(nc_arq)
seq_h

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
phb <- get_wrf_var_all(nc_arq, 'PHB')
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

# 0.5 porque é de 0 a 1e4, efetivamente o mesmo que fazer um min-max aqui
vis_wrf * 0.5e3
nc_close(nc_arq)