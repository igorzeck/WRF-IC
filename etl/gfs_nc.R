# Arquivo para importar os dados do gfs
# 0. Setup ----
library(tidyverse)
library(ncdf4)
library(lubridate)

# 0.1 Tempo ----
nc_arq <- nc_open("datasets/fnl_gfs.nc")

valores_t <- ncvar_get(nc_arq, "time")
which(valores_t == 1769947200)
unid_t <- ncatt_get(nc_arq, "time", "units")$value

t_ustr <- strsplit(unid_t, " ")
t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])

seq_h <- as.POSIXct(valores_t, origin = t_origem, tz = "UTC")

nc_close(nc_arq)

# 0.3 Latitude e Longitude ----
nc_arq - nc_open("datasets/fnl_gfs.nc")

lats <- ncvar_get(nc_arq, "latitude")
which(lats == -24)

# Esse é redudante!
longs <- nc_open("datasets/fnl_gfs.nc")

longs <- ncvar_get(nc_arq, "longitude")
which(longs == 314)
longs

seq_h <- as.POSIXct(valores_t, origin = t_origem, tz = "UTC")

nc_close(nc_arq)

# 0.3 Pega para coordenadas de Santos ----
# Coordendas Porto de Santos:
# -23.96592313201299, -46.302018806825046
# Aproximando:
# -24, -46
# Convertendo para o esquema do arquivo
# Assim como visto com ncdump (Instalado com o WRF)
# No arquivo a unidade de medida do latitude e longitude é:
# Degrees North e Degrees East
# latitude: 181 valores
# longitue: 360 valores
# -24:
idy <- which(lats == -24)
idy  # Latitude
# -46:
idx <- which(longs == 360 - 46)
idx  # Longitude

idt = 1  # time
# float TMP_1000mb(time, latitude, longitude)
# Inspecionando o grib com grib_ls -l -24,-46,1 -p value,shortName,level -w paramId=130,level=1000 fnl_20260201_12_00.grib2
# Verifica-se que para as coordendas acima a temperatura é 299.87
temp_alvo <- 299.87 - 273.15
temp_alvo
# 26.72

# 1. Variável temperatura a 1000mb - TMP_1000mb ----
nc_arq <- nc_open("datasets/fnl_gfs.nc")
# Por algum motivo abaixo não está operando
# nc_temp <- ncvar_get(nc_arq, "TMP_1000mb", start = c(idt, idx, idy), count = c(1, 1,1))
nc_temp <- ncvar_get(nc_arq, "TMP_1000mb")
# Por agora as coordenadas são extraídas diretamente da matriz
dim(nc_temp)
temp <- (nc_temp[idx,idy]) - 273.15
temp
# Acesso a temperatura correto agora!