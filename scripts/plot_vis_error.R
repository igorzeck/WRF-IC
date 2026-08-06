# Plotagem de AE (Absolute Error) de Visbilidade (pela fórmula de Koschmieder para WRF)
# METAR: 2026 - SBGL
# GFS: gfs_emulated_metar_raw2.csv
# WRF: netCDF lidos diretamente do WD/WRF/test/em_real/ - última simulação
# NOTE: Usar ivisibilidade nativa do GFS

library(tidyverse)
library(lubridate)
library(ncdf4)

set.seed(42)

# 1. Carregamento ---- 
## 1.1. METAR
metar_raw <- read_csv("datasets/metar_SBGL_2026.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC"))

metar_obs <- metar_raw %>%
  filter(datetime >= as.POSIXct("2026-06-22 00:00:00", tz = "UTC") &
         datetime <= as.POSIXct("2026-06-28 00:00:00", tz = "UTC")) %>%
  select(datetime, obs_vis = visibility)

## 1.2. GFS (vis nativa)
gfs_df <- read_csv("datasets/gfs_emulated_metar_raw2.csv", show_col_types = FALSE) %>%
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
  select(datetime, gfs_vis = vis)

# 2. Calcular Koschmieder vis para o WRF
files <- list.files("/home/rf/WD/WRF/test/em_real", pattern = "^wrfout_d04_2026-06-2[2-8]", full.names = TRUE)
files <- sort(files)

records <- list()
for (f in files) {
  nc <- ncdf4::nc_open(f)
  
  lats <- ncdf4::ncvar_get(nc, "XLAT")
  lons <- ncdf4::ncvar_get(nc, "XLONG")
  if (length(dim(lats)) == 3) lats <- lats[,,1]
  if (length(dim(lons)) == 3) lons <- lons[,,1]
  
  target_lat <- -22.804944
  target_lon <- -43.256455
  dist_deg <- sqrt((lats - target_lat)^2 + (lons - target_lon)^2)
  min_idx <- which(dist_deg == min(dist_deg), arr.ind = TRUE)
  i <- min_idx[1, 1]
  j <- min_idx[1, 2]
  
  t2   <- ncdf4::ncvar_get(nc, "T2")[i, j] - 273.15
  psfc <- ncdf4::ncvar_get(nc, "PSFC")[i, j] / 100
  q2   <- ncdf4::ncvar_get(nc, "Q2")[i, j]
  e_hpa <- (psfc * q2) / (0.622 + 0.378 * q2)
  td   <- (243.5 * log(e_hpa / 6.112)) / (17.67 - log(e_hpa / 6.112))
  
  es <- 6.112 * exp((17.67 * t2) / (t2 + 243.5))
  e  <- 6.112 * exp((17.67 * td) / (td + 243.5))
  rh <- pmin(pmax(e / es, 0), 1)
  
  qcloud <- tryCatch(ncdf4::ncvar_get(nc, "QCLOUD")[i, j, 1], error = function(e) 0)
  rho <- (psfc * 100) / (287.058 * (t2 + 273.15))
  lwc <- qcloud * rho * 1000 # g/m3
  
  beta_clean <- 0.03912 / 1000
  beta_rh    <- 0.03912 / 1000 * pmax(1, (1 / (1 - min(rh, 0.99)))^1.2)
  beta_cloud <- ifelse(lwc > 0, 144.7 / 1000 * (lwc)^0.88, 0)
  
  beta_total <- beta_clean + beta_rh + beta_cloud
  wrf_koschmieder <- 3.912 / beta_total
  
  valores_t <- ncdf4::ncvar_get(nc, "XTIME")
  unid_t <- ncdf4::ncatt_get(nc, "XTIME", "units")$value
  t_ustr <- strsplit(unid_t, " ")
  t_origem <- paste(unlist(t_ustr)[3], unlist(t_ustr)[4])
  dt <- as.POSIXct(valores_t * 60, origin = t_origem, tz = "UTC")
  
  ncdf4::nc_close(nc)
  
  records[[f]] <- data.frame(datetime = dt, wrf_vis = wrf_koschmieder)
}

df_wrf <- bind_rows(records)

# 3. Merge dos dados e cálculo do Erro
df_eval <- metar_obs %>%
  inner_join(gfs_df, by = "datetime") %>%
  inner_join(df_wrf, by = "datetime") %>%
  mutate(
    hour = hour(datetime)
  ) %>%
  filter(hour %% 3 == 0) %>%
  mutate(
    GFS_AE = abs(gfs_vis - obs_vis),
    WRF_AE = abs(wrf_vis - obs_vis)
  )

# 4. Preparo para plotagem
plot_df <- df_eval %>%
  select(datetime, GFS_AE, WRF_AE) %>%
  pivot_longer(cols = c(GFS_AE, WRF_AE), names_to = "Model", values_to = "Absolute_Error")

# 5. Plot
p <- ggplot(plot_df, aes(x = datetime, y = Absolute_Error, color = Model)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_line(alpha = 0.3) +
  scale_color_manual(values = c("GFS_AE" = "#377eb8", "WRF_AE" = "#e41a1c"),
                     labels = c("GFS_AE" = "GFS Error", "WRF_AE" = "WRF Error")) +
  scale_y_continuous(labels = scales::comma_format(suffix = " m")) +
  labs(
    title = "Erro Absoluto na Visibilidade (22 a 28 de Junho)",
    subtitle = "GFS Base vs WRF Koschmieder (Intervalo de 3h)",
    x = "Data",
    y = "Erro Absoluto (m)",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

img_path <- "resources/vis_absolute_error_3h.png"
ggsave(img_path, plot = p, width = 10, height = 6, dpi = 300)

message("Gráfico de erro absoluto de visibilidade gerado com sucesso!")
