# Script para gerar mapa dos dominios WRF
# Setup ----
library(tidyverse)
library(ncdf4)
library(ggplot2)
library(maps)
library(mapdata)

message("Criando mapa de dominios WRF...")

# Funcao para extrair os contornos do dominio
get_domain_edges <- function(nc_path, domain_name) {
  nc <- ncdf4::nc_open(nc_path)
  lats <- ncdf4::ncvar_get(nc, "XLAT_M")
  lons <- ncdf4::ncvar_get(nc, "XLONG_M")
  if (length(dim(lats)) == 3) lats <- lats[,,1]
  if (length(dim(lons)) == 3) lons <- lons[,,1]
  ncdf4::nc_close(nc)
  
  nlon <- dim(lons)[1]
  nlat <- dim(lons)[2]
  
  # Pegar bordas para fazer um poligono fechado (Sul, Leste, Norte, Oeste)
  edge_lon <- c(lons[, 1], lons[nlon, ], rev(lons[, nlat]), rev(lons[1, ]))
  edge_lat <- c(lats[, 1], lats[nlon, ], rev(lats[, nlat]), rev(lats[1, ]))
  
  data.frame(
    lon = edge_lon,
    lat = edge_lat,
    domain = domain_name
  )
}

# Caminhos dos arquivos geo_em
wps_dir <- "/home/rf/WD/WPS"
files <- c(
  "d01" = file.path(wps_dir, "geo_em.d01.nc"),
  "d02" = file.path(wps_dir, "geo_em.d02.nc"),
  "d03" = file.path(wps_dir, "geo_em.d03.nc"),
  "d04" = file.path(wps_dir, "geo_em.d04.nc")
)

# Extrair contornos
domains_df <- data.frame()
for (d in names(files)) {
  if (file.exists(files[[d]])) {
    domains_df <- bind_rows(domains_df, get_domain_edges(files[[d]], d))
  } else {
    message("Aviso: ", files[[d]], " nao encontrado.")
  }
}

# Configurar limites do mapa baseados no d01 (com uma pequena margem)
d01 <- domains_df %>% filter(domain == "d01")
min_lon <- min(d01$lon) - 2
max_lon <- max(d01$lon) + 2
min_lat <- min(d01$lat) - 2
max_lat <- max(d01$lat) + 2

# Criar dataframe para as legendas (canto superior esquerdo de cada dominio)
labels_df <- domains_df %>%
  group_by(domain) %>%
  summarize(
    lon = min(lon) + 0.2, # Deslocamento leve para a direita
    lat = max(lat) - 0.2  # Deslocamento leve para baixo
  )

# Obter dados do mapa
world_map <- map_data("worldHires")

# Plotagem ----

# Criar o plot
p <- ggplot() +
  # Adicionar o mapa base
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "#e0e0e0", color = "#999999", linewidth = 0.2) +
  # Adicionar os dominios
  geom_path(data = domains_df, aes(x = lon, y = lat, color = domain, group = domain), linewidth = 1.2) +
  # Adicionar os textos flutuantes com fundo
  geom_label(data = labels_df, aes(x = lon, y = lat, label = toupper(domain), color = domain), 
            fontface = "bold", size = 5, fill = "white", label.size = NA, alpha = 0.8, show.legend = FALSE) +
  # Definir as cores dos dominios
  scale_color_manual(values = c("d01" = "#1f77b4", "d02" = "#ff7f0e", "d03" = "#2ca02c", "d04" = "#d62728")) +
  # Restringir o mapa aos limites do d01
  coord_cartesian(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat)) +
  # Tema bonito
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#f0f8ff", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "lightgray")
  ) +
  labs(
    subtitle = "Configuração centralizada em: Aeroporto Internacional do Galeão (SBGL)",
    x = "Longitude",
    y = "Latitude",
    color = "Domínios"
  )

# Salvamento ----
# Salva o plot
out_dir <- "resources"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

out_file <- file.path(out_dir, "dominios_wrf.png")
ggsave(out_file, plot = p, width = 10, height = 8, dpi = 300)
message("Mapa salvo em: ", out_file)
