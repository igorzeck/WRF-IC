# Programa para plotar o output do WRF utilizando a biblioteca wrf-pyton
# Imports ----
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import matplotlib.ticker as mticker
import numpy as np
import subprocess

from netCDF4 import Dataset
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from mpl_toolkits.axes_grid1 import make_axes_locatable
from wrf import (getvar, interplevel, to_np, latlon_coords, ALL_TIMES)

from pathlib import Path

# Geração de imagens ----
print("Gerando imagens...")
## GFS ----
path_arq = Path('datasets/met_em/global_comp/met_em_mergido.nc')
ncfile = Dataset(path_arq)

t2 = getvar(ncfile, "TT", timeidx=ALL_TIMES)
lats, longs = latlon_coords(t2)

# Geração de imagens do GFS
print("Gerando imagem do modelo gfs...")
for _f, t2i in enumerate(t2):
    print(f"Frame: {_f + 1}/{len(t2)}", end="\r")
    # Colormap e projeção
    cmap = plt.get_cmap('Reds')
    crs = ccrs.PlateCarree()

    fig = plt.figure(figsize=(20,12))    
    ax = fig.add_subplot(111, facecolor='None', projection=crs)
    ax.coastlines(resolution='10m', alpha=0.2)
    plot_t2 = ax.pcolormesh(longs, lats, t2i[0], cmap=cmap)
    plt.savefig(f".tmp_f_gfs_{_f}.jpg",bbox_inches='tight', pad_inches=0, format='jpg')
    plt.close()

## WRF ----
path_arq = Path('datasets/wrfout/wrf_global/wrfout_d01_2025-07-27.nc')
ncfile = Dataset(path_arq)

t2 = getvar(ncfile, "T2", timeidx=ALL_TIMES)
# Por segurança lats e longs de novo (mesmo que teoricamente sejam os mesmos)
lats, longs = latlon_coords(t2)

# Geração de imagens do WRF
print("\nGerando imagem do modelo wrf...")
for _f, t2i in enumerate(t2):
    print(f"Frame: {_f + 1}/{len(t2)}", end='\r')
    # Colormap e projeção
    cmap = plt.get_cmap('Reds')
    crs = ccrs.PlateCarree()

    fig = plt.figure(figsize=(20,12))    
    ax = fig.add_subplot(111, facecolor='None', projection=crs)
    ax.coastlines(resolution='10m', alpha=0.2)
    plot_t2 = ax.pcolormesh(longs, lats, t2i, cmap=cmap)
    plt.savefig(f".tmp_f_wrf_{_f}.jpg",bbox_inches='tight', pad_inches=0, format='jpg')
    plt.close()

# Geração de vídeo com o FFMPEG
print("\nGeração de vídeo...")
for f_rate, modelo in ((4, 'gfs'), (12,'wrf')):
    commando = f"ffmpeg -y -framerate {f_rate} -i .tmp_f_{modelo}_%d.jpg -c:v libx264 -pix_fmt yuv420p resources/{modelo}_global.mp4"
    subprocess.run(commando.split(' '))
    imagens = [f'.tmp_f_{modelo}_{n}.jpg' for n in range(len(t2))]
    subprocess.run(['rm'] + imagens)
    print(f"Vídeo do modelo {modelo} gerado!")

print("Fim do script.")