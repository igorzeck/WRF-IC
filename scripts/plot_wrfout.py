# Programa para plotar o output do WRF utilizando a biblioteca wrf-pyton
# Imports ----
from netCDF4 import Dataset
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import matplotlib.ticker as mticker
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from mpl_toolkits.axes_grid1 import make_axes_locatable
from wrf import (getvar, interplevel, to_np, latlon_coords)