#!/bin/bash
# Script para a junção de dados "above aground" dos Gribs2 do GFS para Netcdf

# Limpa log
echo "" > .merge.log

echo "Grib inicial..."
# Cria arquivo (para append)
# Not o "OR" lógico
wgrib2 gfs.0p25.2026012400.f000.grib2 -match "(above ground|:surface:)" -not "MAX" -not "MIN" -nc4 -netcdf gfs_2026-01-24_aground.nc &>> .merge.log
# Aplica para todos arquivos no diretório
for arq in *[1-9].grib2; do
    echo "Rodando para: $arq"
    wgrib2 "$arq" -match "(above ground|:surface:)" -not "MAX" -not "MIN" -append -nc4 -netcdf gfs_2026-01-24_aground.nc &>> .merge.log
done
