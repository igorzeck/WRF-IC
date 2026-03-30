#!/bin/bash
# Script para juntar arquivos grib em um único netcdf
wgrib2 *f000.grib2 -netcdf gfs_2026-01-24.nc

for arq in *.grib*; do
	echo "$arq"
	wgrib2 $file -append -netcdf gfs_2026-01-24.nc
done
