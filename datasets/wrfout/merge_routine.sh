#!/bin/bash
# Junta arquivos wrfout em 4 domínios
# Para evitar appends de outras execuções:
echo "" > merge_routine_out.txt

for i in {1..4}; do
	echo "Domínio $i..."
	cdo mergetime wrfout_d0"$i"* wrfout_d0"$i"_2026-01-24.nc &>> merge_routine_out.txt
done
