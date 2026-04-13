#!/bin/bash
# Junta arquivos wrfout em até 4 domínios
# Para evitar appends de outras execuções:
echo "" > merge_routine_out.txt

data=$1
prefix=$2

if [[ -z "$prefix" ]]; then
  prefix="wrfout_"
fi


for i in {1..4}; do
	echo "Domínio $i..."
	cdo mergetime "$prefix"d0"$i"* "$prefix"d0"$i"_"$data".nc &>> merge_routine_out.txt
done
