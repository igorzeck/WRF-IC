#!/usr/bin/env python3
# Script python para a predição de tempo de execução de uma simulação
# com base nos dados de uma simulação Baseline.
# NOTE: Assume crescimento linear em relação a ela
import argparse
import os
import glob
from datetime import datetime
import math

# NOTE: Valores baseados em mpirun -n 6
# CONSTANTES
GEOGRID_SEC = 6.06
UNGRIB_SEC_PER_FILE = 16.37
METGRID_SEC_PER_FILE = 33.98
REAL_SEC_PER_HOUR = 0.45
WRF_SEC_PER_HOUR = 414.65

GRIB2_SIZE_MB = 543.0
OUT_D01_MB_PER_HR = 7.2
OUT_D02_MB_PER_HR = 12.0
OUT_D03_MB_PER_HR = 11.5
OUT_D04_MB_PER_HR = 11.3

# CONSTANTES INTERMEDIÁRIAS (WPS)
FILE_MB_PER_IN = 781.0
GEO_EM_TOTAL_MB = 3.0
MET_EM_MB_PER_IN = 11.2

# CONSTANTS DE RESTART
RST_D01_MB = 66.0
RST_D02_MB = 110.0
RST_D03_MB = 105.0
RST_D04_MB = 103.0

# Print de horas em format Hh Mm SSs
def format_time(seconds):
    h = int(seconds // 3600)
    m = int((seconds % 3600) // 60)
    s = seconds % 60
    return f"{h}h {m}m {s:.2f}s"

# Tamanho em MB ou GB
def format_size(mb):
    if mb > 1024:
        return f"{mb/1024:.2f} GB"
    return f"{mb:.2f} MB"

# Print do report automatizado
def print_report(sim_hours, num_files, restart_interval=None):
    t_geogrid = GEOGRID_SEC
    t_ungrib = num_files * UNGRIB_SEC_PER_FILE
    t_metgrid = num_files * METGRID_SEC_PER_FILE
    t_real = sim_hours * REAL_SEC_PER_HOUR
    t_wrf = sim_hours * WRF_SEC_PER_HOUR
    t_total = t_geogrid + t_ungrib + t_metgrid + t_real + t_wrf

    # Extrapolação linear - Constantes
    s_in = num_files * GRIB2_SIZE_MB
    s_d01 = sim_hours * OUT_D01_MB_PER_HR
    s_d02 = sim_hours * OUT_D02_MB_PER_HR
    s_d03 = sim_hours * OUT_D03_MB_PER_HR
    s_d04 = sim_hours * OUT_D04_MB_PER_HR
    s_out_total = s_d01 + s_d02 + s_d03 + s_d04

    # Extrapolação linear - Constantes - Intermediários
    s_wps_file = num_files * FILE_MB_PER_IN
    s_wps_geo = GEO_EM_TOTAL_MB
    s_wps_met = num_files * MET_EM_MB_PER_IN
    s_inter_total = s_wps_file + s_wps_geo + s_wps_met

    # Extrapolação linear - Restarts
    s_rst_total = 0.0
    num_restarts = 0
    if restart_interval and restart_interval > 0:
        num_restarts = int(sim_hours // restart_interval)
        s_rst_total = num_restarts * (RST_D01_MB + RST_D02_MB + RST_D03_MB + RST_D04_MB)

    # Total
    s_grand_total = s_in + s_out_total + s_inter_total + s_rst_total

    print("="*60)
    print(" REPORT DE TEMPO DE EXECUÇÃO (v3)")
    print("="*60)
    print(f"Tempo de simulação : {sim_hours} horas")
    print(f"Número de arquivos de input : {num_files} (3h a 3h)")
    if restart_interval:
        print(f"Intervalor de restart : A cada {restart_interval} horas -> ({num_restarts} snapshots)")
    print("-" * 60)
    print("TEMPO DE EXECUÇÃO ESTIMADA:")
    print(f"  geogrid.exe : {format_time(t_geogrid)}")
    print(f"  ungrib.exe  : {format_time(t_ungrib)}")
    print(f"  metgrid.exe : {format_time(t_metgrid)}")
    print(f"  real.exe    : {format_time(t_real)}")
    print(f"  wrf.exe     : {format_time(t_wrf)}")
    print(f"  TOTAL       : {format_time(t_total)}")
    print("-" * 60)
    print("ESPAÇO DE ARMAZEN. ESTIMADO:")
    print(f"  [1] Dados de Input (GFS) : {format_size(s_in)}")
    print(f"  [2] Arquivos WPS Intermediários : {format_size(s_inter_total)}")
    print(f"        -> ungrib  (FILE:*): {format_size(s_wps_file)}")
    print(f"        -> geogrid (geo_em): {format_size(s_wps_geo)}")
    print(f"        -> metgrid (met_em): {format_size(s_wps_met)}")
    print(f"  [3] Output por domínio (WRFOUT): {format_size(s_out_total)}")
    print(f"        -> Domínio 1 : {format_size(s_d01)}")
    print(f"        -> Domínio 2 : {format_size(s_d02)}")
    print(f"        -> Domínio 3 : {format_size(s_d03)}")
    print(f"        -> Domínio 4 : {format_size(s_d04)}")
    
    if restart_interval:
        print(f"  [4] Arquivos de Restart (WRFRST) : {format_size(s_rst_total)}")
    
    print("-" * 60)
    print(f"FOOTPRINT TOTAL DA PIPELINE: {format_size(s_grand_total)}")
    print("="*60)

def main():
    parser = argparse.ArgumentParser(description="Predição linear do tempo de execução do WRF e uso de disco.")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--input-dir", type=str, help="Caminho dos arquivos GRIB2 do GFS (input).")
    group.add_argument("--datas", type=str, nargs=2, metavar=('INICIO', 'FIM'), help="Data de início e fim e formato ISO (e.g. 2026-06-27T00:00:00).")
    
    parser.add_argument("--intervalo-restarts", type=int, help="Intervalo (em horas) para geração de wrfrst (restarts).")

    args = parser.parse_args()

    if args.input_dir:
        if not os.path.isdir(args.input_dir):
            print(f"Erro: {args.input_dir} não é um diretório válido.")
            return
        
        files = [f for f in os.listdir(args.input_dir) if os.path.isfile(os.path.join(args.input_dir, f))]
        num_files = len(files)
        
        if num_files == 0:
            print("Erro: Não há arquvios no diretório especificado.")
            return
        
        sim_hours = (num_files - 1) * 3
        if sim_hours < 0:
            sim_hours = 0
            
    elif args.datas:
        try:
            start_dt = datetime.fromisoformat(args.datas[0])
            end_dt = datetime.fromisoformat(args.datas[1])
        except ValueError:
            print("Erro: Formato de data inválido -> Use em formato ISO, e.g. 2026-06-27T00:00:00")
            return
            
        diff_hours = (end_dt - start_dt).total_seconds() / 3600.0
        if diff_hours < 0:
            print("Erro: Data final deveria ser APÓS data inicial.")
            return
            
        sim_hours = math.ceil(diff_hours)
        num_files = (sim_hours // 3) + 1

    print_report(sim_hours, num_files, args.intervalo_restarts)

if __name__ == "__main__":
    main()
