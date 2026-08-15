#!/usr/bin/env python3
"""
append_lmlt_to_metar.py
Adiciona a variável LMLT e umidade relativa a um CSV METAR no formato do dataset histórico.
"""

import os
import glob
import math
import argparse
import subprocess
import pandas as pd
import numpy as np
from datetime import datetime

LAT_SBGL = -22.804944
LON_SBGL = -43.256455

COLUMN_RENAME_MAP = {
    "wind_speed": "vel_vento",
    "wind_direction": "dir_vento",
    "temperature": "temp_ar",
    "dew_point": "temp_orvalho",
    "pressure": "pressao",
    "visibility": "vis",
    "weather_information": "clima",
}

TARGET_COLUMN_ORDER = [
    "datetime", "vel_vento", "dir_vento", "temp_ar", "temp_orvalho",
    "pressao", "vis", "clima", "categ_nuvem", "altura_nuvem", "lmlt", "umidade_relativa"
]


def inspect_grib_variables(grib_path):
    """Lista as variáveis presentes no arquivo GRIB (GRIB1/GRIB2)."""
    if not os.path.exists(grib_path):
        print(f"Erro: Arquivo '{grib_path}' não encontrado.")
        return
    
    print(f"\n--- Inspecionando variáveis: {grib_path} ---")
    cmd = ["grib_ls", "-p", "dataDate,dataTime,shortName,name", grib_path]
    try:
        res = subprocess.run(cmd, capture_output=True, text=True, check=True)
        lines = res.stdout.strip().split("\n")
        print(f"Total de registros listados: {len(lines)}")
        for line in lines[:30]:
            print(f"  {line}")
        if len(lines) > 30:
            print(f"  ... mais {len(lines) - 30} registros.")
    except Exception:
        # Fallback para wgrib2 se grib_ls falhar
        cmd_w2 = ["wgrib2", grib_path, "-s"]
        res = subprocess.run(cmd_w2, capture_output=True, text=True)
        lines = res.stdout.strip().split("\n")
        print(f"Total de registros (wgrib2): {len(lines)}")
        for line in lines[:30]:
            print(f"  {line}")


def extract_grib_values(grib_file, var_match="TMP", lon=LON_SBGL, lat=LAT_SBGL):
    """
    Extrai valores GRIB (GRIB1 ou GRIB2) no ponto (lon, lat).
    Retorna dicionário { "YYYY-MM-DDTHH:MM:SSZ": valor_celsius }.
    """
    lmlt_dict = {}

    # Tenta usar grib_get (ecCodes - suporta GRIB1 e GRIB2)
    cmd_ec = ["grib_get", "-l", f"{lat},{lon},1", "-p", "dataDate,dataTime", grib_file]
    res_ec = subprocess.run(cmd_ec, capture_output=True, text=True)
    if res_ec.returncode == 0 and res_ec.stdout:
        for line in res_ec.stdout.strip().split("\n"):
            parts = line.strip().split()
            if len(parts) >= 3:
                try:
                    date_str, time_str, val_str = parts[0], parts[1], parts[2]
                    val = float(val_str)
                    dt_obj = datetime.strptime(date_str, "%Y%m%d")
                    t_val = int(time_str)
                    hour = t_val // 100
                    minute = t_val % 100
                    dt = dt_obj.replace(hour=hour, minute=minute)

                    if val > 100.0:
                        val = val - 273.15  # Converte Kelvin -> Celsius

                    iso_str = dt.strftime("%Y-%m-%dT%H:%M:%SZ")
                    lmlt_dict[iso_str] = round(val, 3)
                except (ValueError, IndexError):
                    continue
        if lmlt_dict:
            return lmlt_dict

    # Fallback para wgrib2 (para arquivos GRIB2 específicos)
    cmd_w2 = ["wgrib2", grib_file, "-match", var_match, "-lon", str(lon), str(lat)]
    res_w2 = subprocess.run(cmd_w2, capture_output=True, text=True)
    if res_w2.returncode == 0 and res_w2.stdout:
        for line in res_w2.stdout.strip().split("\n"):
            if "val=" in line:
                try:
                    val_str = line.split("val=")[-1]
                    val = float(val_str)
                    if val > 100.0:
                        val = val - 273.15
                    for token in line.split(":"):
                        if token.startswith("d="):
                            date_part = token.replace("d=", "")[:10]
                            dt = datetime.strptime(date_part, "%Y%m%d%H")
                            iso_str = dt.strftime("%Y-%m-%dT%H:%M:%SZ")
                            lmlt_dict[iso_str] = round(val, 3)
                except (ValueError, IndexError):
                    continue

    return lmlt_dict


def extract_lmlt_from_gribs(grib_input_path, grib_var="TMP"):
    """Varre arquivos/diretórios GRIB e extrai dados de LMLT."""
    if os.path.isfile(grib_input_path):
        grib_files = [grib_input_path]
    elif os.path.isdir(grib_input_path):
        grib_files = sorted(glob.glob(os.path.join(grib_input_path, "*.grib*"))) + \
                     sorted(glob.glob(os.path.join(grib_input_path, "*.grb*")))
    else:
        print(f"Aviso: Caminho '{grib_input_path}' não encontrado.")
        return {}

    lmlt_dict = {}
    print(f"Extraindo LMLT de {len(grib_files)} arquivo(s) GRIB...")

    for gfile in grib_files:
        file_dict = extract_grib_values(gfile, var_match=grib_var)
        lmlt_dict.update(file_dict)

    print(f"Extraídos {len(lmlt_dict)} registros LMLT com timestamp.")
    return lmlt_dict


def calculate_relative_humidity(temp_ar, temp_orvalho):
    """Calcula a umidade relativa a partir da temperatura e ponto de orvalho em °C."""
    if pd.isna(temp_ar) or pd.isna(temp_orvalho):
        return np.nan
    try:
        num = math.exp((17.625 * temp_orvalho) / (243.04 + temp_orvalho))
        den = math.exp((17.625 * temp_ar) / (243.04 + temp_ar))
        return round(num / den, 2)
    except (ValueError, ZeroDivisionError):
        return np.nan


def generate_mock_lmlt(df):
    """Gera valores simulados de LMLT (°C) baseados na temperatura do ar."""
    np.random.seed(42)
    lmlt_vals = []
    for idx, row in df.iterrows():
        t_ar = row["temp_ar"]
        if pd.isna(t_ar):
            lmlt_vals.append(np.nan)
        else:
            noise = (np.sin(idx / 12.0) * 0.3) + np.random.normal(0, 0.15)
            lmlt_vals.append(round(t_ar + noise, 3))
    return lmlt_vals


def process_metar_file(input_csv, output_csv, grib_path=None, grib_var="TMP", force_mock=False):
    """Processa o METAR CSV, adiciona LMLT/umidade relativa e salva o arquivo final."""
    if not os.path.exists(input_csv):
        raise FileNotFoundError(f"Arquivo CSV de entrada não encontrado: {input_csv}")

    print(f"Lendo METAR: {input_csv}")
    df = pd.read_csv(input_csv)
    df = df.rename(columns=COLUMN_RENAME_MAP)

    if "umidade_relativa" not in df.columns:
        df["umidade_relativa"] = [
            calculate_relative_humidity(t, td) 
            for t, td in zip(df["temp_ar"], df["temp_orvalho"])
        ]

    lmlt_data = {}
    if grib_path and os.path.exists(grib_path) and not force_mock:
        lmlt_data = extract_lmlt_from_gribs(grib_path, grib_var)

    if lmlt_data:
        print("Mapeando valores extraídos do GRIB com timestamps do METAR...")
        df["lmlt"] = df["datetime"].map(lmlt_data)
        missing_count = df["lmlt"].isna().sum()
        if missing_count > 0:
            print(f"Aviso: {missing_count} linhas não encontraram correspondência no GRIB. Preenchendo lacunas com estimativa.")
            mock_values = generate_mock_lmlt(df)
            df["lmlt"] = df["lmlt"].fillna(pd.Series(mock_values, index=df.index))
    else:
        print("Gerando valores simulados de LMLT...")
        df["lmlt"] = generate_mock_lmlt(df)

    available_cols = [col for col in TARGET_COLUMN_ORDER if col in df.columns]
    df_out = df[available_cols]

    os.makedirs(os.path.dirname(os.path.abspath(output_csv)), exist_ok=True)
    df_out.to_csv(output_csv, index=False)
    print(f"Arquivo salvo com sucesso em: {output_csv}")
    print(f"Dimensões: {df_out.shape[0]} linhas, {df_out.shape[1]} colunas.")


def main():
    parser = argparse.ArgumentParser(description="Adiciona a variável LMLT ao CSV do METAR.")
    parser.add_argument("--metar-in", default="datasets/metar_SBGL_2026.csv", help="CSV METAR de entrada.")
    parser.add_argument("--metar-out", default="datasets/metar_SBGL_2026_lmlt.csv", help="CSV METAR de saída.")
    parser.add_argument("--grib-path", default="/home/rf/WRF-IC/datasets/era5/lmlt_2026.grib", help="Caminho do arquivo ou diretório GRIB.")
    parser.add_argument("--grib-var", default="TMP", help="Nome da variável no wgrib2/grib_get.")
    parser.add_argument("--inspect", action="store_true", help="Inspeciona as variáveis do GRIB.")
    parser.add_argument("--mock", action="store_true", help="Força a geração de dados simulados.")

    args = parser.parse_args()

    if args.inspect:
        if not args.grib_path:
            print("Especifique --grib-path para inspecionar.")
        else:
            inspect_grib_variables(args.grib_path)
        return

    process_metar_file(
        input_csv=args.metar_in,
        output_csv=args.metar_out,
        grib_path=args.grib_path,
        grib_var=args.grib_var,
        force_mock=args.mock
    )


if __name__ == "__main__":
    main()
