# ==============================================================================
# SCRIPT PARA EXTRAÇÃO DE DADOS DO GFS (GRIB2) PARA CSV
# Extrai variáveis de um diretório de arquivos GRIB2 para um CSV com uma linha por timestep.
# ==============================================================================
import os
import glob
import subprocess
import math
from datetime import datetime, timedelta
from pathlib import Path
import pandas as pd

# Mapeamento: nome descritivo -> padrão wgrib2 -match
VARIAVEIS_GFS = {
    # Temperatura e umidade
    "tmp_2m":       "TMP:2 m above ground",
    "tmp_sfc":      "TMP:surface",
    "dpt_2m":       "DPT:2 m above ground",
    "rh_2m":        "RH:2 m above ground",
    "spfh_2m":      "SPFH:2 m above ground",
    # Vento
    "ugrd_10m":     "UGRD:10 m above ground",
    "vgrd_10m":     "VGRD:10 m above ground",
    # Pressão
    "prmsl":        "PRMSL:mean sea level",
    "mslet":        "MSLET:mean sea level",
    "pres_sfc":     "PRES:surface",
    # Visibilidade
    "vis_sfc":      "VIS:surface",
    # Cobertura de nuvens
    "tcdc":         "TCDC:entire atmosphere",
    "lcdc":         "LCDC:low cloud layer",
    "mcdc":         "MCDC:middle cloud layer",
    "hcdc":         "HCDC:high cloud layer",
    # Geopotential heights
    "hgt_sfc":      "HGT:surface",
    "hgt_0deg":     "HGT:0C isotherm",
    "hgt_ceil":     "HGT:cloud ceiling",
    "hgt_htfl":     "HGT:highest tropospheric freezing level",
    # Índices de instabilidade
    "lftx":         "LFTX:surface",
    # Água precipitável e hidrometeoros
    "pwat":         "PWAT:entire atmosphere",
    "cwat":         "CWAT:entire atmosphere",
    # Precipitação
    "cpofp":        "CPOFP:surface",
    # Rugosidade
    "sfcr":         "SFCR:surface",
}

def extrair_valor_wgrib2(arquivo_grib: str, variavel: str, lon: float, lat: float):
    """Extrai o valor interpolado de uma variável no ponto (lon, lat) via wgrib2."""
    comando = ["wgrib2", arquivo_grib, "-match", variavel, "-lon", str(lon), str(lat)]
    resultado = subprocess.run(comando, capture_output=True, text=True)
    if resultado.returncode != 0 or not resultado.stdout:
        return None
    
    linha = resultado.stdout.strip().split("\n")[0]
    try:
        return float(linha.split("val=")[-1])
    except (IndexError, ValueError):
        return None

def obter_datetime_do_arquivo(nome_arquivo: str) -> datetime:
    """Extrai o datetime de previsão a partir do nome do arquivo GFS (ex: gfs.0p25.2026060100.f003.grib2)."""
    partes = nome_arquivo.split(".")
    data_str = partes[2]  # "2026060100"
    fh_str = partes[3]    # "f003"
    
    dt_base = datetime.strptime(data_str, "%Y%m%d%H")
    horas_prev = int(fh_str.replace("f", ""))
    return dt_base + timedelta(hours=horas_prev)

def extrair_variaveis_grib(arquivo_grib: str, lat: float, lon: float) -> dict:
    """Extrai todas as variáveis definidas em VARIAVEIS_GFS de um único arquivo GRIB2."""
    registro = {}
    for nome, padrao in VARIAVEIS_GFS.items():
        registro[nome] = extrair_valor_wgrib2(arquivo_grib, padrao, lon, lat)
    return registro

def calcular_derivadas(registro: dict) -> dict:
    """Calcula variáveis derivadas (conversões de unidades e vento) a partir dos valores brutos."""
    # Temperatura: Kelvin -> Celsius
    tmp_k = registro.get("tmp_2m")
    dpt_k = registro.get("dpt_2m")
    registro["temp_ar"] = (tmp_k - 273.15) if tmp_k is not None else None
    registro["temp_orvalho"] = (dpt_k - 273.15) if dpt_k is not None else None

    # Temperatura de superfície
    tmp_sfc = registro.get("tmp_sfc")
    registro["temp_sfc"] = (tmp_sfc - 273.15) if tmp_sfc is not None else None

    # Umidade relativa: 0-100 -> 0-1
    rh = registro.get("rh_2m")
    registro["umidade_relativa"] = (rh / 100.0) if rh is not None else None

    # Pressão ao nível do mar: Pa -> hPa
    prmsl = registro.get("prmsl")
    registro["pressao"] = (prmsl / 100.0) if prmsl is not None else None

    mslet = registro.get("mslet")
    registro["pressao_eta"] = (mslet / 100.0) if mslet is not None else None

    pres_sfc = registro.get("pres_sfc")
    registro["pressao_sfc"] = (pres_sfc / 100.0) if pres_sfc is not None else None

    # Vento: componentes -> velocidade (m/s) e direção meteorológica (graus)
    u = registro.get("ugrd_10m")
    v = registro.get("vgrd_10m")
    if u is not None and v is not None:
        registro["vel_vento"] = math.sqrt(u**2 + v**2)
        registro["dir_vento"] = (270 - math.degrees(math.atan2(v, u))) % 360
    else:
        registro["vel_vento"] = None
        registro["dir_vento"] = None

    return registro

def processar_diretorio_gfs(dir_grib: str, lat: float, lon: float, arq_saida: str = None) -> pd.DataFrame:
    """
    Processa todos os arquivos GRIB2 em um diretório, extraindo variáveis para (lat, lon).
    Retorna um DataFrame e opcionalmente salva em CSV.
    """
    padrao = os.path.join(dir_grib, "gfs.*.grib2")
    arquivos = sorted(glob.glob(padrao))
    
    if not arquivos:
        print(f"Nenhum arquivo GFS encontrado em: {dir_grib}")
        return pd.DataFrame()

    print(f"Processando {len(arquivos)} arquivos GRIB2 em: {dir_grib}")
    dados = []

    for arquivo in arquivos:
        nome = os.path.basename(arquivo)
        dt_previsao = obter_datetime_do_arquivo(nome)
        print(f"  Extraindo: {nome} -> {dt_previsao}")

        registro = extrair_variaveis_grib(arquivo, lat, lon)
        registro = calcular_derivadas(registro)
        registro["datetime"] = dt_previsao
        dados.append(registro)

    df = pd.DataFrame(dados)

    # Reordenar colunas: datetime primeiro
    cols = ["datetime"] + [c for c in df.columns if c != "datetime"]
    df = df[cols]

    if arq_saida:
        os.makedirs(os.path.dirname(os.path.abspath(arq_saida)), exist_ok=True)
        df.to_csv(arq_saida, index=False)
        print(f"CSV salvo em: {arq_saida} ({len(df)} registros)")

    return df


def main():
    import argparse
    parser = argparse.ArgumentParser(description="Extrai variáveis GFS (GRIB2) para CSV em um ponto específico.")
    parser.add_argument("dir_grib", help="Diretório contendo os arquivos GRIB2.")
    parser.add_argument("--lat", type=float, default=-22.804944, help="Latitude do ponto alvo.")
    parser.add_argument("--lon", type=float, default=-43.256455, help="Longitude do ponto alvo.")
    parser.add_argument("--saida", default=None, help="Caminho do CSV de saída (opcional).")
    args = parser.parse_args()

    df = processar_diretorio_gfs(args.dir_grib, args.lat, args.lon, args.saida)
    if df.empty:
        print("Nenhum dado extraído.")
    else:
        print(f"\nExtraídos {len(df)} registros com {len(df.columns)} colunas.")
        print(df.head(3).to_string())


if __name__ == "__main__":
    main()
