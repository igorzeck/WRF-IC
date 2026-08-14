import os
import glob
import subprocess
import math
from datetime import datetime, timedelta
import pandas as pd

# ==============================================================================
# CONFIGURAÇÕES GERAIS
# ==============================================================================
DIR_GFS = "/home/rf/WD/DATA/GFS_FINAL/raw_2"
ARQ_SAIDA = "datasets/gfs_emulated_metar_raw2.csv"

# Coordenadas do aeroporto SBGL
LAT_ALVO = -22.804944
LON_ALVO = -43.256455

# ==============================================================================
# FUNÇÕES AUXILIARES
# ==============================================================================
def extrair_valor_wgrib2(arquivo_grib, variavel):
    """
    Executa o wgrib2 para extrair o valor interpolado de uma variável 
    em uma coordenada (latitude/longitude) específica.
    """
    comando = [
        "wgrib2", arquivo_grib,
        "-match", variavel,
        "-lon", str(LON_ALVO), str(LAT_ALVO)
    ]
    resultado = subprocess.run(comando, capture_output=True, text=True)
    if resultado.returncode != 0 or not resultado.stdout:
        return None
    
    # Exemplo de saída: "1:0:d=2026062200:TMP:2 m above ground:anl::lon=316.743545,lat=-22.804944,val=298.15"
    linha = resultado.stdout.strip().split("\n")[0]
    try:
        valor_str = linha.split("val=")[-1]
        return float(valor_str)
    except (IndexError, ValueError):
        return None

def obter_datetime_do_arquivo(nome_arquivo):
    """
    Extrai a data base e a hora de previsão (forecast hour) a partir do
    nome do arquivo (ex: gfs.0p25.2026062200.f003.grib2).
    """
    partes = nome_arquivo.split(".")
    data_str = partes[2]  # ex: "2026062200"
    fh_str = partes[3]    # ex: "f003"
    
    dt_base = datetime.strptime(data_str, "%Y%m%d%H")
    horas_prev = int(fh_str.replace("f", ""))
    
    return dt_base + timedelta(hours=horas_prev)

# ==============================================================================
# PROGRAMA PRINCIPAL
# ==============================================================================
def main():
    print(f"Iniciando extração do GFS no diretório: {DIR_GFS}")
    padrao_busca = os.path.join(DIR_GFS, "gfs.*.grib2")
    arquivos_gfs = sorted(glob.glob(padrao_busca))
    
    if not arquivos_gfs:
        print("Nenhum arquivo GFS encontrado!")
        return

    dados_extraidos = []

    # Iterar sobre todos os arquivos GRIB2 encontrados
    for arquivo in arquivos_gfs:
        nome_arquivo = os.path.basename(arquivo)
        dt_previsao = obter_datetime_do_arquivo(nome_arquivo)
        
        print(f"Processando: {nome_arquivo} -> {dt_previsao}")

        # Extração de Variáveis Diretas
        tmp_k = extrair_valor_wgrib2(arquivo, "TMP:2 m above ground")
        dpt_k = extrair_valor_wgrib2(arquivo, "DPT:2 m above ground")
        rh = extrair_valor_wgrib2(arquivo, "RH:2 m above ground")
        u_wind = extrair_valor_wgrib2(arquivo, "UGRD:10 m above ground")
        v_wind = extrair_valor_wgrib2(arquivo, "VGRD:10 m above ground")
        prmsl_pa = extrair_valor_wgrib2(arquivo, "PRMSL:mean sea level")
        vis = extrair_valor_wgrib2(arquivo, "VIS:surface")

        # Conversões Físicas e Cálculos Derivados
        temp_c = tmp_k - 273.15 if tmp_k is not None else None
        dpt_c = dpt_k - 273.15 if dpt_k is not None else None
        pressao_hpa = prmsl_pa / 100.0 if prmsl_pa is not None else None

        # RH passa a ser entre 0 e 1
        rh /= 100
                
        vel_vento = None
        dir_vento = None
        if u_wind is not None and v_wind is not None:
            # Velocidade do vento de m/s para nós (knots)
            vel_vento = math.sqrt(u_wind**2 + v_wind**2) * 1.94384
            # Direção do vento em graus meteorológicos
            dir_vento = (270 - math.degrees(math.atan2(v_wind, u_wind))) % 360

        # Montar o registro
        registro = {
            "datetime": dt_previsao,
            "temp_ar": temp_c,
            "temp_orvalho": dpt_c,
            "umidade_relativa": rh,
            "vel_vento": vel_vento,
            "dir_vento": dir_vento,
            "pressao": pressao_hpa,
            "gfs_vis": vis
        }
        
        dados_extraidos.append(registro)

    # ==============================================================================
    # SALVAMENTO E EXPORTAÇÃO
    # ==============================================================================
    df = pd.DataFrame(dados_extraidos)
    
    # Criar diretório pai caso não exista
    os.makedirs(os.path.dirname(ARQ_SAIDA), exist_ok=True)
    
    df.to_csv(ARQ_SAIDA, index=False)
    print(f"\nExtração concluída com sucesso!")
    print(f"Total de registros: {len(df)}")
    print(f"Arquivo salvo em: {ARQ_SAIDA}")

if __name__ == "__main__":
    main()
