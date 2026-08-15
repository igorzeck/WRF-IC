# ======
# Script para, dado uma data inicial e final:
# 1. Coletar dados GFS por meio do GDEX de f00 a f24 (24h) para cada dia do período definido
#    1.1. Transformar os dados GFS em arquivos CSV
# 2. Aplicar dados GFS no WPS para
#    2.0. Rodar o Geogrid (apenas uma vez, no primeiro dia do período definido)
#    2.1. Rodar o Ungrib
#    2.2. Rodar o Metgrid
# 3. Criar arquivos de entrada para o WRF (programa Real)
# 4. Rodar o WRF para cada dia do período definido
#    4.1. Transformar os arquivos de saída do WRF em arquivos CSV
# 5. Apagar arquivos intermediários (GFS, WPS, WRF) para economizar espaço em disco
# 6. Repetir o processo até o final do período ou até o dia atual, o que ocorrer primeiro
#
# A cada erro ou run bem-sucedido, o script envia um e-mail para o usuário com o status do processo.
# ======
# ---- Setup ----
import os
import sys
import urllib.request
from pathlib import Path
import yaml
import datetime as dt
import pandas as pd

# Adiciona o diretório etl/ ao path para importar submódulos
sys.path.insert(0, str(Path(__file__).parent))
from transformacoes.gfs_grib2_to_csv import processar_diretorio_gfs

# Configurações
DIR_ETL = Path(__file__).parent
ARQ_ETAPAS = DIR_ETL / "configs/etapas.yaml"
DIR_DADOS = DIR_ETL / "dados"
DIR_GFS = DIR_DADOS / "gfs"

# URL base do repositório GDEX (NCAR ds084.1 / d084001)
GDEX_BASE_URL = "https://osdf-director.osg-htc.org/ncar/gdex/d084001"

# Globais
total_tempo_execucao = {
    "extracao_dados_gfs": 0,
    "conversao_dados_gfs_para_csv": 0,
    "geogrid": 0,
    "ungrib": 0,
    "metgrid": 0,
    "real": 0,
    "wrf": 0,
    "convertendo_dados_wrf_para_csv": 0
}
total_arquivos_gerados = {
    "gfs": 0,
    "gfs_csv": 0,
    "geogrid": 0,
    "ungrib": 0,
    "metgrid": 0,
    "wrfinput": 0,
    "wrfrst": 0,
    "wrfout": 0,
    "wrfout_csv": 0
}
total_tamanho_arquivos = {
    "gfs": 0,
    "gfs_csv": 0,
    "geogrid": 0,
    "ungrib": 0,
    "metgrid": 0,
    "wrfinput": 0,
    "wrfrst": 0,
    "wrfout": 0,
    "wrfout_csv": 0
}

# ---- Helpers ----
def parse_data(val) -> dt.date:
    """Converte string ou date/datetime para dt.date."""
    if isinstance(val, dt.datetime):
        return val.date()
    elif isinstance(val, dt.date):
        return val
    elif isinstance(val, str):
        return dt.datetime.strptime(val, "%Y-%m-%d").date()
    raise ValueError(f"Formato de data inválido: {val}")

def carregar_etapas() -> dict:
    """Carrega o arquivo de configuração etapas.yaml."""
    if not ARQ_ETAPAS.exists():
        raise FileNotFoundError(f"Arquivo {ARQ_ETAPAS} não encontrado!")
    with open(ARQ_ETAPAS, "r", encoding="utf-8") as file:
        return yaml.safe_load(file) or {}

def update_etapas(etapas: dict):
    """Atualiza o arquivo etapas.yaml mantendo datas no formato YYYY-MM-DD."""
    global total_tempo_execucao, total_arquivos_gerados, total_tamanho_arquivos

    ARQ_ETAPAS.parent.mkdir(parents=True, exist_ok=True)

    # NOTE: Fica mais fácil atribuir os valores globais ao dicionário antes de salvar do que ir atribuindo direto ao dicionário
    etapas['total_tempo_execucao'] = total_tempo_execucao
    etapas['total_arquivos_gerados'] = total_arquivos_gerados
    etapas['total_tamanho_arquivos'] = total_tamanho_arquivos

    etapas_para_salvar = {}
    for k, v in etapas.items():
        if isinstance(v, (dt.date, dt.datetime)):
            etapas_para_salvar[k] = v.strftime("%Y-%m-%d")
        else:
            etapas_para_salvar[k] = v

    with open(ARQ_ETAPAS, "w", encoding="utf-8") as file:
        yaml.dump(etapas_para_salvar, file)

def enviar_email(assunto: str, corpo: str):
    """Envia um e-mail (ou loga status) para o usuário com o status do processo."""
    print(f"\n[EMAIL] Assunto: {assunto}\nCorpo: {corpo}\n")

# ---- Extração GFS ----
def get_gfs_request_url(data: dt.date, hora_run: int = 0, hora_forecast: int = 0) -> str:
    """Retorna a URL do GDEX para um arquivo GFS específico."""
    ano = data.year
    mes = f"{data.month:02d}"
    dia = f"{data.day:02d}"
    hora_str = f"{hora_run:02d}"

    return f"{GDEX_BASE_URL}/{ano}/{ano}{mes}{dia}/gfs.0p25.{ano}{mes}{dia}{hora_str}.f{hora_forecast:03d}.grib2"

def baixar_arquivo(url: str, destino: Path) -> bool:
    """Baixa o arquivo HTTP salvando no disco se ainda não existir."""
    destino.parent.mkdir(parents=True, exist_ok=True)
    if destino.exists() and destino.stat().st_size > 0:
        print(f"  [Existente] {destino.name} ({destino.stat().st_size / (1024*1024):.1f} MB)")
        return True

    print(f"  [Baixando] {destino.name}...", end="\r")
    try:
        global total_arquivos_gerados, total_tamanho_arquivos

        req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req) as response, open(destino, "wb") as out_file:
            out_file.write(response.read())
        
        print(f"  [Concluído] {destino.name} ({destino.stat().st_size / (1024*1024):.1f} MB)")

        total_arquivos_gerados['gfs'] += 1
        total_tamanho_arquivos['gfs'] += destino.stat().st_size

        return True
    except Exception as e:
        print(f"  [ERRO] Falha ao baixar {destino.name}: {e}")
        if destino.exists():
            destino.unlink()
        return False

def extrair_dados_gfs(data_atual: dt.date, hora_run: int = 0, forecast_inicio: int = 0, forecast_fim: int = 23) -> bool:
    """Extrai todos os arquivos GFS (f000 a f024 de 3h em 3h) para a data e hora_run especificadas."""
    print(f"\n--- Baixando GFS para {data_atual} (Run {hora_run:02d}Z, f{forecast_inicio:03d} a f{forecast_fim:03d}) ---")
    sucesso_total = True

    for fh in range(forecast_inicio, forecast_fim + 1, 3):
        url = get_gfs_request_url(data_atual, hora_run=hora_run, hora_forecast=fh)
        nome_arquivo = f"gfs.0p25.{data_atual.strftime('%Y%m%d')}{hora_run:02d}.f{fh:03d}.grib2"
        destino = DIR_GFS / data_atual.strftime('%Y%m%d') / nome_arquivo

        if not baixar_arquivo(url, destino):
            sucesso_total = False

    return sucesso_total

def main():
    global total_tempo_execucao, total_arquivos_gerados, total_tamanho_arquivos

    etapas = carregar_etapas()

    data_inicial = parse_data(etapas.get("data_inicial", "2026-06-01"))
    data_final = parse_data(etapas.get("data_final", "2026-06-30"))
    lat_alvo = etapas.get("lat", -22.804944)
    lon_alvo = etapas.get("long", -43.256455)

    total_arquivos_gerados = etapas.get('total_arquivos_gerados', total_arquivos_gerados)
    total_tamanho_arquivos = etapas.get('total_tamanho_arquivos', total_tamanho_arquivos)
    total_tempo_execucao = etapas.get('total_tempo_execucao', total_tempo_execucao)

    str_mais_recente = etapas.get("data_mais_recente")
    data_mais_recente = parse_data(str_mais_recente) if str_mais_recente else data_inicial

    # Somente se não for a primeira run
    if (data_mais_recente != data_inicial) and (data_mais_recente < data_final):
        print(f"[Aviso] Continuando ETL do GFS a partir de {data_mais_recente} até {data_final}.")

    print(f"ETL GFS iniciado | Período: {data_inicial} até {data_final} | Progresso atual: {data_mais_recente}")

    dias_totais = (data_final - data_inicial).days
    dias_inicio = (data_mais_recente - data_inicial).days

    for offset in range(dias_inicio, dias_totais + 1):
        data_de_agora = dt.date.today()
        
        data_atual = data_inicial + dt.timedelta(days=offset)
        data_status = ""

        # - Verificação de término do processo -
        if data_atual >= data_de_agora:
            data_status = f"Data atual {data_atual} atingiu a data de hoje ({data_de_agora}). Finalizando."
        elif data_atual > data_final:
            data_status = f"Data atual {data_atual} ultrapassou a data final ({data_final}). Finalizando."

        if data_status:
            print(data_status)
            enviar_email(assunto="ETL GFS Concluído", corpo=data_status)
            break

        etapas['data_mais_recente'] = data_atual.strftime("%Y-%m-%d")
        update_etapas(etapas)

        # - Etapa 0: Download dos dados GFS -
        if etapas.get('etapa', 0) == 0:
            total_tempo_execucao['extracao_dados_gfs'] = dt.datetime.now()
            sucesso = extrair_dados_gfs(data_atual, hora_run=0, forecast_inicio=0, forecast_fim=23)
            total_tempo_execucao['extracao_dados_gfs'] = (dt.datetime.now() - total_tempo_execucao['extracao_dados_gfs']).total_seconds()

            if sucesso:
                print(f"[Sucesso] Download GFS finalizado com sucesso para {data_atual}! Continuando para etapa 1.")
                etapas['etapa'] = 1
                update_etapas(etapas)
            else:
                msg_erro = f"Falha ao baixar dados GFS para {data_atual}."
                print(f"[Erro] {msg_erro} Interrompendo execução.")
                enviar_email(assunto=f"Erro no ETL GFS para {data_atual}", corpo=msg_erro)
                break


        # - Etapa 1: Conversão GFS GRIB2 -> CSV (Passo 1.1) -
        if etapas.get('etapa') == 1:
            dir_grib_dia = str(DIR_GFS / data_atual.strftime('%Y%m%d'))
            arq_csv = str(DIR_DADOS / "csv" / f"gfs_{data_atual.strftime('%Y%m%d')}.csv")

            print(f"\n--- Convertendo GFS GRIB2 para CSV: {data_atual} ---")
            try:
                total_tempo_execucao['conversao_dados_gfs_para_csv'] = dt.datetime.now()

                if Path(arq_csv).exists():
                    print(f"[Aviso] CSV já existe para {data_atual}: {arq_csv}!")
                    df = pd.read_csv(arq_csv)
                else:
                    print(f"[PROCESSANDO] Convertendo GFS para CSV para {data_atual}: {arq_csv}...")
                    df = processar_diretorio_gfs(dir_grib_dia, lat_alvo, lon_alvo, arq_csv)

                    if not df.empty:
                        total_arquivos_gerados['gfs_csv'] += 1
                        total_tamanho_arquivos['gfs_csv'] += Path(arq_csv).stat().st_size
                
                total_tempo_execucao['conversao_dados_gfs_para_csv'] = (dt.datetime.now() - total_tempo_execucao['conversao_dados_gfs_para_csv']).total_seconds()

                if not df.empty:
                    print(f"[Sucesso] CSV gerado com sucesso para {data_atual}: {arq_csv}")

                    etapas['etapa'] = 2

                    update_etapas(etapas)
                else:
                    msg_erro = f"Nenhum dado extraído do GFS para {data_atual}."

                    print(f"[Erro] {msg_erro}")

                    enviar_email(assunto=f"Erro no ETL GFS (CSV) para {data_atual}", corpo=msg_erro)
                    break

            except Exception as e:
                msg_erro = f"Erro ao converter GFS para CSV em {data_atual}: {e}"
                print(f"[Erro] {msg_erro}")
                enviar_email(assunto=f"Erro no ETL GFS (CSV) para {data_atual}", corpo=msg_erro)
                break

        # Próximas etapas (2 a 5) serão adicionadas no desenvolvimento incremental
        if etapas.get('etapa') == 2:
            print(f"[Aguardando Etapa 2] CSV do GFS para {data_atual} está pronto.")
            break  # Placeholder para a próxima etapa

        if etapas.get('etapa') == 5:
            print(f"[Concluído] ETL GFS para {data_atual} finalizado com sucesso!")
            etapas['etapa'] = 0
            update_etapas(etapas)

if __name__ == "__main__":
    main()