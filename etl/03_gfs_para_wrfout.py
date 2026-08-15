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
# 
# ---
# Observações:
# - O script deve ser executado em um ambiente Linux com WRF e WPS instalados e configurados corretamente.
# - O script utiliza o GDEX (NCAR ds084.1 / d084001) para baixar os dados GFS.
# - O script não calcula o tamanho dos arquivos de link, apenas arquivos **criados** pelo WPS, WRF e CSVs.
# - O caminho do working directory do WPS e WRF deve ser definido no arquivo configs/wd_dir.txt.
#   - wd_dir/WPS e wd_dir/WRF devem existir e conter os executáveis do WPS e WRF, respectivamente.
# - O WRF é rodado a partir do diretório test/em_real/WRF dentro do working directory definido.
# ======
# ---- Setup ----
import os
import sys
import urllib.request
from pathlib import Path
import yaml
import datetime as dt
import pandas as pd
import subprocess

# Adiciona o diretório etl/ ao path para importar submódulos
sys.path.insert(0, str(Path(__file__).parent))
from transformacoes.gfs_grib2_to_csv import processar_diretorio_gfs

# Configurações
DIR_ETL = Path(__file__).parent
ARQ_ETAPAS = DIR_ETL / "configs/etapas.yaml"
DIR_DADOS = DIR_ETL / "dados"
DIR_GFS = DIR_DADOS / "gfs"

WD_DIR = Path((DIR_ETL / Path("configs/wd_dir.txt")).read_text().strip())
WPS_DIR = WD_DIR / "WPS"
WRF_DIR = WD_DIR / "test/em_real/WRF"

# URL base do repositório GDEX (NCAR ds084.1 / d084001)
GDEX_BASE_URL = "https://osdf-director.osg-htc.org/ncar/gdex/d084001"

# Globais
tempo_execucao = {
    "extracao_dados_gfs": 0,
    "conversao_dados_gfs_para_csv": 0,
    "geogrid": 0,
    "ungrib": 0,
    "metgrid": 0,
    "real": 0,
    "wrf": 0,
    "convertendo_dados_wrf_para_csv": 0
}
arquivos_gerados = {
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
tamanho_arquivos = {
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
    global tempo_execucao, arquivos_gerados, tamanho_arquivos

    ARQ_ETAPAS.parent.mkdir(parents=True, exist_ok=True)

    # NOTE: Fica mais fácil atribuir os valores globais ao dicionário antes de salvar do que ir atribuindo direto ao dicionário
    etapas['tempo_execucao'] = tempo_execucao
    etapas['arquivos_gerados'] = arquivos_gerados
    etapas['tamanho_arquivos'] = tamanho_arquivos

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
        global arquivos_gerados, tamanho_arquivos

        req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req) as response, open(destino, "wb") as out_file:
            out_file.write(response.read())
        
        print(f"  [Concluído] {destino.name} ({destino.stat().st_size / (1024*1024):.1f} MB)")

        arquivos_gerados['gfs'] += 1
        tamanho_arquivos['gfs'] += destino.stat().st_size

        return True
    except Exception as e:
        print(f"  [ERRO] Falha ao baixar {destino.name}: {e}")
        if destino.exists():
            destino.unlink()
        return False

def extrair_dados_gfs(data_atual: dt.date, hora_run: int = 0, forecast_inicio: int = 0, forecast_fim: int = 24) -> bool:
    """Extrai todos os arquivos GFS (f000 a f024 de 3h em 3h) para a data e hora_run especificadas."""
    print(f"--- Baixando GFS para {data_atual} (Run {hora_run:02d}Z, f{forecast_inicio:03d} a f{forecast_fim:03d}) ---\n")
    sucesso_total = True

    for fh in range(forecast_inicio, forecast_fim + 1, 3):
        url = get_gfs_request_url(data_atual, hora_run=hora_run, hora_forecast=fh)
        nome_arquivo = f"gfs.0p25.{data_atual.strftime('%Y%m%d')}{hora_run:02d}.f{fh:03d}.grib2"
        destino = DIR_GFS / data_atual.strftime('%Y%m%d') / nome_arquivo

        if not baixar_arquivo(url, destino):
            sucesso_total = False

    return sucesso_total

# --- WPS ---
def preencher_namelist_wps(data_inicial: dt.date, data_final: dt.date):
    """Preenche o arquivo namelist.wps com a data atual."""
    print(f"\nPreenchendo namelist.wps para {data_inicial} até {data_final}...")
    template_path = DIR_ETL / "templates" / "template_namelist.wps"
    if not template_path.exists():
        raise FileNotFoundError(f"[Erro] Arquivo {template_path} não encontrado!")

    with open(template_path, "r", encoding="utf-8") as file:
        content = file.read()

    # Substitui as datas no conteúdo do namelist.wps
    content = content.replace("_wps_data_inicial_", data_inicial.strftime("%Y-%m-%d_%H:%M:%S"))
    content = content.replace("_wps_data_final_", data_final.strftime("%Y-%m-%d_%H:%M:%S"))

    namelist_path = WPS_DIR / "namelist.wps"

    with open(namelist_path, "w", encoding="utf-8") as file:
        file.write(content)

def preencher_namelist_input(data_inicial: dt.date, data_final: dt.date):
    """Preenche o arquivo namelist.input com a data atual."""
    print(f"\nPreenchendo namelist.input para {data_inicial} até {data_final}...")
    template_path = DIR_ETL / "templates" / "template_namelist.input"
    if not template_path.exists():
        raise FileNotFoundError(f"[Erro] Arquivo {template_path} não encontrado!")

    with open(template_path, "r", encoding="utf-8") as file:
        content = file.read()

    # Substitui as datas no conteúdo do namelist.input
    content = content.replace("_dia_fim_", data_final.strftime("%d"))
    content = content.replace("_hora_fim_", data_final.strftime("%H"))
    content = content.replace("_mes_fim_", data_final.strftime("%m"))
    content = content.replace("_ano_fim_", data_final.strftime("%Y"))

    content = content.replace("_dia_inicio_", data_inicial.strftime("%d"))
    content = content.replace("_hora_inicio_", data_inicial.strftime("%H"))
    content = content.replace("_mes_inicio_", data_inicial.strftime("%m"))
    content = content.replace("_ano_inicio_", data_inicial.strftime("%Y"))

    content = content.replace("_qte_dias_", str((data_final - data_inicial).days))
    content = content.replace("_qte_horas_", str((data_final - data_inicial).days * 24))
    content = content.replace("_qte_minutos_", str((data_final - data_inicial).days * 24 * 60))
    content = content.replace("_qte_segundos_", str((data_final - data_inicial).days * 24 * 60 * 60))

    namelist_path = WRF_DIR / "namelist.input"

    with open(namelist_path, "w", encoding="utf-8") as file:
        file.write(content)


def rodar_geogrid() -> bool:
    """Roda o Geogrid do WPS."""
    # NOTE: Arquivos geo_em* não são deletados, pois são utilizados em runs futuras: nescessário
    #       modificar manualmente caso mude alguma configuração do domínio do WRF
    # NOTE: O output não é suprimido. Em caso de erro, olhe os logs: "log.*" no diretório do WPS
    print("\nRodando Geogrid...")
    try:
        subprocess.run(
            ["./geogrid.exe"],
            cwd=WPS_DIR,
            capture_output=False,
            text=True,
            check=True
        )

        print(f"\n[Sucesso] Geogrid concluído em {WPS_DIR}!")

        # Contagem do número e tamanho dos arquivos gerados pelo Geogrid
        arquivos_geogrid = list(Path(WPS_DIR).glob("geo_em*"))
        arquivos_gerados['geogrid'] += len(arquivos_geogrid) # Em geral, são n arquivos para n domínios
        tamanho_arquivos['geogrid'] += sum(f.stat().st_size for f in arquivos_geogrid)

        return True

    except subprocess.CalledProcessError as e:
        print(f"[Erro] Falha ao rodar Geogrid em {WPS_DIR}!")
        print("Return code:", e.returncode)
        print(f"STDERR: {e.stderr}")
        return False

def rodar_link_grib(dir_grib: str) -> bool:
    """Roda o Link Grib do WPS."""
    # NOTE: Nescessário deletar para garantir que os GRIBFILE* antigos não interfiram na execução dos próximos passos do WPS
    grib_files = list(Path(WPS_DIR).glob("GRIBFILE*"))

    if grib_files:
        print("[Aviso] Deletando arquivos GRIBFILE* existentes no WPS_DIR antes de rodar o Link Grib...")
        for arquivo in grib_files:
            arquivo.unlink()
    
    print(f"\nRodando Link Grib para {dir_grib}...")

    try:
        subprocess.run(
            ["./link_grib.csh", f"{dir_grib}/"],
            cwd=WPS_DIR,
            capture_output=False,
            text=True,
            check=True
        )

        print(f"\n[Sucesso] Link Grib concluído para {dir_grib} -> {WPS_DIR}!")
        return True

    except subprocess.CalledProcessError as e:
        print(f"[Erro] Falha ao rodar Link Grib para {dir_grib}!")
        print("Return code:", e.returncode)
        print(f"STDERR: {e.stderr}")
        return False

def rodar_ungrib() -> bool:
    """Roda o Ungrib do WPS."""
    print("\nRodando Ungrib...")
    # Aqui você chamaria o comando do Ungrib, por exemplo:
    # os.system("./ungrib.exe")
    # Para fins de demonstração, vamos apenas simular a execução.
    import time
    time.sleep(2)  # Simula tempo de execução
    arquivos_gerados['ungrib'] += 1
    tamanho_arquivos['ungrib'] += 0 # Exemplo: 1 MB
    return True

def rodar_metgrid() -> bool:
    """Roda o Metgrid do WPS."""
    print("\nRodando Metgrid...")
    # Aqui você chamaria o comando do Metgrid, por exemplo:
    # os.system("./metgrid.exe")
    # Para fins de demonstração, vamos apenas simular a execução.
    import time
    time.sleep(2)  # Simula tempo de execução
    arquivos_gerados['metgrid'] += 1
    tamanho_arquivos['metgrid'] += 0 # Exemplo: 1 MB
    return True

def main():
    global tempo_execucao, arquivos_gerados, tamanho_arquivos

    etapas = carregar_etapas()

    data_inicial = parse_data(etapas.get("data_inicial", "2026-06-01"))
    data_final = parse_data(etapas.get("data_final", "2026-06-30"))
    lat_alvo = etapas.get("lat", -22.804943908755842)
    lon_alvo = etapas.get("long", -43.256455001858306)

    arquivos_gerados = etapas.get('arquivos_gerados', arquivos_gerados)
    tamanho_arquivos = etapas.get('tamanho_arquivos', tamanho_arquivos)
    tempo_execucao = etapas.get('tempo_execucao', tempo_execucao)

    str_mais_recente = etapas.get("data_mais_recente")
    data_mais_recente = parse_data(str_mais_recente) if str_mais_recente else data_inicial

    # Somente se não for a primeira run
    if (data_mais_recente != data_inicial) and (data_mais_recente < data_final):
        primeira_run = False
        print(f"[Aviso] Continuando ETL do GFS a partir de {data_mais_recente} até {data_final}.")
    else:
        primeira_run = True

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
            print("ETAPA 0: Download dos dados GFS\n")
            tempo_execucao['extracao_dados_gfs'] = dt.datetime.now()
            sucesso = extrair_dados_gfs(data_atual, hora_run=0, forecast_inicio=0, forecast_fim=24)
            tempo_execucao['extracao_dados_gfs'] = (dt.datetime.now() - tempo_execucao['extracao_dados_gfs']).total_seconds()

            if sucesso:
                print(f"[Sucesso] Download GFS finalizado com sucesso para {data_atual}! Continuando para etapa 1.")
                etapas['etapa'] = 1
                update_etapas(etapas)
            else:
                msg_erro = f"Falha ao baixar dados GFS para {data_atual}."
                print(f"[Erro] {msg_erro} Interrompendo execução.")
                enviar_email(assunto=f"Erro no ETL GFS para {data_atual}", corpo=msg_erro)
                break

            preencher_namelist_wps(data_atual, data_atual + dt.timedelta(days=1))
            preencher_namelist_input(data_atual, data_atual + dt.timedelta(days=1))
            print("FIM ETAPA 0: Download dos dados GFS\n\n")


        # - Etapa 1: Conversão GFS GRIB2 -> CSV (Passo 1.1) -
        if etapas.get('etapa') == 1:
            print("INI ETAPA 1: Conversão GFS GRIB2 -> CSV\n")
            dir_grib_dia = str(DIR_GFS / data_atual.strftime('%Y%m%d'))
            arq_csv = str(DIR_DADOS / "csv" / f"gfs_{data_atual.strftime('%Y%m%d')}.csv")

            print(f"--- Convertendo GFS GRIB2 para CSV: {data_atual} ---\n")
            try:
                tempo_execucao['conversao_dados_gfs_para_csv'] = dt.datetime.now()

                if Path(arq_csv).exists():
                    print(f"[Existente] CSV já existe para {data_atual}: {arq_csv}!")
                    df = pd.read_csv(arq_csv)
                else:
                    print(f"[PROCESSANDO] Convertendo GFS para CSV para {data_atual}: {arq_csv}...")
                    df = processar_diretorio_gfs(dir_grib_dia, lat_alvo, lon_alvo, arq_csv)

                    if not df.empty:
                        arquivos_gerados['gfs_csv'] += 1
                        tamanho_arquivos['gfs_csv'] += Path(arq_csv).stat().st_size
                
                tempo_execucao['conversao_dados_gfs_para_csv'] = (dt.datetime.now() - tempo_execucao['conversao_dados_gfs_para_csv']).total_seconds()

                if not df.empty:
                    print(f"\n[Sucesso] CSV gerado com sucesso para {data_atual}: {arq_csv}")

                    etapas['etapa'] = 2

                    update_etapas(etapas)
                else:
                    msg_erro = f"Nenhum dado extraído do GFS para {data_atual}."

                    print(f"\n[Erro] {msg_erro}")

                    enviar_email(assunto=f"Erro no ETL GFS (CSV) para {data_atual}", corpo=msg_erro)
                    break

            except Exception as e:
                msg_erro = f"Erro ao converter GFS para CSV em {data_atual}: {e}"
                print(f"[Erro] {msg_erro}")
                enviar_email(assunto=f"Erro no ETL GFS (CSV) para {data_atual}", corpo=msg_erro)
                break
            print(f"\nFIM ETAPA 1: Conversão GFS GRIB2 -> CSV\n")

        # Próximas etapas (2 a 5) serão adicionadas no desenvolvimento incremental
        if etapas.get('etapa') == 2:
            print("INI ETAPA 2: Processamento WPS (Geogrid, Ungrib, Metgrid)\n")
            print(f"[Aguardando Etapa 2] CSV do GFS para {data_atual} está pronto.")

            sucesso = True

            if primeira_run:
                tempo_execucao['geogrid'] = dt.datetime.now()
                sucesso = rodar_geogrid()
                tempo_execucao['geogrid'] = (dt.datetime.now() - tempo_execucao['geogrid']).total_seconds()

                if sucesso:
                    print(f"[Sucesso] Geogrid concluído para {data_atual}!")
                else:
                    print(f"[Erro] Falha ao rodar Geogrid para {data_atual}!")
                    break
            
            sucesso = rodar_link_grib(dir_grib=str(DIR_GFS / data_atual.strftime('%Y%m%d')))

            if sucesso:
                print(f"[Sucesso] Link Grib concluído para {data_atual}!")
            else:
                print(f"[Erro] Falha ao rodar Link Grib para {data_atual}!")
                break

            tempo_execucao['ungrib'] = dt.datetime.now()
            sucesso = rodar_ungrib()
            tempo_execucao['ungrib'] = (dt.datetime.now() - tempo_execucao['ungrib']).total_seconds()
            if sucesso:
                print(f"[Sucesso] Ungrib concluído para {data_atual}!")
            else:
                print(f"[Erro] Falha ao rodar Ungrib para {data_atual}!")
                break

            tempo_execucao['metgrid'] = dt.datetime.now()
            sucesso = rodar_metgrid()
            tempo_execucao['metgrid'] = (dt.datetime.now() - tempo_execucao['metgrid']).total_seconds()
            if sucesso:
                print(f"[Sucesso] Metgrid concluído para {data_atual}!")
            else:
                print(f"[Erro] Falha ao rodar Metgrid para {data_atual}!")
                break

            print("\nFIM ETAPA 2: Processamento WPS\n")
            break  # Placeholder para a próxima etapa

        if etapas.get('etapa') == 5:
            print(f"[Concluído] ETL GFS para {data_atual} finalizado com sucesso!")
            etapas['etapa'] = 0
            update_etapas(etapas)

if __name__ == "__main__":
    main()