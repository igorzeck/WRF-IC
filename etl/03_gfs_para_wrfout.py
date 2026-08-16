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
# IMPORTANTE
# Considerações (LEIA ANTES DE RODAR O SCRIPT):
# - O script deve ser executado em um ambiente Linux com WRF e WPS instalados e configurados corretamente.
# - O script utiliza o GDEX (NCAR ds084.1 / d084001) para baixar os dados GFS.
# - O script não calcula o tamanho dos arquivos de link, apenas arquivos **criados** pelo WPS, WRF e CSVs.
# - O caminho do working directory do WPS e WRF deve ser definido no arquivo configs/wd_dir.txt.
#   - wd_dir/WPS e wd_dir/WRF devem existir e conter os executáveis do WPS e WRF, respectivamente.
# - O WRF é rodado a partir do diretório WRF/test/em_real/ dentro do working directory definido.
# - A Vtable do WPS deve estar configurado corretamente e linkada no diretório WPS antes de rodar o script.
# ======
# TODO: Devido ao jeito que as pastas são organizadas ele não funcionaria talvez para horizontes < 24h? Verificar isso
# ---- Setup ----
import os
import sys
import socket
import urllib.request
import urllib.error
from pathlib import Path
import yaml
import datetime as dt
import pandas as pd
import subprocess

# Adiciona o diretório etl/ ao path para importar submódulos
sys.path.insert(0, str(Path(__file__).parent))
from transformacoes.gfs_grib2_para_csv import processar_diretorio_gfs
from transformacoes.wrfout_para_csv import processar_diretorio_wrfout

# Configurações
DIR_ETL = Path(__file__).parent
ARQ_ETAPAS = DIR_ETL / "configs/etapas.yaml"
DIR_DADOS = DIR_ETL / "dados"
DIR_GFS = DIR_DADOS / "gfs"
ARQ_VAR_TARGETS = DIR_ETL.parent / "datasets" / "var_targets.txt"
if Path(DIR_ETL / "configs/wd_dir.txt").exists():
    WD_DIR = Path((DIR_ETL / "configs/wd_dir.txt").read_text().strip())
else:
    print("[ERRO] Arquivo configs/wd_dir.txt não encontrado! Defina o working directory do WPS e WRF.")
    sys.exit(1)
WPS_DIR = WD_DIR / "WPS"
WRF_DIR = WD_DIR / "WRF/test/em_real/"

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
    # TODO: implementar
    print(f"\n[EMAIL] Assunto: {assunto}\nCorpo: {corpo}\n")

# ---- Extração GFS ----
def get_gfs_request_url(data: dt.date, hora_run: int = 0, hora_forecast: int = 0) -> str:
    """Retorna a URL do GDEX para um arquivo GFS específico."""
    ano = data.year
    mes = f"{data.month:02d}"
    dia = f"{data.day:02d}"
    hora_str = f"{hora_run:02d}"

    return f"{GDEX_BASE_URL}/{ano}/{ano}{mes}{dia}/gfs.0p25.{ano}{mes}{dia}{hora_str}.f{hora_forecast:03d}.grib2"

def baixar_arquivo_gfs(url: str, destino: Path, tentativas: int = 3, timeout: int = 180) -> bool:
    """Baixa um arquivo GFS com retentativas, streaming e escrita atômica (por chunk a chunk)."""
    destino.parent.mkdir(parents=True, exist_ok=True)
    if destino.exists() and destino.stat().st_size > 0:
        print(f"  [Existente] {destino.name} ({destino.stat().st_size / (1024*1024):.1f} MB)")
        return True

    global arquivos_gerados, tamanho_arquivos

    temp_path = destino.with_suffix(destino.suffix + ".part")
    for tentativa in range(1, tentativas + 1):
        print(f"  [Baixando] {destino.name} (tentativa {tentativa}/{tentativas})...", end="\r")
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
            with urllib.request.urlopen(req, timeout=timeout) as response, open(temp_path, "wb") as out_file:
                while True:
                    chunk = response.read(1024 * 1024)
                    if not chunk:
                        break
                    out_file.write(chunk)

            if temp_path.stat().st_size <= 0:
                raise ValueError("download vazio")

            temp_path.replace(destino)
            print(f"  [Concluído] {destino.name} ({destino.stat().st_size / (1024*1024):.1f} MB)" + " " * 20)

            arquivos_gerados['gfs'] += 1
            tamanho_arquivos['gfs'] += destino.stat().st_size
            return True

        except (urllib.error.HTTPError, urllib.error.URLError, TimeoutError, socket.timeout, OSError, ValueError) as e:
            print(f"  [ERRO] Falha ao baixar {destino.name} na tentativa {tentativa}: {e}")
            if temp_path.exists():
                temp_path.unlink()

    if destino.exists() and destino.stat().st_size == 0:
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

        if not baixar_arquivo_gfs(url, destino):
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

    if not content.endswith("\n"):
        content += "\n"

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
        return False

def rodar_link_grib(dir_grib: str) -> bool:
    """Roda o Link Grib do WPS."""
    # NOTE: Necessário deletar para garantir que os GRIBFILE* antigos não interfiram na execução dos próximos passos do WPS
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
        return False

def rodar_ungrib() -> bool:
    """Roda o Ungrib do WPS."""
    # NOTE: Inclui a deleção tanto do arquivo FILE:*, mas mantém a Vtable
    intermediate_files = list(Path(WPS_DIR).glob("FILE:*"))

    if intermediate_files:
        print("[Aviso] Deletando arquivos \"intermediários\" existentes no WPS_DIR antes de rodar o Ungrib...")
        for arquivo in intermediate_files:
            arquivo.unlink()
    
    print("\nRodando Ungrib...")
    try:
        subprocess.run(
            ["./ungrib.exe"],
            cwd=WPS_DIR,
            capture_output=False,
            text=True,
            check=True
        )

        print(f"\n[Sucesso] Ungrib concluído em {WPS_DIR}!")

        # Contagem do número e tamanho dos arquivos gerados pelo Ungrib
        arquivos_ungrib = list(Path(WPS_DIR).glob("FILE:*"))
        arquivos_gerados['ungrib'] += len(arquivos_ungrib) # Em geral, são n * horas arquivos para n domínios
        tamanho_arquivos['ungrib'] += sum(f.stat().st_size for f in arquivos_ungrib)

        return True

    except subprocess.CalledProcessError as e:
        print(f"[Erro] Falha ao rodar Ungrib em {WPS_DIR}!")
        print("Return code:", e.returncode)
        return False

def rodar_metgrid() -> bool:
    """Roda o Metgrid do WPS."""
    intermediate_files = list(Path(WPS_DIR).glob("met_em*"))

    if intermediate_files:
        print("[Aviso] Deletando arquivos met_em* existentes no WPS_DIR antes de rodar o Metgrid...")
        for arquivo in intermediate_files:
            arquivo.unlink()
    
    print("\nRodando Metgrid...")
    try:
        subprocess.run(
            ["./metgrid.exe"],
            cwd=WPS_DIR,
            capture_output=False,
            text=True,
            check=True
        )

        print(f"\n[Sucesso] Metgrid concluído em {WPS_DIR}!")

        # Contagem do número e tamanho dos arquivos gerados pelo Metgrid
        arquivos_metgrid = list(Path(WPS_DIR).glob("met_em*"))
        arquivos_gerados['metgrid'] += len(arquivos_metgrid) # Em geral, são n * horas arquivos para n domínios
        tamanho_arquivos['metgrid'] += sum(f.stat().st_size for f in arquivos_metgrid)

        return True

    except subprocess.CalledProcessError as e:
        print(f"[Erro] Falha ao rodar Metgrid em {WPS_DIR}!")
        print("Return code:", e.returncode)
        return False

# --- WRF ---
def rodar_real() -> bool:
    """Roda o Real do WRF."""
    intermediate_files = list(Path(WRF_DIR).glob("wrfinput*")) + list(Path(WRF_DIR).glob("wrfbdy*")) + list(Path(WRF_DIR).glob("met_em*"))

    if intermediate_files:
        print("[Aviso] Deletando arquivos wrfinput*, wrfbdy* e met_em* existentes no WRF_DIR antes de rodar o Real...")
        for arquivo in intermediate_files:
            arquivo.unlink()

    # NOTE: Necessário linkar (ln -sf) arquivos met_em* do WPS para o diretório do WRF antes de rodar o Real
    print("Linkando arquivos met_em* do WPS para o diretório do WRF...")
    met_em_files = list(Path(WPS_DIR).glob("met_em*"))
    for met_em_file in met_em_files:
        link_path = Path(WRF_DIR) / met_em_file.name
        if not link_path.exists():
            link_path.symlink_to(met_em_file)

    print("\nRodando Real...")

    try:
        subprocess.run(
            ["./real.exe"],
            cwd=WRF_DIR,
            capture_output=False,
            text=True,
            check=True
        )

        print(f"\n[Sucesso] Real concluído em {WRF_DIR}!")

        # Contagem do número e tamanho dos arquivos gerados pelo Real
        # NOTE: Deconsidera o arquivo wrfbdy_d01
        arquivos_real = list(Path(WRF_DIR).glob("wrfinput*"))
        arquivos_gerados['wrfinput'] += len(arquivos_real) # Em geral, são n arquivos para n domínios
        tamanho_arquivos['wrfinput'] += sum(f.stat().st_size for f in arquivos_real)

        return True

    except subprocess.CalledProcessError as e:
        print(f"[Erro] Falha ao rodar Real em {WRF_DIR}!")
        print("Return code:", e.returncode)
        return False

def rodar_wrf(cores: int) -> bool:
    """Roda o WRF. NOTE: Para ver o output desse: `tail -f rsl.out.0000` em outro terminal"""
    intermediate_files = list(Path(WRF_DIR).glob("wrfout*")) + list(Path(WRF_DIR).glob("wrfrst*"))

    if intermediate_files:
        print("[Aviso] Deletando arquivos wrfout*, wrfrst* existentes no WRF_DIR antes de rodar o WRF...")
        for arquivo in intermediate_files:
            arquivo.unlink()

    # NOTE: Necessário linkar (ln -sf) arquivos met_em* do WPS para o diretório do WRF antes de rodar o Real
    print("Linkando arquivos met_em* do WPS para o diretório do WRF...")
    met_em_files = list(Path(WPS_DIR).glob("met_em*"))
    for met_em_file in met_em_files:
        link_path = Path(WRF_DIR) / met_em_file.name
        if not link_path.exists():
            link_path.symlink_to(met_em_file)

    print("\nRodando WRF...")

    try:
        subprocess.run(
            ["mpirun", "-np", str(cores), "./wrf.exe"],
            cwd=WRF_DIR,
            capture_output=False,
            text=True,
            check=True
        )

        print(f"\n[Sucesso] WRF concluído em {WRF_DIR}!")

        # Contagem do número e tamanho dos arquivos gerados pelo WRF
        # NOTE: Deconsidera o arquivo wrfbdy_d01
        arquivos_wrfout = list(Path(WRF_DIR).glob("wrfout*"))
        arquivos_wrfrst = list(Path(WRF_DIR).glob("wrfrst*"))
        arquivos_gerados['wrfout'] += len(arquivos_wrfout) # Em geral, são n arquivos para n domínios
        tamanho_arquivos['wrfout'] += sum(f.stat().st_size for f in arquivos_wrfout)
        arquivos_gerados['wrfrst'] += len(arquivos_wrfrst) # Número de arquivos baseado nas configurações em namelist.input
        tamanho_arquivos['wrfrst'] += sum(f.stat().st_size for f in arquivos_wrfrst)

        return True

    except subprocess.CalledProcessError as e:
        print(f"[Erro] Falha ao rodar WRF em {WRF_DIR}!")
        print("Return code:", e.returncode)
        return False

def main():
    global tempo_execucao, arquivos_gerados, tamanho_arquivos

    etapas = carregar_etapas()

    data_inicial = parse_data(etapas.get("data_inicial", "2026-06-01"))
    data_final = parse_data(etapas.get("data_final", "2026-06-30"))
    lat_alvo = etapas.get("lat", -22.804943908755842)
    long_alvo = etapas.get("long", -43.256455001858306)
    
    arquivos_gerados = etapas.get('arquivos_gerados', arquivos_gerados)
    tamanho_arquivos = etapas.get('tamanho_arquivos', tamanho_arquivos)
    tempo_execucao = etapas.get('tempo_execucao', tempo_execucao)

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
            # WRF precisa de 1h a menos para interpolar corretamente
            preencher_namelist_input(data_atual, data_atual + dt.timedelta(hours=23))
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
                    df = processar_diretorio_gfs(dir_grib_dia, lat_alvo, long_alvo, arq_csv)

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
        
        print("=== WPS ===\n")

        if etapas.get('etapa') == 2:
            print("INI ETAPA 2: Geogrid\n")

            sucesso = True

            # NOTE: O Geogrid só precisa ser rodado uma vez, no primeiro dia do período definido. Se já foi rodado, pula essa etapa.
            if not any(Path(WPS_DIR).glob("geo_em*")):
                etapas['geogrid_rodou'] = False
    
            if not etapas.get('geogrid_rodou'):
                tempo_execucao['geogrid'] = dt.datetime.now()
                sucesso = rodar_geogrid()
                tempo_execucao['geogrid'] = (dt.datetime.now() - tempo_execucao['geogrid']).total_seconds()

                if sucesso:
                    print(f"[Sucesso] Geogrid concluído para {data_atual}!")

                    etapas['etapa'] = 3

                    update_etapas(etapas)
                else:
                    print(f"[Erro] Falha ao rodar Geogrid para {data_atual}!")
                    break
            else:
                print(f"[Aviso] Pulando Geogrid para {data_atual} (já foi rodado na primeira run).")
                etapas['geogrid_rodou'] = True
            print("\nFIM ETAPA 2: Geogrid\n")

        if etapas.get('etapa') == 3:
            print("INI ETAPA 3: Link Grib\n")
            sucesso = rodar_link_grib(dir_grib=str(DIR_GFS / data_atual.strftime('%Y%m%d')))

            if sucesso:
                print(f"[Sucesso] Link Grib concluído para {data_atual}!")
                # TODO: Talvez seja interessante criar uma função para atualizar etapas, tempo_execucao, arquivos_gerados e tamanho_arquivos de forma mais organizada
                etapas['etapa'] = 4

                update_etapas(etapas)
            else:
                print(f"[Erro] Falha ao rodar Link Grib para {data_atual}!")
                break
            print("\nFIM ETAPA 3: Link Grib\n")
        
        if etapas.get('etapa') == 4:
            print("INI ETAPA 4: Ungrib\n")
            tempo_execucao['ungrib'] = dt.datetime.now()
            sucesso = rodar_ungrib()
            tempo_execucao['ungrib'] = (dt.datetime.now() - tempo_execucao['ungrib']).total_seconds()
            if sucesso:
                print(f"[Sucesso] Ungrib concluído para {data_atual}!")
                etapas['etapa'] = 5

                update_etapas(etapas)
            else:
                print(f"[Erro] Falha ao rodar Ungrib para {data_atual}!")
                break
            print("\nFIM ETAPA 4: Ungrib\n")

        if etapas.get('etapa') == 5:
            print("INI ETAPA 5: Metgrid\n")
            tempo_execucao['metgrid'] = dt.datetime.now()
            sucesso = rodar_metgrid()
            tempo_execucao['metgrid'] = (dt.datetime.now() - tempo_execucao['metgrid']).total_seconds()
            if sucesso:
                # TODO: Função para mensagens com tabulação, cores e prefixos de sucesso/erro/aviso
                print(f"[Sucesso] Metgrid concluído para {data_atual}!")
                etapas['etapa'] = 6

                update_etapas(etapas)
            else:
                print(f"[Erro] Falha ao rodar Metgrid para {data_atual}!")
                break

            print("\nFIM ETAPA 5: Metgrid\n")
        
        print("=== WRF ===\n")
    
        if etapas.get('etapa') == 6:
            print("INI ETAPA 6: Real\n")
            tempo_execucao['real'] = dt.datetime.now()
            sucesso = rodar_real()
            tempo_execucao['real'] = (dt.datetime.now() - tempo_execucao['real']).total_seconds()
            if sucesso:
                print(f"[Sucesso] Real concluído para {data_atual}!")
                etapas['etapa'] = 7

                update_etapas(etapas)
            else:
                print(f"[Erro] Falha ao rodar Real para {data_atual}!")
                break
            print("\nFIM ETAPA 6: Real\n")
        
        if etapas.get('etapa') == 7:
            print("INI ETAPA 7: WRF\n")
            tempo_execucao['wrf'] = dt.datetime.now()
            sucesso = rodar_wrf(etapas.get('cores', 6))
            tempo_execucao['wrf'] = (dt.datetime.now() - tempo_execucao['wrf']).total_seconds()
            if sucesso:
                print(f"[Sucesso] WRF concluído para {data_atual}!")
                etapas['etapa'] = 8

                update_etapas(etapas)
            else:
                print(f"[Erro] Falha ao rodar WRF para {data_atual}!")
                break
            print("\nFIM ETAPA 7: WRF\n")

        if etapas.get('etapa') == 8:
            print("INI ETAPA 8: Conversão WRF -> CSV\n")
            tempo_execucao['convertendo_dados_wrf_para_csv'] = dt.datetime.now()
            try:
                sucesso = False
                arq_csv = str(DIR_ETL.parent / "datasets" / "wrfout_csv" / f"wrfout_d{int(etapas.get('dom', 4)):02d}_{data_atual.strftime('%Y%m%d')}.csv")
                if Path(arq_csv).exists():
                    sucesso = True
                    print(f"[Existente] CSV já existe para {data_atual}: {arq_csv}!")
                else:
                    df = processar_diretorio_wrfout(
                        wrf_dir=str(WRF_DIR),
                        lat=lat_alvo,
                        lon=long_alvo,
                        arquivo_targets=str(ARQ_VAR_TARGETS),
                        arq_saida=arq_csv,
                        pattern="wrfout*",
                        dom=etapas.get('dom', 4),
                        verbose=True,
                        quiet_unsupported=True,
                    )

                    if df.empty:
                        print(f"[Aviso] Nenhum dado WRFOUT convertido para {data_atual}.")
                    else:
                        sucesso = True
                        print(f"[Sucesso] Conversão WRF -> CSV concluída para {data_atual}: {arq_csv} ({len(df)} registros)")
                        arquivos_gerados['wrfout_csv'] += 1
                        tamanho_arquivos['wrfout_csv'] += Path(arq_csv).stat().st_size

                if sucesso:
                    etapas['etapa'] = 9
                    update_etapas(etapas)
            except Exception as e:
                msg_erro = f"Erro ao converter WRF para CSV em {data_atual}: {e}"
                print(f"[Erro] {msg_erro}")
                enviar_email(assunto=f"Erro no ETL WRF (CSV) para {data_atual}", corpo=msg_erro)
                break
            finally:
                tempo_execucao['convertendo_dados_wrf_para_csv'] = (dt.datetime.now() - tempo_execucao['convertendo_dados_wrf_para_csv']).total_seconds()
            print("\nFIM ETAPA 8: Conversão WRF -> CSV\n")

        if etapas.get('etapa') == 9:
            print("INI ETAPA 9: Limpeza de arquivos intermediários\n")
            # Retira arquivos de input e wrfout, mas mantém as pastas
            pasta_input = DIR_DADOS / "input"
            pasta_wrfout = DIR_DADOS / "wrfout"
            for arquivo in pasta_input.glob("*"):
                arquivo.unlink()
            for arquivo in pasta_wrfout.glob("*"):
                arquivo.unlink()
            
            etapas['etapa'] = 10
            update_etapas(etapas)
            print("\nFIM ETAPA 9: Limpeza de arquivos intermediários\n")
        
        if etapas.get('etapa') == 10:
            print(f"[Concluído] ETL GFS para {data_atual} finalizado com sucesso!")
            etapas['etapa'] = 0
            etapas['data_mais_recente'] = data_atual.strftime("%Y-%m-%d")
            update_etapas(etapas)
            primeira_run = False

    if (data_atual >= data_de_agora) or (data_atual > data_final):
        print("Juntando arquivos csv em um único arquivo final...")
        arq_dir = DIR_ETL.parent / "datasets" / "wrfout_csv"
        dom = int(etapas.get("dom", 4))

        arquivos_csv = sorted(arq_dir.glob(f"wrfout_d{dom:02d}_*.csv"))

        if not arquivos_csv:
            print(f"[Aviso] Nenhum arquivo encontrado para merge em {arq_dir} (domínio d{dom:02d}).")
            enviar_email(
                assunto="ETL GFS Concluído",
                corpo=(
                    f"ETL finalizado para {data_inicial} até {data_final}, "
                    f"mas não há CSVs para merge do domínio d{dom:02d}."
                ),
            )
        else:
            dfs = []
            
            for arquivo in arquivos_csv:
                try:
                    dfs.append(pd.read_csv(arquivo))
                except Exception as e:
                    print(f"[Aviso] Falha ao ler {arquivo.name}: {e}")

            if not dfs:
                print("[Aviso] Nenhum CSV válido para merge após leitura.")
            else:
                df_final = pd.concat(dfs, ignore_index=True)
                arq_final = DIR_ETL.parent / "datasets" / f"{data_inicial}_{data_final}_d{dom:02d}.csv"
                df_final.to_csv(arq_final, index=False)

                print(f"[Sucesso] Arquivo final gerado com sucesso: {arq_final}")
                print(
                    f"[Concluído] ETL GFS finalizado para {data_inicial} até {data_final}. "
                    f"Arquivo final: {arq_final}"
                )
                enviar_email(
                    assunto="ETL GFS Concluído",
                    corpo=(
                        f"ETL finalizado com sucesso para {data_inicial} até {data_final}. "
                        f"Arquivo final: {arq_final}"
                    ),
                )

if __name__ == "__main__":
    main()