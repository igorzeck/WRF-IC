# --- Script para a extração de METARs pelo API REDEMET ---
# Link para instruções de acesso: https://ajuda.decea.mil.br/base-de-conhecimento/api-redemet-o-que-e/
# Todos os caminhos referenciados neste arquivo são relativos ao diretório raiz
# Este script gera dois arquivos:
# CSV contendo os METARS
# LOG contendo os METARS que não puderam ser decodificados
import subprocess as sp
import json
import datetime
from datetime import datetime as dt
from math import ceil
from os.path import isfile, join

# Chave fornecida pelo DECEA (Necessário solicitar)
api_key=''
if isfile("api_key.txt"):
    with open("api_key.txt", "r") as arq:
        api_key = arq.readline()
# -- Checagem inicial --
if not api_key:
    print("Chave API não preenchida!")
    exit(-1)
else:
    print("Chave API encontrada!")
# Parâmetros de formatação
dt_pstr = "%Y%m%d"
dt_pstr_data = "%Y-%m-%d"
dt_pstr_comp = "%Y-%m-%d %H:%M:%S"

# Parâmetros modificáveis
data_ini = datetime.date(2025, 1, 1)
data_fim = datetime.date(2025, 12, 31)
localidade=input("Localidade:")

data_ini_str = data_ini.strftime(dt_pstr + "00")
data_fim_str = data_fim.strftime(dt_pstr + "23")

# Arquivo
arq_raw=join('datasets', f"raw_metar_{localidade}_{data_ini.strftime(dt_pstr_data)}_{data_fim.strftime(dt_pstr_data)}.txt")
arq_log=join('datasets', f"metar_{localidade}_{data_ini.strftime(dt_pstr_data)}_{data_fim.strftime(dt_pstr_data)}.log")

log_list = []

## -- Funções --
# -- Extração --
def extrair_metar():
    # Esse menos 1 dia é para o loop ser inclusivo em data de início e fim
    data_meio = data_ini - datetime.timedelta(1)
    linhas_brutas = []
    intervalo_horas=8760
    intervalo=datetime.timedelta(hours=intervalo_horas) # Máximo de entradas é 8760 permitidas em uma chamada...
    cont = 0
    cont_max = (data_fim - data_ini) / intervalo
    if cont_max == 0:
        cont_max = 1

    while data_meio < data_fim:
        # Esse mais 1 dia é para evitar duplicagem em cada loop
        data_meio_ant = data_meio + datetime.timedelta(1)
        data_meio = data_meio_ant + intervalo
        if data_meio > data_fim:
            data_meio = data_fim

        # 0. Comando atual
        data_meio_ant_str = data_meio_ant.strftime(dt_pstr + "00")
        data_meio_str = data_meio.strftime(dt_pstr + "23")

        # Prints
        print(f"Pegando dados de {data_meio_ant} 00:00 até {data_meio} 23:00...",end=" ")
        print((cont, ceil(cont_max)), f"{cont/cont_max*100:.2f}%")
        pagina = 1
        while True:
            # Tem que ir de página em página (cada uma com 150 entradas) pode pegar pelo json
            url_metar = f"https://api-redemet.decea.mil.br/mensagens/metar/{localidade}?&api_key={api_key}&data_ini={data_meio_ant_str}&data_fim={data_meio_str}&page={pagina}"
            # Definição do comando curl
            cmd = ['curl',
                '',
                    url_metar]

            # Passo 1 - curl (dados em json)
            _curl = sp.run(cmd, capture_output=True, text=True)

            # Passo 2 - json
            if _curl.returncode == 0:
                data = json.loads(_curl.stdout)
                # Passo 3 - pega as mensagens (METARs)
                # Data completa da emissão do METAR e METAR em si (mensagem)
                linhas_brutas.extend([item['validade_inicial'] + "<>" + item['mens'] for item in data['data']['data']])
                # Verifica se chegou na página finl
                if pagina == data['data']['last_page']:
                    break
                else:
                    # Senão vai para próxima
                    pagina += 1
            else:
                # print(f"Erro {_curl.stderr}")
                break # Prossegue com o resto do script até a data que conseguiu capturar
        cont += 1
    return linhas_brutas

linhas_metars = extrair_metar()
print("Sucesso!")
print("Criando arquivo .csv e de log...")
## Passo 4 - Arquivos finais
with open(arq_raw, 'w', encoding='utf-8') as arqout:
    arqout.write("datetime<>metar\n")
    arqout.writelines('\n'.join(linhas_metars))

if log_list:
    with open(arq_log, 'w', encoding='utf-8') as arqout:
        arqout.writelines(log_list)
