# Plano de Ação e Próximos Passos - Iniciação Científica WRF-IC

**Projeto**: Extração e Processamento de Dados Meteorológicos: Modelagem de dados para aplicação ao modelo WRF  
**Autor**: Igor Silva de Carvalho  
**Orientador**: Prof. Dr. João Paulo Ferreira de Mello  
**Instituição**: FATEC Baixada Santista "Rubens Lara" / CENEP - Autoridade Portuária de Santos (APS)  
**Data**: 15 de Agosto de 2026  

---

## 1. Resumo do Parecer Técnico da Banca Avaliadora

> [!NOTE]
> **Resultado da Avaliação**: Aprovado com Mérito (Média Final: **8,1**)  
> **Destaques**: Relevância (**9,0**) e Originalidade (**9,0**). A metodologia e a reprodutibilidade da pipeline foram apontadas como o principal ponto forte do trabalho.

### Notas por Critério
- **Relevância**: 9,0
- **Originalidade**: 9,0
- **Metodologia**: 8,5
- **Revisão Bibliográfica**: 8,0
- **Resultados**: 8,0
- **Estatística**: 7,5
- **Redação**: 7,5
- **Apresentação**: 7,0

---

## 2. Resposta aos Questionamentos da Banca

### Questionamento 1: *Como assegurar que o desempenho MOS não decorre de data leakage?*
- **Diagnóstico**: Na versão parcial, a avaliação do modelo MOS ocorreu sobre a mesma janela temporal ou conjunto de treino.
- **Ação Recomendada**: Implementar uma rotina de **Validação Temporal Rígida (Out-of-Sample)** em que o modelo MOS seja treinado exclusivamente em simulações históricas passadas e testado em um evento futuro totalmente inédito (ex: evento de junho/2026).

### Questionamento 2: *Qual o ganho operacional do WRF em relação ao custo computacional observado?*
- **Diagnóstico**: O WRF exigiu 17h49min de computação para simular 144h.
- **Ação Recomendada**: Elaborar uma seção de **Análise de ROI Computacional**, demonstrando que o WRF reduz o viés (*bias*) em mais de 95% em relação ao GFS e aumenta a resolução temporal de 3h para 1h, justificando o custo computacional em ambientes operacionais com hardware dedicado.

### Questionamento 3: *Como os resultados se comportam em separação temporal rígida treino/teste?*
- **Ação Recomendada**: Gerar uma tabela comparativa no relatório final trazendo métricas de **Treino**, **Validação Cruzada (CV)** e **Teste Independente (Out-of-Sample)**.

### Questionamento 4: *Qual o tamanho mínimo de série histórica simulada para validação robusta?*
- **Ação Recomendada**: Construir um experimento de **Curva de Aprendizado (Learning Curve)** variando o volume da base histórica simulada de treino (ex.: 48h, 96h, 144h, 300h) e medindo a estabilidade das métricas de erro.

### Questionamento 5: *Por que LightGBM foi escolhido em detrimento de outras abordagens?*
- **Ação Recomendada**: Criar uma tabela de *benchmark* de modelos comparando LightGBM com Random Forest, XGBoost e Regressão Linear Baseline, destacando velocidade de treino, uso de memória e métricas preditivas.

---

## 3. Pesquisa Paralela: Visibilímetro do Aeroporto do Galeão (SBGL)

No Aeroporto Internacional do Galeão (SBGL), sob regulação do DECEA / REDEMET / NAV Brasil, a visibilidade meteorológica e o RVR (*Runway Visual Range*) são aferidos por sensores automáticos integrados:

### Equipamentos Utilizados
1. **Sensor de Tempo Presente e Visibilidade (Vaisala PWD22 / PWD52)**:
   - **Fabricante**: Vaisala (Finlândia).
   - **Tecnologia**: *Forward Scattering* (Espalhamento Frontal de Luz Infravermelha em ângulo de ~42°).
   - **Funcionamento**: Mede diretamente o Coeficiente de Extinção Óptica ($\beta$) das gotículas de água suspensas na atmosfera e calcula o MOR (*Meteorological Optical Range*).
2. **Transmissômetro de Pista (Vaisala LT31 / FS11)**:
   - **Tecnologia**: Transmissometria direta por feixe de luz em base fixa nas cabeceiras da pista.

### Conexão com a Pesquisa
- A fórmula teórica de Koschmieder empregada na pesquisa:
  \[
  V = \frac{3,912}{\beta_{\text{total}}}
  \]
  é exatamente a fundamentação física embarcada nos sensores **Vaisala PWD/LT31** para converter extinção óptica ($\beta$) em visibilidade horizontal ($V$).
- Essa informação reforça a fundamentação física do trabalho ao correlacionar as saídas microfísicas do WRF com o sensor real do aeródromo.

---

## 4. Proposta de Reorganização do Repositório (`WRF-IC`)

```text
WRF-IC/
├── README.md                      # Documentação técnica e guia de reprodução
├── environment.yml                # Dependências Python
├── WRF-IC.Rproj                   # Projeto RStudio
│
├── etl/                           # Pipelines de extração e transformação
│   ├── 01_api_redemet_metar.py    # Coleta de dados METAR
│   ├── 02_metar_decod.R           # Decodificação e limpeza dos METARs
│   ├── append_lmlt_to_metar.py    # Extração de LMLT (GRIB1/GRIB2) e fusão com METAR
│   ├── gfs_nc.R                   # Processamento de arquivos GFS NetCDF
│   └── analise_nevoeiro_estacoes.R# Estatísticas sazonais e geração de gráficos
│
├── experiments/                   # Experimentos para validação estrita da banca
│   ├── 01_out_of_sample_validation.R
│   ├── 02_algorithm_benchmark.R
│   └── 03_learning_curve_datasize.R
│
├── models/                        # Scripts de treinamento e modelos salvos
│   ├── train_historical_metar_models.R
│   ├── lightgbm_metar_2011_2025.R
│   └── saved_models/              # Artefatos .rds / .pkl
│
├── datasets/                      # Bases de dados organizadas
│   ├── metar_SBGL_2011_2025_lmlt.csv
│   ├── metar_SBGL_2026_lmlt.csv
│   └── era5/
│
└── resources/                     # Gráficos, tabelas e relatórios exportados
    ├── incidencia_nevoeiro_estacao.csv
    ├── incidencia_nevoeiro_mes.csv
    ├── nevoeiro_heatmap_mes_hora.png
    └── nevoeiro_por_estacao_mes.png
```

---

## 5. Cronograma Recomendado de Próximos Passos

1. **Fase 1 (Experimentos Metodológicos)**:
   - Executar validação *Out-of-Sample* sem *data leakage*.
   - Gerar tabela de *benchmark* de algoritmos (LightGBM vs RF vs XGBoost).
2. **Fase 2 (Escrita e Revisão do Texto)**:
   - Adicionar subseção teórica sobre o visibilímetro Vaisala PWD do Galeão.
   - Revisar gramática, uniformização de siglas e legendas de tabelas/figuras.
3. **Fase 3 (Preparação para Artigo / TCC)**:
   - Finalizar a documentação do repositório `WRF-IC`.
   - Compilar a versão final com os novos resultados de teste independente.
