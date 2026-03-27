# Comparação de tempos de execução do modelo WRF por tempo
# Setup ----
library(dplyr)
library(lubridate)
library(ggplot2)

# Arquivo
arq <- "etl/tempo_exec.md"
linhas <- readLines(arq)

cour_ <- 0
t_u <- 0
t_g <- 0
t_m <- 0
t_r <- 0
t_w <- 0

df_t <- tribble(
  ~cour, ~ungrib, ~geogrid, ~metgrid, ~real, ~wrf, ~input_s
)

for (linha in linhas) {
  if (grepl("Cour", linha)) {
    cour_ <- cour_ + 1
  }
  if (grepl("Ungrib", linha)) {
    t_u <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("Geogrid", linha)) {
    t_g <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("Metgrid", linha)) {
    t_m <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("real.exe", linha)) {
    t_r <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
  }
  if (grepl("wrf.exe", linha)) {
    t_w <- period_to_seconds(ms(unlist(strsplit(linha,":"))[2]))
    
    df_t <- df_t %>% 
      add_row(
        cour = cour_,
        ungrib = t_u,
        geogrid = t_g,
        metgrid = t_m,
        real = t_r,
        wrf = t_w,
        input_s = 6 * cour_ * 3600
      )
  }
}

df_t
# Análise ----
df_t %>% 
  ggplot(aes(input_s, wrf)) +
  geom_line()

## Cour Extra (5) ----
# ## Cour 5:
# #### `Ungrib.exe`
# real	2m45,728s
# user	2m19,958s
# sys	0m18,175s
# #### `Geogrid.exe`
# real	0m6,606s
# user	0m5,701s
# sys	0m0,731s
# #### `Metgrid.exe`
# real	13m51,651s
# user	13m23,625s
# sys	0m20,432s
# #### **`mpirun -np 1 ./real.exe`**
# real	4m6,586s
# user	3m53,106s
# sys	0m4,714s
# #### **`mpirun -np 6 ./wrf.exe`**
# real	3551m3,528s
# user	20812m26,081s
# sys	491m13,292s
df_t <- df_t %>% 
  add_row(
    cour = 5,
    ungrib = period_to_seconds(ms("2m45,728s")),
    geogrid = period_to_seconds(ms("0m6,606s")),
    metgrid = period_to_seconds(ms("13m51,651s")),
    real = period_to_seconds(ms("4m6,586s")),
    wrf = period_to_seconds(ms("3551m3,528s")),
    input_s = 31 * 24 * 3600
  )

## Cria valores para a previsão de tempo com regressão linear ----
modelo = df_t[1:4,] %>%
  lm(data = ., formula = wrf ~ input_s)
modelo
summary(modelo)
# (Intercept)      input_s  
# 48.0120       0.0776 
# t_previsto(t_input) = 0.0776 * t_input + 48.0120
# Comparando com os valores do Cour: 5
df_t[5,]
valor_previsto <- predict(modelo, newdata = df_t[5,])
print(seconds_to_period(valor_previsto))
print(seconds_to_period(df_t[5,]$wrf))
paste0(round((df_t[5,]$wrf - valor_previsto) / df_t[5,]$wrf * 100, 2),"%")
# Erro relativo de 2.43%
# Isso mesmo com todas as diferenças de input (mais domínios, além de área diferente!)