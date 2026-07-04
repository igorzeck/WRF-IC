# Script para a junção de arquivos WRFOUt em caasv com metars
library(tidyverse)

wrfout_df <-read_csv("datasets/wrfout/final_csv/wrfout_d04_jan-fev.csv")
metar_df <- read_csv("datasets/metar_SBGL_2026.csv")

vis_df <- metar_df %>% 
  select(datetime, visibility)

merged_df <- left_join(wrfout_df, vis_df, by='datetime')
glimpse(merged_df)

write_csv(merged_df, "datasets/df_treino_d04.csv")
