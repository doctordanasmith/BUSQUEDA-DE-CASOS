library(tidyverse)
library(lubridate)
library(readxl)

POSITIVOS_18_19_20 <- read_csv2("POSITIVOS_18_19_20.csv")

POSITIVOS_21_22 <- read_csv2("POSITIVOS_21_22.csv")

Base <- POSITIVOS_18_19_20 %>% 
  bind_rows(POSITIVOS_21_22)

CNYF <- Base %>% filter(INSTITUCION == "CLINICA DEL NIÑO Y LA MADRE") %>% 
  distinct(Paciente, .keep_all = TRUE) %>% 
  select(HC, Paciente, Edad, Procedencia)

write_excel_csv2(CNYF, "CNYF.csv")

SANATORIO_BELGRANO <- Base %>% 
  filter(INSTITUCION == "SANATORIO BELGRANO") %>% 
  distinct(Paciente, .keep_all = TRUE) %>% 
  select(HC, Paciente, Edad, Procedencia)

write_excel_csv2(SANATORIO_BELGRANO, "SANATORIO_BELGRANO.csv")
