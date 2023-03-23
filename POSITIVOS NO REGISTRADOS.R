library(tidyverse)

library(lubridate)

library(readxl)

POSITIVOS_LARIOJJA_resto2022 <- read_excel("POSITIVOS_SIN_NA_resto2022.xlsx") %>% 
  mutate(No.Documento = as.character(No.Documento)) %>% 
  filter(`FECHA RECEP` > "2022-11-15")

BASE_CANREG <- read.csv("2023 - 03 - 14.csv") %>% 
  mutate(No.Docuento = as.character(No.Documento))

DUPLICADOS_POR_DNI <- POSITIVOS_LARIOJJA_resto2022 %>% 
  semi_join(BASE_CANREG, by = "No.Documento") %>% 
  mutate(ID = row_number())

DUPLICADOS_POR_CODIGO <- POSITIVOS_LARIOJJA_resto2022 %>% 
  semi_join(BASE_CANREG , by = "Codigo")%>% 
  mutate(ID = row_number())

DUPLICADOS <- DUPLICADOS_POR_DNI %>% 
  bind_rows(DUPLICADOS_POR_CODIGO)

UNICOS <- POSITIVOS_LARIOJJA_resto2022 %>% 
  anti_join(DUPLICADOS) %>%  
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_POR_DNI, "DUPLICADOS_POR_DNI_resto 2022.csv")

write_excel_csv2(UNICOS, "UNICOS_resto 2022.csv")

write_excel_csv2(DUPLICADOS_POR_CODIGO, "DUPLICADOS_POR_CODIGO_resto 2022.csv")
