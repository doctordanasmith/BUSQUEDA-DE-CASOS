library(tidyverse)

library(lubridate)

library(readxl)

base_canreg <- read.csv("2023 - 03 - 14.csv")

UNICOS <- read.csv2("UNICOS_BUFFELLI_RADIOLOGICO_H2021.csv") %>% 
   mutate(No.Documento = as.character(No.Documento))

cargados_segun_DNI <- base_canreg %>% 
  semi_join(UNICOS,  by = "No.Documento")

cargados_segun_codigo <- base_canreg %>% 
  semi_join(UNICOS,  by = "Codigo")

cargados <-  cargados_segun_DNI %>% 
  bind_rows(cargados_segun_codigo)

NO_REGISTRADOS_BUFFELLI_2018_2021 <- UNICOS %>% 
  anti_join(cargados,by = "Codigo" ) %>% 
  select(-ID) %>% 
  mutate(ID= row_number())

write_excel_csv2(NO_REGISTRADOS_BUFFELLI_2018_2021, "NO_REGISTRADOS_BUFFELLI_2018_2021.csv")
