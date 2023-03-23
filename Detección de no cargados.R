library(tidyverse)

library(lubridate)

library(readxl)

base_canreg <- read.csv("2023 - 03 - 14.csv")

unicos_larioja2022 <- read_excel("UNICOS_2022_SINNA.xlsx") %>% 
  mutate(No.Documento = as.character(`No Documento`))

cargados_segun_DNI <- base_canreg %>% 
  semi_join(unicos_larioja2022,  by = "No.Documento")


cargados_segun_codigo <- base_canreg %>% 
  semi_join(unicos_larioja2022,  by = "Codigo")

cargados <-  cargados_segun_DNI %>% 
  bind_rows(cargados_segun_codigo)

NO_CARGADOS_LARIOJA_2022 <- unicos_larioja2022 %>% 
  anti_join(cargados,by = "Codigo" ) %>% 
  select(-ID, -`No Documento`) %>% 
  mutate(ID= row_number())

write_excel_csv2(NO_CARGADOS_LARIOJA_2022, "NO_CARGADOS_LARIOJA_2022.csv")
