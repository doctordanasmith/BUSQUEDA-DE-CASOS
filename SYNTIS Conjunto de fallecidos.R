library(tidyverse)
library(readxl)
library(lubridate)


fallecidos_A_mas_B <- read_excel("fallecidos_A_mas_B.xlsx") |>
  mutate(`No Documento` = as.character(`No Documento`))|>
  arrange(`No Documento`) |>
  mutate(FFALL = ymd(FFALL)) 

canreg <- read_csv("2022 - 12 - 16.csv") %>% 
  filter(`Fecha Diagnós.` > "2018/01/01")

canreg_vivos <- canreg %>% 
  filter(`Estado Vital (desc)` %in% c("Vivo","Invalid code.","Desc."))

cruza_vivos_por_DNI <- fallecidos_A_mas_B %>% 
  semi_join(canreg_vivos, by = "No Documento") %>% 
  select(`No Documento`, deno, FFALL) %>% 
  mutate(Codigo = deno)
 
cruza_vivos_por_codigo <- fallecidos_A_mas_B %>% 
  semi_join(canreg_vivos, by = "Codigo") %>% 
  select(Codigo, deno, FFALL, Codigo)

editar_que_esta_fallecido <- cruza_vivos_por_DNI %>% 
  bind_rows(cruza_vivos_por_codigo)

write_excel_csv2(editar_que_esta_fallecido, "editar_que_esta_fallecido.csv")

canreg_fallecidos <- canreg %>% 
  filter(`Estado Vital (desc)` == "Muerto")

cruza_fallecidos_por_DNI <- fallecidos_A_mas_B %>% 
  semi_join(canreg_fallecidos, by = "No Documento") %>% 
  select(`No Documento`, deno, FFALL)%>% 
  mutate(Codigo = deno)

cruza_fallecidos_por_codigo <- fallecidos_A_mas_B %>% 
  semi_join(canreg_fallecidos, by = "Codigo") %>% 
  select(Codigo, deno, FFALL)

chequear_si_fecha_correcta <- cruza_fallecidos_por_DNI %>% 
  bind_rows(cruza_fallecidos_por_codigo)

write_excel_csv2(chequear_si_fecha_correcta, "chequear_si_fecha_correcta.csv")


##%>% rename(`No Documento` = "ID_PERSONA") %>% 
##  mutate(`No Documento` = as.character(`No Documento`))
