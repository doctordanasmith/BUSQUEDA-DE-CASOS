library(tidyverse)

library(lubridate)

library(readxl)

BASE_7_DE_JUNIO <- read_csv("BASE 7 DE JUNIO 2021.csv") 

BASE_7_DE_JUNIO <- BASE_7_DE_JUNIO %>% mutate(`Fecha Diagnós.` = dmy(`Fecha Diagnós.`)) %>% filter(`Fecha Diagnós.` > "2017-12-31")

BASE_7_DE_JUNIO_PENDIENTES <- BASE_7_DE_JUNIO %>%  filter(`Estado caso` == 0)

BASE_7_DE_JUNIO_BORRADO <- BASE_7_DE_JUNIO %>%  filter(`Estado caso` == 2) %>%  select(`No Documento`, `Estado caso`, `Apellido Pat.`, Nombres, `Topografía (desc)`, `Morfología (desc)`, `Fecha Diagnós.`, Profesional, `Institución 2`)

write_excel_csv(BASE_7_DE_JUNIO_BORRADO, 
                 file = "BASE_7_DE_JUNIO_BORRADO.csv")

x <- c("LAGUNA DE LOS PADRES", "SAN FRANCISCO", "LOS ORTIZ", "LOMA ALTA", "CHAPADMALAL",
       "EL TEJADO", "SANTA ISABEL", "ESTACION CAMET", "ESTACION CHAPADMALAL", 
       "EL BOQUERON", "COLONIA BARRAGAN", "VALLE HERMOSO", "EL COYUNCO",
       "GLORIA DE LA PEREGRINA", "EL DORADO", "LAS MARGARITAS", "2 DE ABRIL", 
       "LA ADELA", "EL SOSIEGO", "LAS QUINTAS", "LOS ACANTILADOS", "SAN EDUARDO")
  

NOMARDEL <- BASE_7_DE_JUNIO %>%  filter(`Estado caso` == 1, Profesional %in% x)