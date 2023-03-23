library(tidyverse)
library(lubridate)
library(readxl)

HIEMIfiltrada <- read_xlsx("Base HIEMI 2018 2019 2020 Y HASTA 13 - 07 -2021 Parte Filtrada por el ROHA.xlsx") %>% 
  select(Apellido, Nombres, Sexo, Numero_Documento, FechaNacimiento, Fecha_Dx_Confirmado, Anios_al_Dx, topografia, 
         Morfologia, Lateralidad, MetodoDx, Departamento, Domicilio)
glimpse(HIEMIfiltrada)

HIEMImasdeunaenfermedad <- read_xlsx("Base HIEMI 2018 2019 2020 Y HASTA 13 - 07 -2021 Parte Mas de una enfermedad.xlsx") %>% 
  rename(Departamento = departamento) %>% 
  select(Apellido, Nombres, Sexo, Numero_Documento, FechaNacimiento, Fecha_Dx_Confirmado, Anios_al_Dx, topografia, 
         Morfologia, Lateralidad, MetodoDx, Departamento, Domicilio)

HIEMIobservaciones <- read_xlsx("Base HIEMI 2018 2019 2020 Y HASTA 13 - 07 -2021 Parte Observaciones.xlsx") %>% 
  rename(Departamento = departamento) %>% 
  select(Apellido, Nombres, Sexo, Numero_Documento, FechaNacimiento, Fecha_Dx_Confirmado, Anios_al_Dx, topografia, 
         Morfologia, Lateralidad, MetodoDx, Departamento, Domicilio)

HIEMIrecaidas <- read_xlsx("Base HIEMI 2018 2019 2020 Y HASTA 13 - 07 -2021 Parte Recaidas.xlsx") %>% 
  rename(Departamento = departamento) %>% 
  select(Apellido, Nombres, Sexo, Numero_Documento, FechaNacimiento, Fecha_Dx_Confirmado, Anios_al_Dx, topografia, 
         Morfologia, Lateralidad, MetodoDx, Departamento, Domicilio)

HIEMIrefcontrarref <- read_xlsx("Base HIEMI 2018 2019 2020 Y HASTA 13 - 07 -2021 Parte Referencia Contrarreferencia.xlsx") %>% 
  rename(Departamento = departamento) %>% 
  select(Apellido, Nombres, Sexo, Numero_Documento, FechaNacimiento, Fecha_Dx_Confirmado, Anios_al_Dx, topografia, 
         Morfologia, Lateralidad, MetodoDx, Departamento, Domicilio)

HIEMIseguimiento <- read_xlsx("Base HIEMI 2018 2019 2020 Y HASTA 13 - 07 -2021 Parte Seguimiento.xlsx") %>% 
    select(Apellido, Nombres, Sexo, Numero_Documento, FechaNacimiento, Fecha_Dx_Confirmado, Anios_al_Dx, topografia, 
         Morfologia, Lateralidad, MetodoDx, Departamento, Domicilio)

HIEMItratamiento <- read_xlsx("Base HIEMI 2018 2019 2020 Y HASTA 13 - 07 -2021 Parte Tratamiento.xlsx") %>% 
  rename(Departamento = departamento) %>% 
  select(Apellido, Nombres, Sexo, Numero_Documento, FechaNacimiento, Fecha_Dx_Confirmado, Anios_al_Dx, topografia, 
         Morfologia, Lateralidad, MetodoDx, Departamento, Domicilio)


HIEMI2018_2019_2020primeraparte <- HIEMIfiltrada %>%   bind_rows(list(HIEMImasdeunaenfermedad, HIEMIrefcontrarref,
                                                                      HIEMIrecaidas, HIEMIseguimiento, HIEMItratamiento,
                                                                      HIEMIobservaciones)) %>% arrange(Numero_Documento)

glimpse(HIEMI2018_2019_2020primeraparte)

HIEMI2018_2019_2020primeraparte <- HIEMI2018_2019_2020primeraparte %>% 
  mutate(FechaNacimiento = as_date(FechaNacimiento), 
         Fecha_Dx_Confirmado = as_date(Fecha_Dx_Confirmado)) %>%  
  filter(Fecha_Dx_Confirmado > "2017-12-31") %>% 
  distinct(Numero_Documento, topografia, .keep_all = T ) %>% 
  arrange(Apellido)

write_excel_csv(HIEMI2018_2019_2020primeraparte, "HIEMI2018_2019_2020primeraparte.csv")

BUSCAR_HC <- read_xlsx("Listado de fotos HIEMI 2018 2019 2020 2021(primera parte).xlsx")

BUSCAR_HC <- BUSCAR_HC %>%  filter(`H.C. A BUSCAR` == "X") %>%  arrange(AÑO)

write_excel_csv(BUSCAR_HC, "BUSCAR_HC.csv")
