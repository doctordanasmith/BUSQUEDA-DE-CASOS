library(tidyverse)

library(lubridate)

library(readxl)


DIAGNOSTICO_INICIAL <-  rep(NA, 23727)

HOUSSAY_2019_1_semestre <- read_xlsx("HOUSSAY 2019 - primer semestre.xlsx",
                                     col_types = c(rep("guess", 23),"text", rep("guess", 20))
                                     ) %>% 
  mutate(`FECHA RECEP` = ymd_hms(F__INICIO), 
         `FECHA RECEP` = as_date(`FECHA RECEP`), 
         DIAGNOSTICO = as.character(DIAGNÓSTICO), 
         NODOC = as.character(DOCUMENTO)) %>% 
  bind_cols(DIAGNOSTICO_INICIAL= DIAGNOSTICO_INICIAL) %>% 
  rename(`FECHA NAC` = F_NACIMIENTO) %>% 
 select(PACIENTE, NODOC, `FECHA NAC`, EDAD_Años_, GÉNERO,`FECHA RECEP`, DIAGNOSTICO,
       DIRECCIÓN, DIAGNOSTICO_INICIAL )

DIAGNOSTICO_INICIAL <-  rep(NA, 34510)

HOUSSAY_2019_2_semestre <- read_xlsx("HOUSSAY 2019 - segundo semestre.xlsx",
                                     col_types = c(rep("guess", 23),"text", 
                                                   rep("guess", 20))) %>% 
  mutate(`FECHA RECEP`= ymd_hms(F__INICIO), `FECHA RECEP` = as_date(`FECHA RECEP`), 
         DIAGNOSTICO = as.character(DIAGNÓSTICO), NODOC = as.character(DOCUMENTO)) %>%
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  rename(`FECHA NAC`= F_NACIMIENTO ) %>% 
  select(PACIENTE, NODOC, `FECHA NAC`, EDAD_Años_,GÉNERO, `FECHA RECEP`, DIAGNOSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) 


DIAGNOSTICO_INICIAL <-  rep(NA, 28498)

HOUSSAY_2020_1_semestre <- read_xlsx("HOUSSAY 2020 - primer semestre.xlsx",
                                     col_types = c(rep("guess", 23),"text", 
                                                   rep("guess", 20))) %>% 
  mutate(`FECHA RECEP` = ymd_hms(F__INICIO), `FECHA RECEP` = as_date(`FECHA RECEP`), 
         DIAGNOSTICO = as.character(DIAGNÓSTICO)) %>%
  mutate(NODOC = as.character(DOCUMENTO)) %>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, NODOC, F_NACIMIENTO, EDAD_Años_,GÉNERO,`FECHA RECEP` , DIAGNOSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename( `FECHA NAC`= F_NACIMIENTO ) 


DIAGNOSTICO_INICIAL <-  rep(NA, 24388)

HOUSSAY_2020_2_semestre <- read_xlsx("HOUSSAY 2020 - segundo semestre.xlsx",
                                     col_types = c(rep("guess", 23),"text", 
                                                   rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO), NODOC = as.character(DOCUMENTO)) %>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, NODOC, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% 
  rename(DIAGNOSTICO = DIAGNÓSTICO, `FECHA RECEP` = F__INICIO, 
         `FECHA NAC`= F_NACIMIENTO )

DIAGNOSTICO_INICIAL <-  rep(NA, 6333)

HOUSSAY_2021_01 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 1,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20)) ) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  bind_cols(DIAGNOSTICO_INICIAL= DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO )

DIAGNOSTICO_INICIAL <-  rep(NA, 6177)

HOUSSAY_2021_02 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 2,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  bind_cols(DIAGNOSTICO_INICIAL =DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO ) 

DIAGNOSTICO_INICIAL <-  rep(NA, 7972)

HOUSSAY_2021_03 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 3,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_, GÉNERO,F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN,DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO )

DIAGNOSTICO_INICIAL <-  rep(NA, 7113)

HOUSSAY_2021_04 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 4,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO ) 

DIAGNOSTICO_INICIAL <-  rep(NA, 6859)

HOUSSAY_2021_05 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 5,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO ) 

DIAGNOSTICO_INICIAL <-  rep(NA, 7467)

HOUSSAY_2021_06 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 6,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO ) 

DIAGNOSTICO_INICIAL <-  rep(NA, 7449)

HOUSSAY_2021_07 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 7,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>%
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO )

DIAGNOSTICO_INICIAL <-  rep(NA, 7514)

HOUSSAY_2021_08 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 8,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 20))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO), 
         DIAGNÓSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, DIAGNÓSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL ) %>% rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNÓSTICO, 
                                `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO ) 

HOUSSAY_2021_09_ingresos <- read_xlsx("HOUSSAY 2021 - 09.xlsx", sheet = 1,
                                      col_types = c(rep("guess", 23),"text", 
                                                    rep("guess", 23))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO)) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, 
         DIAGNÓSTICO_DE_INGRESO, DIAGNOSTICO_DE_EGRESO, DIRECCIÓN ) %>% 
  rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNOSTICO_DE_EGRESO, 
         `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO, 
         DIAGNOSTICO_INICIAL = DIAGNÓSTICO_DE_INGRESO)



`FECHA NAC` <- rep(NA, 7339)

DIRECCIÓN <- rep(NA, 7339) 

DIAGNOSTICO_INICIAL <-  rep(NA, 7339)

GÉNERO <- rep(NA, 7339)

HOUSSAY_2021_09_egresos <- read_xlsx("HOUSSAY 2021 - 09.xlsx", sheet = 2) %>% 
  mutate(`FECHA RECEP` = ymd_hms(Fecha_de_egreso), 
         `FECHA RECEP` =as_date(`FECHA RECEP`), 
         NODOC = as.character(Documento), 
         EDAD_Años_ = Edad, DIAGNOSTICO = Nombre_Diagnóstico) %>% 
        unite("PACIENTE", Nombres:Apellidos, sep = "", na.rm= T) %>% 
  bind_cols(
          `FECHA NAC` = `FECHA NAC` ,DIRECCIÓN = DIRECCIÓN , 
          DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL, GÉNERO = as.character(GÉNERO)) %>% 
    select(PACIENTE, NODOC, EDAD_Años_,GÉNERO, `FECHA RECEP`, 
         DIAGNOSTICO, DIAGNOSTICO_INICIAL, `FECHA NAC` , DIRECCIÓN) 


HOUSSAY_2021_10_ingresos <- read_xlsx("HOUSSAY 2021 - 10.xlsx", sheet = 1) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), F__INICIO = as_date(F__INICIO)) %>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO)) %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_Años_,GÉNERO, F__INICIO, 
         DIAGNÓSTICO_DE_INGRESO, DIAGNOSTICO_DE_EGRESO, DIRECCIÓN ) %>% 
  rename(NODOC = DOCUMENTO, DIAGNOSTICO = DIAGNOSTICO_DE_EGRESO, 
         `FECHA RECEP` = F__INICIO, `FECHA NAC`= F_NACIMIENTO, 
         DIAGNOSTICO_INICIAL = DIAGNÓSTICO_DE_INGRESO )
##CHEQUEAR EL BIND COLS Y EL AS DATE


`FECHA NAC` <- rep(NA, 10353)

DIAGNOSTICO_INICIAL <- rep(NA, 10353) 

DIRECCIÓN <-  rep(NA, 10353)

GÉNERO <- rep(NA, 10353)


HOUSSAY_2021_10_egresos <- read_xlsx("HOUSSAY 2021 - 10.xlsx", sheet = 2) %>%
  mutate(`FECHA RECEP` = ymd_hms(Fecha_de_egreso), 
         `FECHA RECEP` =as_date(`FECHA RECEP`), 
         NODOC = as.character(Documento), 
         EDAD_Años_ = Edad, DIAGNOSTICO = Nombre_Diagnóstico) %>% 
  unite("PACIENTE", Nombres:Apellidos, sep = "", na.rm= T) %>% 
  bind_cols(
    `FECHA NAC` = `FECHA NAC` ,DIRECCIÓN = DIRECCIÓN , 
    DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL, GÉNERO = as.character(GÉNERO))%>%
  select(PACIENTE, NODOC, `FECHA NAC`, EDAD_Años_,GÉNERO, `FECHA RECEP`, 
         DIAGNOSTICO, DIAGNOSTICO_INICIAL, DIRECCIÓN ) 

HOUSSAY_2021_11_ingresos <- read_xlsx("HOUSSAY 2021 - 11.xlsx", sheet = 1,
                             col_types = c(rep("guess", 23),"text", 
                                           rep("guess", 18))) %>% 
  mutate(F__INICIO = ymd_hms(F__INICIO), `FECHA RECEP` = as_date(F__INICIO), 
         DIAGNOSTICO = as.character(DIAGNÓSTICO)) %>% 
  mutate(NODOC = as.character(DOCUMENTO), `FECHA NAC`= F_NACIMIENTO,
         DIAGNOSTICO_INICIAL = `DIAGNOSTICO DE EGRESO`) %>% 
  select(PACIENTE, NODOC,`FECHA NAC` , EDAD_Años_, GÉNERO, `FECHA RECEP`, 
         DIAGNOSTICO, DIAGNOSTICO_INICIAL, DIRECCIÓN ) 

`FECHA NAC` <- rep(NA, 8355 )

DIRECCIÓN <- rep(NA, 8355 )

DIAGNOSTICO_INICIAL <- rep(NA, 8355 )

GÉNERO <- rep(NA, 8355)


HOUSSAY_2021_11_egresos <- read_xlsx("HOUSSAY 2021 - 11.xlsx", sheet = 2) %>%
  mutate(F__INICIO = ymd_hms(Fecha_de_egreso), 
         `FECHA RECEP` =as_date(F__INICIO), 
         NODOC = as.character(Documento), 
         EDAD_Años_ = Edad, DIAGNOSTICO = Nombre_Diagnóstico) %>% 
  unite("PACIENTE", Nombres:Apellidos, sep = "", na.rm= T) %>%
  bind_cols(
    `FECHA NAC` = `FECHA NAC` ,DIRECCIÓN = DIRECCIÓN , 
    DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL, GÉNERO = as.character(GÉNERO))%>%
  select(PACIENTE, NODOC, `FECHA NAC`, EDAD_Años_,GÉNERO, `FECHA RECEP`, 
         DIAGNOSTICO, DIAGNOSTICO_INICIAL, DIRECCIÓN ) 

DIAGNOSTICO_INICIAL <-  rep(NA, 15159)

HOUSSAY_2021_12_2022_01_1 <- read_xlsx("HOUSSAY 2021 - 12 y 2022 - 01.xlsx", sheet = 1,
                                       col_types = c(rep("guess", 23),"text", 
                                                     rep("guess", 20))) %>% 
  bind_cols(DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL) %>% 
  rename(NODOC = DOCUMENTO, `FECHA RECEP` = F__EGRESO, 
         DIAGNOSTICO = `DIAGNÓSTICO DE INGRESO` ) %>% 
  mutate(`FECHA RECEP` = ymd_hms(`FECHA RECEP`), 
         `FECHA RECEP` = as_date(`FECHA RECEP`), `FECHA NAC` = as_date(F_NACIMIENTO),
         NODOC = as.character(NODOC)) %>% 
  select(PACIENTE, NODOC,`FECHA NAC` , EDAD_Años_,GÉNERO, `FECHA RECEP` ,DIAGNOSTICO,
         DIRECCIÓN, DIAGNOSTICO_INICIAL)



`FECHA NAC` <- rep(NA, 15076)

DIRECCIÓN <- rep(NA, 15076)

DIAGNOSTICO_INICIAL <-  rep(NA, 15076)

GÉNERO <- rep(NA, 15076)


  
HOUSSAY_2021_12_2022_01_2 <- read_xlsx("HOUSSAY 2021 - 12 y 2022 - 01.xlsx", sheet = 2,
                                       ) %>% 
  mutate(`FECHA RECEP` = ymd_hms(Fecha_de_ingreso), 
        `FECHA RECEP` =as_date(`FECHA RECEP`), 
         NODOC = as.character(Documento), 
         EDAD_Años_ = Edad, DIAGNOSTICO = Nombre_Diagnóstico ) %>% 
  unite("PACIENTE", Nombres:Apellidos, sep = "", na.rm= T) %>% 
  bind_cols(
    `FECHA NAC` = `FECHA NAC` ,DIRECCIÓN = DIRECCIÓN , 
    DIAGNOSTICO_INICIAL = DIAGNOSTICO_INICIAL, GÉNERO = as.character(GÉNERO))%>%
  select(PACIENTE, NODOC, `FECHA NAC`, EDAD_Años_, GÉNERO, `FECHA RECEP`, 
         DIAGNOSTICO, DIAGNOSTICO_INICIAL, DIRECCIÓN ) 


BASEHOUSSAY_2019_012022 <- HOUSSAY_2019_1_semestre %>% bind_rows(
  HOUSSAY_2019_2_semestre,
  HOUSSAY_2020_1_semestre,
  HOUSSAY_2020_2_semestre,
  HOUSSAY_2021_01,
  HOUSSAY_2021_02,
  HOUSSAY_2021_03,
  HOUSSAY_2021_04,
  HOUSSAY_2021_05,
  HOUSSAY_2021_06,
  HOUSSAY_2021_07,
  HOUSSAY_2021_08,
  HOUSSAY_2021_09_egresos,
  HOUSSAY_2021_09_ingresos,
  HOUSSAY_2021_10_egresos,
  HOUSSAY_2021_10_ingresos,
  HOUSSAY_2021_11_egresos,
  HOUSSAY_2021_11_ingresos,
  HOUSSAY_2021_12_2022_01_1 ,
  HOUSSAY_2021_12_2022_01_2 )

write_excel_csv(BASEHOUSSAY_2019_012022, "BASEHOUSSAY_2019_012022.csv" )





##PARA LEER BASE POR BASE HAY QUE USAR READ_EXCEL..SHEET..?

##VER SI LAS VARIABLES FUERON ESCRITAS IGUAL EN TODAS LAS BASES

##UNIR TODAS LAS BASES DEL HOUSSAY, CRUZARLO CON BUFFELLI Y AL FINAL, PARA QUE NO
##SE NOS ESCAPE NINGUN CASO, CRUZAR TODOS CON LA CAPITA DE ONCOLOGICOS 
##LA CAPITA DE ONCOLOGICOS TAMBIEN NOS PUEDE SERVIR PARA OBTENER LOS DNI DE LOS DE PAMI

BASEHOUSSAY_2019_012022 <- BASEHOUSSAY_2019_012022 %>% 
  rename( EDAD = EDAD_Años_)%>% 
  mutate(DIAGNOSTICO = str_to_upper(DIAGNOSTICO),
         DIAGNOSTICO_INICIAL = str_to_upper(DIAGNOSTICO_INICIAL)) %>% 
  select(NODOC, PACIENTE,`FECHA NAC`, `FECHA RECEP`, EDAD, GÉNERO, DIAGNOSTICO, 
         DIAGNOSTICO_INICIAL,
         DIRECCIÓN) %>% 
  unite("DIAGNOSTICO", DIAGNOSTICO:DIAGNOSTICO_INICIAL, sep = " ", na.rm= T) %>%
  mutate(CANCER = case_when(str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "LINFOMA")&
                       str_detect(string = DIAGNOSTICO, pattern = "ADENOLINFOMA", negate = T)  ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "CARCINOIDE") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MELANOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "LEUCEMIA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "SARCOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MIELOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "PLASMOCITOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MESOTELIOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "ASTROCITOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "OLIGODENDROGLIOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "BLASTOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MENINGIOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "GLIOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "EPENDIMOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "PLEXOS COROIDEOS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "PLEXO COROIDES") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "PINEOCITOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MEDULOEPITELIOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "SCHWANNOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "HEMANGIOPERICITOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MIELOPROLIF") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MIELODISPLAS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOSIS DE CELULAS DE LANGERHANS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MATASTASIS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "TUMOR MALIGNO") & 
                       str_detect(string = DIAGNOSTICO, pattern = "HISTORIA|CIRUGIA PROFILACTICA", negate = T   ) ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "NEOPLASIA MALIGNA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "NEOPLASICO MALIGNO") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "CARCINOMATOSIS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MALIGNO") & 
                                  str_detect(string = DIAGNOSTICO, pattern = "HISTORIA|CIRUGIA PROFILACTICA", negate = T   ) ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MALIGNA") &
                     str_detect(string = DIAGNOSTICO, pattern = "NO MALIGNA", negate = T) ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "PROLACTINOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "CARCINO") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "FEOCROMOCITOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "SARCOMATOSIS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "SEMINOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "GERMINOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "INVASOR") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "INVASORA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "GERMINALES") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "POLIEMBRIOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "HIPERNEFROMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "HODGKIN") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MICOSIS FUNGOIDE") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "SINDROME DE SEZARY") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MALTOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MASTOCITOMA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "BLASTICO") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "BLASTICA") &
                                  str_detect(string = DIAGNOSTICO, pattern = "MEGALOBLASTICA", negate = T) ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MACROGLOBULINEMIA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA") &
                                  str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA SECUNDARIA", negate = T  ) ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MIELOESCLEROSIS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "MIELOFIBROSIS") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "TROMBOCITEMIA") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "BOWEN ") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "SERTOLI") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "LEYDIG") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI")) %>% 
  mutate (AÑO = case_when(str_detect(string = `FECHA RECEP`, pattern = "^2018") ~ "2018", 
                          str_detect(string = `FECHA RECEP`, pattern = "^2019") ~ "2019",
                          str_detect(string = `FECHA RECEP`, pattern = "^2020") ~ "2020",
                          str_detect(string = `FECHA RECEP`, pattern = "^2021") ~ "2021",
                          str_detect(string = `FECHA RECEP`, pattern = "^2022") ~ "2022")) 
    

POSITIVOS_HOUSSAY_2019_012022 <- BASEHOUSSAY_2019_012022 %>% 
  mutate(`FECHA NAC` = ymd(`FECHA NAC`),`FECHA NAC` = as_date(`FECHA NAC`) ) %>% 
  filter(CANCER == "SI")%>% distinct(NODOC, .keep_all = TRUE) %>% 
  select(NODOC, PACIENTE,`FECHA NAC`, `FECHA RECEP`, EDAD, GÉNERO, DIAGNOSTICO, DIRECCIÓN, AÑO) %>% 
  arrange(AÑO)
##CAMBIAR NA POR 0??

write_excel_csv2(POSITIVOS_HOUSSAY_2019_012022,  "POSITIVOS_HOUSSAY_2019_012022.csv")



CANREG <- read_csv("2022 - 04 - 11.csv") %>% 
  mutate(`Fecha Diagnós.` = ymd(`Fecha Diagnós.`), NODOC =as.character(`No Documento`))



DUPLICANREG_HOUSSAY <- POSITIVOS_HOUSSAY_2019_012022 %>%  semi_join(CANREG) %>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICANREG_HOUSSAY, "DUPLICANREG_HOUSSAY.csv")  


UNICOS_HOUSSAY <- POSITIVOS_HOUSSAY_2019_012022 %>% anti_join(CANREG)%>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_HOUSSAY, "UNICOS_HOUSSAY.csv")

CLAVE_DUPLICADOS_PARA_CHEQUEAR <- BASEHOUSSAY_2019_012022 %>% count(NODOC, sort = T) %>% 
  filter(n > 1) 

DUPLIMISMABASEHOUSSAY_PARA_CHEQUEAR <- BASEHOUSSAY_2019_012022 %>% 
  anti_join(CANREG) %>% 
  semi_join(CLAVE_DUPLICADOS_PARA_CHEQUEAR) %>% 
  mutate(`FECHA NAC` = ymd(`FECHA NAC`),`FECHA NAC` = as_date(`FECHA NAC`) ) %>% 
  filter(CANCER == "SI")%>%  
  select(NODOC, PACIENTE,`FECHA NAC`, `FECHA RECEP`, EDAD, DIAGNOSTICO, DIRECCIÓN, AÑO) %>% 
  arrange(AÑO)%>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLIMISMABASEHOUSSAY_PARA_CHEQUEAR, 
                 "DUPLIMISMABASEHOUSSAY_PARA_CHEQUEAR.csv")




##PAMI_092021_ONCOCAPITADOS <- read_excel("Cápita ONCO PAMI 2021-09.xlsx") %>% rename(NODOC =`\`NRO DOC\`` ) 

##POSITIVOS_092021_CAPITADOS <- POSITIVOS_HOUSSAY %>% 
##  semi_join(PAMI_092021_ONCOCAPITADOS, by = "NODOC")








