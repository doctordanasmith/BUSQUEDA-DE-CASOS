library(tidyverse)

library(lubridate)

library(readxl)

CANREG <- read.csv("CANREG.csv")

HOUSSAY_2019_1_semestre <- read.csv("HOUSSAY 2019 - 1 semestre.csv") %>% 
  select(PACIENTE, DOCUMENTO, F_NACIMIENTO, EDAD_AÃ.os_, F__INICIO,DIAGNÃ.STICO ,
         DIRECCIÃ.N ) %>% rename(No.Documento = DOCUMENTO) 
HOUSSAY_2019_2_semestre <- read.csv("HOUSSAY 2019 - 2 semestre.csv") %>% 
  rename(No.Documento = DOCUMENTO)
HOUSSAY_2020_1_semestre <- read.csv("HOUSSAY 2020 - 1 semestre.csv") %>% 
  rename(No.Documento = DOCUMENTO)
HOUSSAY_2020_2_semestre <- read.csv("HOUSSAY 2020 - 2 semestre.csv") %>% 
  rename(No.Documento = DOCUMENTO)
HOUSSAY_2021_01 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 1) %>% 
  mutate(No.Documento = as.character(DOCUMENTO)) %>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_02 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 2)%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_03 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 3)%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_04 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 4)%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_05 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 5)%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_06 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 6)%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_07 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 7)%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_08 <- read_xlsx("HOUSSAY 2021 - 01 al 08.xlsx", sheet = 8)%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_09 <- read_xlsx("HOUSSAY 2021 - 09.xlsx")%>% 
  rename(No.Documento = DOCUMENTO)%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_10 <- read_xlsx("HOUSSAY 2021 - 10.xlsx")%>% 
  mutate(No.Documento = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))
HOUSSAY_2021_11 <- read_xlsx("HOUSSAY 2021 - 11.xlsx")%>% 
  mutate(DOCUMENTO = as.character(DOCUMENTO))%>% 
  mutate(ATENDIDO = as.character(ATENDIDO))

HOUSSAY_2019_AL_2021_11 <- HOUSSAY_2019_1_semestre %>% bind_rows(
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
  HOUSSAY_2021_09,
  HOUSSAY_2021_10,
  HOUSSAY_2021_11)

PAMI_2021_09 <- read_xlsx("PAMI 2021-09.xlsx")



##PARA LEER BASE POR BASE HAY QUE USAR READ_EXCEL..SHEET..?

##VER SI LAS VARIABLES FUERON ESCRITAS IGUAL EN TODAS LAS BASES

##UNIR TODAS LAS BASES DEL HOUSSAY, CRUZARLO CON BUFFELLI Y AL FINAL, PARA QUE NO
##SE NOS ESCAPE NINGUN CASO, CRUZAR TODOS CON LA CAPITA DE ONCOLOGICOS 
##LA CAPITA DE ONCOLOGICOS TAMBIEN NOS PUEDE SERVIR PARA OBTENER LOS DNI DE LOS DE PAMI

Base_092021 <- Base_092021 %>%  mutate(F_NACIMIENTO =  as_date(F_NACIMIENTO), 
                                       F__INICIO = as_date(F__INICIO)) %>%
  rename(`NRO DOC` = DOCUMENTO, `FECHA RECEP` = F__INICIO,`FECHA NAC`= F_NACIMIENTO,
          EDAD = EDAD_Años_, DIAGNOSTICO = `DIAGNOSTICO DE EGRESO`, 
          DIAGNOSTICO_INICIAL = `DIAGNÓSTICO DE INGRESO`, DOMICILIO = DIRECCIÓN) %>% 
  mutate(DIAGNOSTICO = str_to_upper(DIAGNOSTICO),
         DIAGNOSTICO_INICIAL = str_to_upper(DIAGNOSTICO_INICIAL))


Base_01_082021 <- read_xlsx("BASE 01-082021.xlsx", sheet = ................) ##probando

##RENAME Y MUTATE

Base_2020_1 <- read.csv("Pac admitidos 1er semestre 2020.csv")

##RENAME Y MUTATE

Base_2020_2 <- read.csv("Pac admitidos 2do semestre 2020.csv")

##RENAME Y MUTATE

Base_2019_1 <- read.csv("Pac admitidos 1er sermestre 2019.csv")

##RENAME Y MUTATE

Base_2019_2 <- read.csv("Pac admitidos 2do semestre 2019.csv")

##RENAME Y MUTATE

Base <- Base_092021 %>%  bind_rows(.........................)




Base_conCANCER <- Base %>% mutate (CANCER = case_when(str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "LINFOMA") ~ "SI",
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
                                                    str_detect(string = DIAGNOSTICO, pattern = "TUMOR MALIGNO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "NEOPLASIA MALIGNA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "NEOPLASICO MALIGNO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "CARCINOMATOSIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "MALIGNO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "MALIGNA") ~ "SI",
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
                                                    str_detect(string = DIAGNOSTICO, pattern = "BLASTICA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "MACROGLOBULINEMIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "MIELOESCLEROSIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "MIELOFIBROSIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "TROMBOCITOPENIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "SERTOLI") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "LEYDIG") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "BOWEN ") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "LINFOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "CARCINOIDE") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MELANOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "LEUCEMIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "SARCOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MIELOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "PLASMOCITOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MESOTELIOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "ASTROCITOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "OLIGODENDROGLIOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "BLASTOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MENINGIOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "GLIOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "EPENDIMOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "PLEXOS COROIDEOS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "PLEXO COROIDES") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "PINEOCITOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MEDULOEPITELIOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "SCHWANNOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "HEMANGIOPERICITOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MIELOPROLIF") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MIELODISPLAS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "HISTIOCITOSIS DE CELULAS DE LANGERHANS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MATASTASIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "TUMOR MALIGNO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "NEOPLASIA MALIGNA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "NEOPLASICO MALIGNO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "CARCINOMATOSIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MALIGNO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MALIGNA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "PROLACTINOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "CARCINO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "FEOCROMOCITOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "SARCOMATOSIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "SEMINOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "GERMINOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "INVASOR") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "INVASORA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "GERMINALES") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "POLIEMBRIOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "HIPERNEFROMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "HODGKIN") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MICOSIS FUNGOIDE") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "SINDROME DE SEZARY") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MALTOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MASTOCITOMA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "BLASTICO") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "BLASTICA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MACROGLOBULINEMIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "POLICITEMIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MIELOESCLEROSIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "MIELOFIBROSIS") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "TROMBOCITOPENIA") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "BOWEN") ~ "SI",
                                                    str_detect(string = DIAGNOSTICO_INICIAL, pattern = "ANEMIA REFRACTARIA") ~ "SI")) %>% 
  mutate (AÑO = case_when(str_detect(string = `FECHA RECEP`, pattern = "^2018") ~ "2018", 
                          str_detect(string = `FECHA RECEP`, pattern = "^2019") ~ "2019",
                          str_detect(string = `FECHA RECEP`, pattern = "^2020") ~ "2020",
                          str_detect(string = `FECHA RECEP`, pattern = "^2021") ~ "2021"))


POSITIVOS <- Base_conCANCER %>% filter(CANCER == "SI") %>% 
  select(PACIENTE, `NRO DOC`, GÉNERO, `FECHA NAC`, EDAD, `FECHA RECEP`, DIAGNOSTICO_INICIAL, DIAGNOSTICO,
         DOMICILIO, MES, AÑO) %>% 
  arrange(`NRO DOC`)


POSITIVOS_092021 <- POSITIVOS %>% distinct(`NRO DOC`, .keep_all = TRUE)

PAMI_092021_ONCOCAPITADOS <- read_xlsx("Capita de Onco  092021.xlsx") 

POSITIVOS_092021_CAPITADOS <- POSITIVOS_092021 %>% semi_join(PAMI_092021_ONCOCAPITADOS)

POSITIVOS_092021_NOCAPITADOS <- POSITIVOS_092021 %>%  anti_join(PAMI_092021_ONCOCAPITADOS)

write_excel_csv(POSITIVOS_092021_CAPITADOS, "POSITIVOS_092021_CAPITADOS.csv")

write_excel_csv(POSITIVOS_092021_NOCAPITADOS, "POSITIVOS_092021_NOCAPITADOS.csv")
