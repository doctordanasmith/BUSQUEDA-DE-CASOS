library(tidyverse)
library(officer)
library(fs)
library(glue)
library(lubridate)
library(readxl)

CAROLINA <-  read_csv("BUFFELLI 2 (CAROLINA).csv") 

LUCIA <-  read_csv("BUFFELLI 1  (LUCIA).csv")

BASE_CANREG <- read_csv("2022 - 06 - 24.csv") %>% 
  unite("paciente", `Apellido Pat.`:Nombres, sep = " ", na.rm= T) %>% 
  mutate(paciente = str_to_upper(paciente)) %>% 
  mutate(`Fecha Diagnós.` = ymd(`Fecha Diagnós.`))


Base <-  CAROLINA %>% bind_rows(LUCIA)

Base <-  Base %>% 
  mutate(procedencia = case_when(str_detect(string = procedencia,
                                            pattern = "HOUSSAY|HOSPITAL HOUSSAY|HOUSSEY|HOSPITAL BERNARDO HUSSAY|
  HUSSAY|HOSPITAL OUSSAY|HSPITAL HOUSSAY|HOSPIAL HOUSSAY|HOSPITAL HUSSAY|houssay|
  HOAPITAL HOUSSAY|SANATORIO HUSSAY|HOSPITAL HOSSAY|HOSPITAL BERNARDO HUSAY|
  HOSPITAL BERNARDO HOUSSAY|HOSPITAL, HOUSSAY|CEHOSPITAL HOUSSAY|PAMI|
  HOSPPITAL HOUSSAY") ~ "HOUSSAY",
                                 str_detect(string = procedencia,
                                            pattern = "INSTITUTO RADIOLOGICO|INSTITUTO RADIOLÓGICO|INTITUTO RADIOLÓGICO.|
             INSTITUTO RADIOLÒGICO|INSITUTO RADIOLOGICO| INSTUTITO RADIOLOGICO|
             INSTITUTO RADOLOGICO|RADIOLOGICO|INSTITUTO RADIOLOGICO/PAMI|
             INSTITUTO RADIÒLOGICO|INSTOTUTO RADIOLOGICO") ~ "INSTITUTO RADIOLOGICO",
                                 str_detect(string = procedencia,
                                            pattern = "DIAGNÓSTICO GASCÓN|DIAGNÓSTICO MÉDICO GASCÓN|
             DIAGNOSTICO MEDICO GASCON|DIAGNÓSTICO GASCON|DIAGNOSTICO GASCON|
             DIAGNOSTICO MEDICO POR IMAGENES GASCON|DIAGNÒSTICO GASCON| 
             DIAGNÒSTICO GASCÒN|DIAGNÒSTICO MÈDICO GASCÒN|
             DIAGNOSTICO MEDICO POR IMAGENES  GASCON|DIAGNÒSTICO MEDICO GASCÒN|
             INSTITUTO DIAGNOSTICO GASCON|IMAGENES GASCON|DIGNÒSTICO GASCON|
             GASCON") ~ "DIAGNOSTICO GASCON",
                                 str_detect(string = procedencia,
                                            pattern = "CED|CENTRO DE ESTUDIOS DIGESTIVOS|
             CENTRO DE ENDOSCOPIA DIGESTIVA") ~ "CED",
                                 str_detect(string = procedencia,
                                            pattern = "IFEM|IMAGENES FEM") ~ "IFEM",
                                 str_detect(string = procedencia,
                                            pattern = "CLINICA PUEYRREDON|CLÌNICA PUEYRREDÒN") ~ 
                                   "CLINICA PUEYRREDON",
                                 str_detect(string = procedencia,
                                            pattern = "IMÁGENES MDQ|IMAGENES MDQ") ~ "IMAGENES MDQ",
                                 str_detect(string = procedencia,
                                            pattern = "CENTRO OFTALMOLOGICO MDP|CENTRO OFTALMOLOGICO MAR DEL PLATA|
             CENTRO OFTALMILOGICO|CENTRO OFTALMOLOGICO|CENTRO OFTALMOLÓGICO|
             CENTRO OFTALMOLOGICO MAR DEL PALTA") ~ "CENTRO OFTALMOLOGICO",
                                 str_detect(string = procedencia,
                                            pattern = "SANATARIO BELGRANO|SANATORIO BELGRANO") ~ 
                                   "SANATORIO BELGRANO",
                                 str_detect(string = procedencia,
                                            pattern = "CLINICA MITRE") ~ "CLINICA MITRE",
                                 str_detect(string = procedencia,
                                            pattern = "CONSULTORIOS ODONTOLOGICOS|CONSULTORIO ODONTOLOGICO") ~ 
                                   "CONSULTORIOS ODONTOLOGICOS",
                                 str_detect(string = procedencia,
                                            pattern = "DIAGNOSTICO MEDICO POR IMAGENES|
             DIAGNÒSTICO MEDICO POR IMÀGENES|DIAGNÒSTICO MÈDICO|DIAGNÒSTICO MEDICO|
             DIAGNOSTICO MEDICO POR IMÀGENES") ~ "DIAGNOSTICO MEDICO POR IMAGENES",
                                 str_detect(string = procedencia,
                                            pattern = "SANATORIO AVENIDA") ~ "SANATORIO AVENIDA",
                                 str_detect(string = procedencia,
                                            pattern = "HIGA|H.I.G.A") ~ "HIGA",
                                 TRUE ~ "INSTITUCIONES NO ACLARADAS O PARTICULARES"))



Base <- Base %>% 
  mutate(`FECHA RECEP`= fecha, `FECHA RECEP`= dmy(`FECHA RECEP`),
         `FECHA RECEP` = as_date(`FECHA RECEP`),
         DIAGNOSTICO = paste(diagnostico, observaciones)) %>% 
  rename(TOPOGRAFIA= topografia,  EDAD = edad) %>% 
  mutate(TOPOGRAFIA = str_to_upper(TOPOGRAFIA), DIAGNOSTICO = 
           str_to_upper(DIAGNOSTICO), clinica = str_to_upper(clinica), CANCER = 
           case_when(str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
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
                     str_detect(string = DIAGNOSTICO, pattern = "BOWEN ") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "SERTOLI") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "LEYDIG") ~ "SI",
                     str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI")) %>% 
  mutate (AÑO = case_when(str_detect(string = `FECHA RECEP`, pattern = "^2018") ~ "2018", 
                          str_detect(string = `FECHA RECEP`, pattern = "^2019") ~ "2019",
                          str_detect(string = `FECHA RECEP`, pattern = "^2020") ~ "2020",
                          str_detect(string = `FECHA RECEP`, pattern = "^2021") ~ "2021"))

POSITIVOS <- Base %>% filter(AÑO %in% c("2018", "2019", "2020", "2021"),
                             CANCER == "SI")%>% 
  select(procedencia, paciente,`FECHA RECEP`, EDAD,TOPOGRAFIA, DIAGNOSTICO, clinica, 
         tipo,  AÑO)

write_excel_csv2(POSITIVOS, "POSITIVOS_DRABUFFELLI 2018 - 2021.csv")


HOUSSAY_CARGADOS <- BASE_CANREG %>% 
  filter(`Institución 1 (desc)`== "HOUSSAY." )

CRUZA_BUFFELLIHOUSSAY <- HOUSSAY_CARGADOS %>% 
  left_join(POSITIVOS) %>%  filter(!is.na(DIAGNOSTICO)) %>% 
  select(`No Documento`, paciente, `FECHA RECEP`, TOPOGRAFIA,
         DIAGNOSTICO, clinica, tipo, AÑO) %>% 
  arrange(AÑO) %>% 
  mutate(ID = row_number() )





