library(tidyverse)
library(officer)
library(fs)
library(glue)
library(lubridate)
library(readxl)


BUFFELLI_2022_1 <-  read_csv2("BUFFELLI_2022_1.csv") 

BUFFELLI_2022_2 <-  read_csv2("BUFFELLI_2022_2.csv")

BUFFELLI_2022   <-  BUFFELLI_2022_1 %>% 
  bind_rows(BUFFELLI_2022_2)




BUFFELLI_2022  <-  BUFFELLI_2022  %>% 
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



BUFFELLI_2022  <- BUFFELLI_2022 %>% 
  mutate(`FECHA RECEP`= fecha, `FECHA RECEP`= dmy(`FECHA RECEP`),
         `FECHA RECEP` = as_date(`FECHA RECEP`),
         DIAGNOSTICO = paste(diagnostico, observaciones)) %>% 
  rename(TOPOGRAFIA= topografia,  EDAD = edad) %>% 
  mutate(TOPOGRAFIA = str_to_upper(TOPOGRAFIA), DIAGNOSTICO = 
           str_to_upper(DIAGNOSTICO), clinica = str_to_upper(clinica), CANCER = 
           case_when(
             str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = " CA[. ]") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "C[AÁ]NCER") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "ONCOL[OÓ]GIC") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LINFOMA") &
               str_detect(string = DIAGNOSTICO, pattern = "ADENOLINFOMA" , negate = T ) ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "CARCINOIDE") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MELANOMA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LEUCEMIA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LLA ") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "SMD ") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LMMC") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LDCG") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LH ") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LNH ") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "HODGKIN") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MTS") ~ "SI",
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
             str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOSIS") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MAT[ÁA]STASIS") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "SECUNDARISMO") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MALIGNA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MALIGNO") ~ "SI",
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
             str_detect(string = DIAGNOSTICO, pattern = "WILMS") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "HODGKIN") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MICOSIS FUNGOIDE") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "S[ÉE]ZARY") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MALTOMA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MASTOCITOMA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STIC")&
               str_detect(string = DIAGNOSTICO, pattern = "MEGALOBLASTICA" , negate = T )&
               str_detect(string = DIAGNOSTICO, pattern = "DECIDUOTROFOBLASTIC" , negate = T ) ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MACROGLOBULINEMIA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MONOCLONAL") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MIELOESCLEROSIS") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MIELOFIBROSIS") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MIELODISPLASIA") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "MIELODISPL[ÁA]SICO") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "INFILTRACI[ÓO]N") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "C[ÉE]LULAS CLARAS") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "TROMBOCITEMIA ESENCIAL") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "BOWEN ") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "S[ÉE]RTOLI") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "LEYDIG") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOMA FIBROSO MALIGNO") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOSIS") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "SENO ENDOD[EÉ]RMICO") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "TUMOR RABDOIDE") ~ "SI",
             str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI")) %>% 
  mutate (AÑO = case_when(str_detect(string = `FECHA RECEP`, pattern = "^2018") ~ "2018", 
                          str_detect(string = `FECHA RECEP`, pattern = "^2019") ~ "2019",
                          str_detect(string = `FECHA RECEP`, pattern = "^2020") ~ "2020",
                          str_detect(string = `FECHA RECEP`, pattern = "^2021") ~ "2021",
                          str_detect(string = `FECHA RECEP`, pattern = "^2022") ~ "2022",
                          str_detect(string = `FECHA RECEP`, pattern = "^2023") ~ "2023"))

POSITIVOS <- BUFFELLI_2022  %>% filter(AÑO %in% c("2018", "2019", "2020", "2021", "2022", "2023"),
                                       CANCER == "SI")%>% 
  select(procedencia, protocolo, paciente,`FECHA RECEP`, EDAD,TOPOGRAFIA, DIAGNOSTICO, clinica, 
         tipo, AÑO) %>% 
  arrange(procedencia) 




POSITIVOS_CED <- POSITIVOS %>% 
  filter(procedencia == "CED") 

DNIs_CONSEGUIDOS <- read_excel("DNIs CED obtenidos el  23 06 2023.xlsx")

POSITIVOS_CED <- POSITIVOS_CED %>% 
  left_join(DNIs_CONSEGUIDOS, by = "protocolo")%>% 
  select(DNI,  Codigo, paciente, `FECHA.RECEP`, EDAD, TOPOGRAFIA, DIAGNOSTICO, clinica, tipo) 

BASE_CANREG<- read_csv(".csv")

DNI_y_codigo_CANREG <- read_excel("2022 - 09 - 09 codigos.xlsx") %>% 
  rename(Codigo = "Hash MD5")

DUPLICADOS_CODIGOS <- POSITIVOS_CED %>%  
  semi_join(BASE_CANREG, by = "Codigo" ) %>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_CODIGOS, "DUPLICADOS_CODIGOS_BUFFELLI CED 2022.csv")

DUPLICADOS_DNI <- POSITIVOS_CED %>% 
  semi_join(DNI_y_codigo_CANREG, by = "DNI")%>% 
  arrange(DNI) %>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_DNI, "DUPLICADOS_DNI_BUFFELLI CED 2022.csv")

DUPLICADOS_2022 <- DUPLICADOS_CODIGOS %>% 
  bind_rows(DUPLICADOS_DNI) 

UNICOS_2022 <- POSITIVOS_CED %>%  
  anti_join(DUPLICADOS_2022, by = "Codigo" )%>% 
  arrange(Codigo) 


CLAVE_DUPLICADOS <-  UNICOS_2022  %>% 
  count(Codigo, sort = T) %>% 
  filter(n > 1) 

UNICOS_duplicadosdentrodemismabase <-  UNICOS_2022 %>% 
  semi_join(CLAVE_DUPLICADOS)%>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_duplicadosdentrodemismabase, 
                 "UNICOS_2022_duplicadosdentrodemismabase_BUFFELLI CED 2022.csv")

CLAVE_NODUPLICADOS <-  UNICOS_2022 %>% 
  count(Codigo, sort = T) %>% 
  filter(n == 1)  

UNICOS_solounavezenlabase <-  UNICOS_2022  %>% 
  semi_join(CLAVE_NODUPLICADOS)%>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_solounavezenlabase, 
                 "UNICOSH2022_solounavezenlabase_BUFFELLI CED 2022.csv")  



