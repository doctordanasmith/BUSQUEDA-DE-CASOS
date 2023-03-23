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
  arrange(procedencia) %>% 
  rename(protocolo_patologa = protocolo)





## POSITIVOS

POSITIVOS_HOUSSAY <- POSITIVOS %>% 
  filter(procedencia == "HOUSSAY") %>% 
  distinct(paciente, .keep_all = T) %>% 
  select(procedencia, protocolo_patologa, paciente) %>% 
  arrange(paciente)

write_excel_csv2(POSITIVOS_HOUSSAY, "PEDIDO DE DNI PACIENTES HTAL HOUSSAY PATOLOGA DRA BUFFELLI.csv")


## DUPLICADOS



BASE_CANREG <- read_csv(".csv") %>% 
  unite("paciente", `Apellido Pat.`:Nombres, sep = " ", na.rm= T) %>% 
  mutate(paciente = str_to_upper(paciente)) %>% 
  mutate(`Fecha Diagnós.` = ymd(`Fecha Diagnós.`))




DNI_DUPLICADOS_DEL_CANREG <- BASE_CANREG %>%  
  semi_join(POSITIVOS, by = "paciente") %>% 
  select(paciente, `No Documento`)

DUPLICADOS_DEL_CANREG <- POSITIVOS %>% 
  semi_join(BASE_CANREG) %>% 
  left_join(DNI_DUPLICADOS_DEL_CANREG) %>%
  mutate(ID = row_number()) %>% 
  select(paciente,`No Documento`,  `FECHA RECEP`, TOPOGRAFIA,
         DIAGNOSTICO, clinica, tipo, AÑO, ID)


write_excel_csv(DUPLICADOS_DEL_CANREG, "DUPLICANREG BUFFELLI 2022.csv")  


POSITIVOS_SINDUPLIDECANREG <- POSITIVOS %>% 
  anti_join(BASE_CANREG, by = "paciente" )

CLAVE_DUPLICADOS <- POSITIVOS_SINDUPLIDECANREG %>% 
  count(paciente, sort = T) %>% 
  filter(n > 1) 

DUPLICADOS_DE_LA_PROPIA_BASE <- POSITIVOS_SINDUPLIDECANREG %>% 
  semi_join(CLAVE_DUPLICADOS) 

PAMI_092021_ONCOCAPITADOS <- read_excel("Cápita ONCO PAMI 2021-09.xlsx") %>% 
  rename(paciente = PACIENTE)

DNI_CAPITADOS092021_PARA_DUPLICADOS_DE_LA_PROPIA_BASE <- PAMI_092021_ONCOCAPITADOS %>% 
  semi_join(DUPLICADOS_DE_LA_PROPIA_BASE, by = "paciente")

DUPLICADOS_DE_LA_PROPIA_BASE_BUFFELLI_CAPITADOS092021 <- 
  DUPLICADOS_DE_LA_PROPIA_BASE %>%  
  semi_join(DNI_CAPITADOS092021_PARA_DUPLICADOS_DE_LA_PROPIA_BASE , by = "paciente" ) %>% 
  left_join(DNI_CAPITADOS092021_PARA_DUPLICADOS_DE_LA_PROPIA_BASE) %>% 
  mutate(ID = row_number()) %>% 
  select(`\`NRO DOC\``, paciente, `FECHA RECEP`, EDAD, TOPOGRAFIA, DIAGNOSTICO, clinica,
         tipo, AÑO)




write_excel_csv(DUPLICADOS_DE_LA_PROPIA_BASE_BUFFELLI_CAPITADOS092021, 
                "DUPLIDELABASE BUFFELLI CAPITADOS 2022.csv")


CLAVE_NODUPLICADOS <- POSITIVOS_SINDUPLIDECANREG %>% count(paciente, sort = T) %>% 
  filter(n == 1)

UNICOS<- POSITIVOS_SINDUPLIDECANREG %>%  semi_join(CLAVE_NODUPLICADOS) 

DNI_CAPITADOS092021_PARA_UNICOS <- PAMI_092021_ONCOCAPITADOS %>% 
  semi_join(UNICOS, by = "paciente")

UNICOS_BUFFELLI_CAPITADOS_2022 <-  DNI_CAPITADOS092021_PARA_UNICOS %>% 
  left_join(UNICOS) %>%
  mutate(ID = row_number()) %>% 
  select(`\`NRO DOC\``, paciente, `FECHA RECEP`, TOPOGRAFIA,
         DIAGNOSTICO, clinica, tipo, AÑO, ID)


write_excel_csv(UNICOS_BUFFELLI_CAPITADOS_2022, "UNICOS_BUFFELLI_CAPITADOS 2022.csv")

UNICOS_BUFFELLI_NO_CAPITADOS_2022 <- UNICOS %>% 
  anti_join(UNICOS_BUFFELLI_CAPITADOS_2022)

DUPLICADOS_DE_LA_PROPIA_BASE_BUFFELLI_NO_CAPITADOS_2022 <- 
  DUPLICADOS_DE_LA_PROPIA_BASE %>% 
  anti_join(DNI_CAPITADOS_2022_PARA_DUPLICADOS_DE_LA_PROPIA_BASE, 
            by = "paciente") %>% 
  select(procedencia, paciente, `FECHA RECEP`, TOPOGRAFIA,
         DIAGNOSTICO, clinica, tipo, AÑO)


NO_CAPITADOS_NOCANREG <- UNICOS_BUFFELLI_NO_CAPITADOS_2022 %>% 
  bind_rows(DUPLICADOS_DE_LA_PROPIA_BASE_BUFFELLI_NO_CAPITADOS_2022)


## LISTADO QUE ME QUEDA POR SOLICITAR EL DNI

CRUZA_BUFFELLIHOUSSAY <- read_csv2("CRUZA_BUFFELLIHOUSSAY.csv")


DNI <- rep(NA, 1666)

Codigo <- rep(NA, 1666)

PEDIR_DNI_y_CARGAR <- NO_CAPITADOS_NOCANREG %>% 
  bind_cols(DNI = DNI, Codigo = Codigo) %>% 
  arrange(procedencia) 

write_excel_csv2(PEDIR_DNI_y_CARGAR, "PEDIR_DNI_y_CARGAR.csv")

## LISTADO DE INSTITUCIONES PARA SOLICITAR DNIS

INSTITUCIONES_BUFFELLI <- Base %>%  distinct(procedencia)

write_excel_csv2(INSTITUCIONES_BUFFELLI, "INSTITUCIONES_BUFFELLI.csv")

DNI_HOUSSAY <- read_csv("BASEHOUSSAY_2019_012022.csv") %>% 
  select(PACIENTE, NODOC) %>% 
  arrange(PACIENTE) %>% 
  distinct(PACIENTE, .keep_all = T) %>% 
  rename(paciente = PACIENTE)

write_excel_csv(DNI_HOUSSAY, "DNI_HOUSSAY.csv")

DOCUMENTO <-  rep("", 1666)

DNI_NO_CAPITADOS <- NO_CAPITADOS_NOCANREG %>%
  bind_cols(DOCUMENTO = DOCUMENTO) %>% 
  filter(procedencia == "HOUSSAY") %>%
  select(paciente, DOCUMENTO,`FECHA RECEP`, AÑO) %>% 
  arrange(`FECHA RECEP`)

write_excel_csv(DNI_NO_CAPITADOS, "DNI BUSQUEDA HOUSSAY 042022.csv")

## PEDIDO DE DNI AL CED

DNI_CED <- PEDIR_DNI_y_CARGAR  %>% 
  left_join(Base, by ="paciente") %>% 
  filter(procedencia.x == "CED") %>%
  bind_cols(DNI = DOCUMENTO) %>% 
  select(protocolo, DNI, paciente,`FECHA RECEP.x`, AÑO.x) %>% 
  arrange(paciente) %>% 
  distinct(paciente, .keep_all = T)


write_excel_csv2(DNI_CED, "DNI solicitados 062022.csv")

CED <- NO_CAPITADOS_NOCANREG %>% 
  filter(procedencia == "CED") %>% 
  arrange(paciente)

CED_DNI_BUFFELLI <- read_excel("DNI s Dra BUFFELLI final.xlsx")

CED_BUFFELLI <- CED %>% left_join(CED_DNI_BUFFELLI, by = "paciente") %>% 
  select(paciente, DOCUMENTO, EDAD, `FECHA RECEP.x`, TOPOGRAFIA, DIAGNOSTICO, clinica,
         tipo, AÑO.x) %>% 
  rename(`No Documento`= DOCUMENTO) %>% 
  mutate(`No Documento`= as.double(`No Documento`))

CED_BUFFELLI_DUPLICONCANREG <- CED_BUFFELLI %>% 
  semi_join(BASE_CANREG, by= "No Documento") %>% 
  mutate(ID = row_number())


write_excel_csv2(CED_BUFFELLI_DUPLICONCANREG, "CED_BUFFELLI_DUPLICONCANREG.csv")


CED_BUFFELLI_UNICOS <- CED_BUFFELLI %>% 
  anti_join(BASE_CANREG, by = "No Documento") %>% 
  mutate(ID = row_number())

write_excel_csv2(CED_BUFFELLI_UNICOS, "CED_BUFFELLI_UNICOS.csv")


RADIOLOGICO <- NO_CAPITADOS_NOCANREG %>% 
  filter(procedencia == "INSTITUTO RADIOLOGICO") %>% 
  arrange(paciente)

write_excel_csv2(RADIOLOGICO, "RADIOLOGICO BUFFELLI.csv")

RADIOLOGICO_BUFFELLI_DNI <- read_excel("RADIOLOGICO BUFFELLI DNI.xlsx")

BUFFELLI_RADIOLOGICO <- RADIOLOGICO %>% 
  left_join(RADIOLOGICO_BUFFELLI_DNI) %>% 
  filter(!is.na(DNI)) %>% 
  select(-procedencia, - protocolo) %>% 
  arrange(`FECHA RECEP`) %>% 
  arrange(paciente)

write_excel_csv2(BUFFELLI_RADIOLOGICO, "BUFFELLI_RADIOLOGICO.csv") ## convierto DNI en códigos y los
## agrego a la base (uso el Hash)

BUFFELLI_RADIOLOGICO_concodigosyDNI <- read_xlsx("BUFFELLI_RADIOLOGICO_concodigosyDNI.xlsx") %>% 
  mutate(`No Documento` = DNI) %>% 
  arrange(`No Documento`)

BASECANREG <- read_csv("2022 - 11 - 08.csv")

BASE_LARIOJA_2021 <- read_csv2("UNICOS_2021_LARIOJA.csv")

BASE_LARIOJA_2022 <- read_csv2("UNICOS_2022_LARIOJA.csv")

BASE_COLON_2021 <- read_csv2("UNICOS_2021.csv")

BASE_COLON_2022 <- read_csv2("UNICOS_2022.csv")


DUPLICADOS_POMARCANREG_2021_porDNI <- BUFFELLI_RADIOLOGICO_concodigosyDNI %>%  
  semi_join(BASECANREG, by= "No Documento" ) 

DUPLICADOS_POMARCANREG_2021_porcodigo <- BUFFELLI_RADIOLOGICO_concodigosyDNI %>%  
  semi_join(BASECANREG, by = "Codigo") 

DUPLICADOS_LARIOJA_2021 <- BUFFELLI_RADIOLOGICO_concodigosyDNI %>%
  semi_join(BASE_LARIOJA_2021, by = "Codigo")

DUPLICADOS_LARIOJA_2022 <- BUFFELLI_RADIOLOGICO_concodigosyDNI %>%
  semi_join(BASE_LARIOJA_2022, by = "Codigo")

DUPLICADOS_COLON_2021 <- BUFFELLI_RADIOLOGICO_concodigosyDNI %>% 
  semi_join(BASE_COLON_2021, by = "Codigo")

DUPLICADOS_COLON_2022 <- BUFFELLI_RADIOLOGICO_concodigosyDNI %>% 
  semi_join(BASE_COLON_2022, by = "Codigo")

DUPLICADOS_2022 <- DUPLICADOS_POMARCANREG_2021_porDNI %>% 
  bind_rows(DUPLICADOS_POMARCANREG_2021_porcodigo, DUPLICADOS_LARIOJA_2021, 
            DUPLICADOS_LARIOJA_2022, DUPLICADOS_COLON_2021, DUPLICADOS_COLON_2022) %>% 
  mutate(ID = row_number()) %>% 
  select(`No Documento`,Codigo, paciente, `FECHA RECEP`, EDAD, TOPOGRAFIA,DIAGNOSTICO,clinica,
         tipo, AÑO, ID)


write_excel_csv2(DUPLICADOS_2022, "DUPLICADOS_BUFFELIRADIOLOGICO_H2021.csv")  



UNICOS_BUFFELLI_RADIOLOGICO <- BUFFELLI_RADIOLOGICO_concodigosyDNI %>% 
  anti_join(DUPLICADOS_2022, by = "No Documento") %>% 
  mutate(ID = row_number()) %>% 
  select(Codigo, `No Documento`, paciente, `FECHA RECEP`, EDAD, TOPOGRAFIA,DIAGNOSTICO,clinica,
         tipo, AÑO, ID)

write_excel_csv2(UNICOS_BUFFELLI_RADIOLOGICO, "UNICOS_BUFFELLI_RADIOLOGICO_H2021.csv")
