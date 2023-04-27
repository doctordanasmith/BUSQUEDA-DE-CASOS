library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)


##janitor::convert_to_date() por si quiero ir de el código para atrás a la fecha

Base <- read_excel("2018 - 2022 DESIMONE.xlsx")  %>% 
  mutate(BIOPSIA = case_when(str_detect
                             (string = protocolo, pattern = "^[Bb]")~ "SI")) %>% 
  filter(BIOPSIA == "SI")



##
##mutate(Fecha = as.Date(`fecha diag`)) %>% 
##mutate (AÑO = case_when(str_detect(string = Fecha, pattern = "^2018") ~ "2018", 
##                        str_detect(string = Fecha, pattern = "^2019") ~ "2019",
##                      str_detect(string = Fecha, pattern = "^2020") ~ "2020",
##                    str_detect(string = Fecha, pattern = "^2021") ~ "2021",
##                  str_detect(string = Fecha, pattern = "^2022") ~ "2022",
##                   str_detect(string = Fecha, pattern = "^2023") ~ "2023"      ))

x <- "ced|oftalmologico|osecac|Avenida| 
amec|CED|Dermoestetica|scarano|
Belgrano|Omega|Mitre|
Traumatologos|Interzonal|CEMA|
Ramaciotti|radiologico|UTA|
niño|Pueyrredón|Consultorio|centro medico|inst|
privado|
25|telefo del paciente 2236864837|onsulorio |amec|RADIO
aveni|dermoes|rx|omega|AVEN|CONSUL|consultorio|hpc|aven|Ave|ave|
omunidad|nterzonal|cema"

y <- "daria|DARIA|daga|darie|dagfa|gese"


Base_gralPuey <- Base %>%
  mutate(PUEYRREDON = case_when(str_detect(string = establecimiento, pattern = x) &
                                  str_detect(string = establecimiento, pattern = y , negate = T)
                                ~ "SI"))%>% 
  filter(PUEYRREDON == "SI", material != "PAPANICOLAU") %>% 
  mutate(Topografía = str_to_upper(paste(material, `sitio de toma`, macroscopía,
                                          sep = " ")),
         DIAGNOSTICO = str_to_upper(paste(microscopía, diagnóstico,`diagnóstico citológico`, nota,
                                                 sep = " "))) %>% 
  select(`dni o hc`, edad, sexo,  `fecha diag`, protocolo, Topografía, DIAGNOSTICO)


Base_gralPuey <- Base_gralPuey %>% mutate(CANCER = case_when(
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
  mutate (AÑO = case_when(str_detect(string = protocolo, pattern = "18$") ~ "2018", 
                          str_detect(string = protocolo, pattern = "19$") ~ "2019",
                          str_detect(string = protocolo, pattern = "20$") ~ "2020",
                          str_detect(string = protocolo, pattern = "21$") ~ "2021",
                          str_detect(string = protocolo, pattern = "22$") ~ "2022",
                          str_detect(string = protocolo, pattern = "23$") ~ "2023"
                          ))


POSITIVOS_DESIMONE_18_22 <- Base_gralPuey %>% filter(CANCER == "SI") 


DNI_codificados_Canreg <- read_excel("2022 - 09 - 16 DNI codificados.xlsx") %>% 
  rename(Codigo = `Hash MD5`)


BASE_CANREG <- read_csv("2023 - 04 - 25.csv") %>% 
  mutate(`No Documento` = as.character(`No Documento`))


write_excel_csv2(POSITIVOS_DESIMONE_18_22, "POSITIVOS_DESIMONE_18_22.csv")

## agrego códigos

POSITIVOS_con_codigos <- read_excel("POSITIVOS_DESIMONE_18_22.xlsx") %>% 
  mutate(`No Documento` = as.character(`dni o hc`))

DUPLICADOS_DESIMONE_18_22_porDNIcodificados <- POSITIVOS_con_codigos %>%  
  semi_join(DNI_codificados_Canreg, by= "Codigo") 

DUPLICADOS_DESIMONE_18_22_porDNI <- DUPLICADOS_DESIMONE_18_22_porDNIcodificados %>% 
  arrange(`No Documento`) %>% 
  mutate(ID = row_number()) %>% 
  select(`No Documento`,`fecha diag`, edad,sexo, Topografía, DIAGNOSTICO,
         AÑO, Codigo,
         ID)

write_excel_csv2(DUPLICADOS_DESIMONE_18_22_porDNI, "DUPLICADOS_DESIMONE_18_22_porDNI.csv")

DUPLICADOS_DESIMONE_18_22_porcodigo <- POSITIVOS_con_codigos %>%  
  semi_join(BASE_CANREG, by= "Codigo")  %>% 
  arrange(`No Documento`) %>% 
  mutate(ID = row_number()) %>% 
  select( Codigo,`fecha diag`, edad,sexo, Topografía, DIAGNOSTICO,
AÑO, `No Documento`,
ID)


write_excel_csv2(DUPLICADOS_DESIMONE_18_22_porcodigo, 
                 "DUPLICADOS_DESIMONE_18_22_porcodigo.csv")  

DUPLICADOS <- DUPLICADOS_DESIMONE_18_22_porDNI %>% 
  bind_rows(DUPLICADOS_DESIMONE_18_22_porcodigo)

UNICOS_DESIMONE_18_22 <- POSITIVOS_con_codigos %>%  
  anti_join(DUPLICADOS, by= "No Documento") %>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number()) %>% 
  select(Codigo,`fecha diag`, edad,sexo, Topografía, DIAGNOSTICO,
         AÑO, `No Documento`,
         ID)

write_excel_csv2(UNICOS_DESIMONE_18_22, "UNICOS_DESIMONE_18_22.csv")

## LLAMABA LA ATENCION DE LA BASE PREVIA
## LA CANTIDAD DE DIAGNOSTICOS IGUALES DENTRO DE UNA MISMA TOPOGRAFIA
## POR ESO SE CHEQUEÓ MAMAS Y RIÑONES
## LUEGO, HABLANDO CON LA DOCTORA, SE DESCUBRIO QUE LA EXPORTACION TUVO UN PROBLEMA
## Y ES QUE SE HABIAN EXPORTADO DIAGNOSTICOS INCORRECTOR JUNTO A INFORMES DE PAP
## ENTONCES POR ESO, EN ESTE SCRIPT , SE FILTRO PARA SOLO TENER LA INFORMACION DE LAS BIOPSIAS

mamas_derechas <- POSITIVOS_DESIMONE_18_22 %>%  mutate(MAMA = case_when(
  str_detect(string = Topografía, pattern = "MAMA DERECHA") ~ "MAMA DERECHA",
  str_detect(string = Topografía, pattern = "MAMA IZQUIERDA") ~ "MAMA IZQUIERDA")) %>% 
  filter(MAMA == "MAMA DERECHA")

mamas_izquierdas <- POSITIVOS_DESIMONE_18_22 %>%  mutate(MAMA = case_when(
  str_detect(string = Topografía, pattern = "MAMA DERECHA") ~ "MAMA DERECHA",
  str_detect(string = Topografía, pattern = "MAMA IZQUIERDA") ~ "MAMA IZQUIERDA")) %>% 
  filter(MAMA == "MAMA IZQUIERDA")


## BUSQUEDA DE RIÑONES

riñones_derechos <- POSITIVOS_DESIMONE_18_22 %>%  mutate(RIÑON = case_when(
  str_detect(string = Topografía, pattern = "RENAL DERECHA") ~ "RIÑON DERECHO",
  str_detect(string = Topografía, pattern = "RENAL IZQUIERDA") ~ "RIÑON IZQUIERDO")) %>% 
  filter(RIÑON == "RIÑON DERECHO")

riñones_izquierdos <- POSITIVOS_DESIMONE_18_22 %>%  mutate(RIÑON = case_when(
  str_detect(string = Topografía, pattern = "RENAL DERECHA") ~ "RIÑON DERECHO",
  str_detect(string = Topografía, pattern = "RENAL IZQUIERDA") ~ "RIÑON IZQUIERDO")) %>% 
  filter(RIÑON == "RIÑON IZQUIERDO")

riñones_izquierdos_descripcion_unica <- riñones_izquierdos %>% 
  mutate(descripcion_unica =  
           case_when(str_detect(string = Topografía,
           pattern = "PIEZA DE TUMORECTOMIA RENAL IZQUIERDA. NA SE RECIBE LESIÓN NODULAR DE 3X2.5X2.7CM")~ "SI")) %>% 
  filter(descripcion_unica == "SI") %>% 
  select(protocolo, Topografía, descripcion_unica)

write_excel_csv2(riñones_izquierdos_descripcion_unica, 
                 "riñones_izquierdos_descripcion_unica.csv")
