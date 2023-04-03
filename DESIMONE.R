library(tidyverse)
library(readxl)
library(lubridate)
library(janitor) 
 
##janitor::convert_to_date() por si quiero ir de el código para atrás a la fecha

Base <- read_excel("2018 - 2022 DESIMONE.xlsx")  


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


