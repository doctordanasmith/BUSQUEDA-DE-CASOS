
library(tidyverse)

library(lubridate)

library(readxl)



BASE_SINPAPS <- read.csv("") 


BASE <- BASE %>% rename(`No Documento` = `numero_doc`,
                        TOPOGRAFIA = topografias ) %>% 
  mutate(fecha_nac= dmy_hm(fecha_nac), fecha_recepcion = dmy_hm(fecha_recepcion )) %>% 
  mutate(fecha_nac =  as_date(fecha_nac), fecha_recepcion = as_date(fecha_recepcion)) %>% 
  mutate(DIAGNOSTICO = str_to_upper(DIAGNOSTICO))




BASE <- BASE %>% mutate (CANCER = case_when(str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "LINFOMA") &
                                              str_detect(string = DIAGNOSTICO, pattern = "ADENOLINFOMA" , negate = T )
                                            ~ "SI",
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
                                            str_detect(string = DIAGNOSTICO, pattern = "MAT[AÁ]STASIS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "TUMOR MALIGNO") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "NEOPLASIA MALIGNA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "NEOPL[AÁ]SICO MALIGNO") ~ "SI",
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
                                            str_detect(string = DIAGNOSTICO, pattern = "WILMS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "HODGKIN") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MICOSIS FUNGOIDE") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "SINDROME DE S[ÉE]ZARY") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MALTOMA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MASTOCITOMA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICO") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MACROGLOBULINEMIA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MIELOESCLEROSIS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MIELOFIBROSIS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MIELODISPLASIA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MIELODISPL[ÁA]SICO") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "INFILTRACI[ÓO]N") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "C[ÉE]LULAS CLARAS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "BOWEN ") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI"
                                            str_detect(string = DIAGNOSTICO, pattern = "TROMBOCITEMIA ESENCIAL") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "BOWEN ") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "S[ÉE]RTOLI") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "LEYDIG") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOMA FIBROSO MALIGNO") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOSIS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "SENO ENDOD[EÉ]RMICO") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "TUMOR RABDOIDE") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI")) %>% 
  mutate (AÑO = case_when(str_detect(string = fecha_recepcion, pattern = "^2018") ~ "2018", 
                          str_detect(string = fecha_recepcion, pattern = "^2019") ~ "2019",
                          str_detect(string = fecha_recepcion, pattern = "^2020") ~ "2020"))


POSITIVOS <- BASE_SINPAPS %>% filter(CANCER == "SI") %>% 
  select(nombre , `No Documento` , fecha_nac, fecha_recepcion, edad ,TOPOGRAFIA ,
         datos_clinicos, DIAGNOSTICO, obtencion, domicilio, AÑO) %>% 
  arrange(`No Documento`)