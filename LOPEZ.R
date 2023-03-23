library(tidyverse)

library(lubridate)

library(readxl)



Base <- read_xlsx("Biopsias 2022 y màs Lopez codificadas.xlsx") 

Base <- Base %>%  
  rename(DIAGNOSTICO = Diagnóstico) %>% 
  mutate(`FECHA RECEP` = as_date(Fecha))

Base <- Base %>% 
  filter(`FECHA RECEP` > "2022-11-11" & `FECHA RECEP` < "2022-12-31")


Base <- Base %>% mutate (CANCER = case_when(str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
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
                                            str_detect(string = DIAGNOSTICO, pattern = "SINDROME DE S[EÉ]ZARY") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MALTOMA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MASTOCITOMA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICO") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MACROGLOBULINEMIA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MIELOESCLEROSIS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "MIELOFIBROSIS") ~ "SI",
                                            str_detect(string = DIAGNOSTICO, pattern = "TROMBOCITOPENIA") ~ "SI",
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
                          str_detect(string = `FECHA RECEP`, pattern = "^2022") ~ "2022"))


POSITIVOS <- Base %>% filter(CANCER == "SI") %>% select( Codigo, Sexo,  `FECHA RECEP`, Edad, 
                                                         Material, DIAGNOSTICO, AÑO)
BASE_CANREG<- read_csv(".csv")

DNI_y_codigo_CANREG <- read_excel("2022 - 09 - 09 codigos.xlsx") %>% 
  rename(Codigo = "Hash MD5")


DUPLICANREG_2022_resto_CODIGOS <- POSITIVOS %>%  
  semi_join(BASE_CANREG, by = "Codigo" ) %>% 
  mutate(DNI = Sexo)

TODO_MENOSDUPLICANREG <- POSITIVOS %>% 
  anti_join(DUPLICANREG_2022_CODIGOS)

DUPLICADOS_CON_DNICODIFICADOS_CANREG <- TODO_MENOSDUPLICANREG %>% 
  left_join(DNI_y_codigo_CANREG, by = "Codigo") %>% 
  filter(!is.na(DNI))


DUPLICADOS_2022_resto <- DUPLICANREG_2022_resto_CODIGOS %>% 
  bind_rows(DUPLICADOS_CON_DNICODIFICADOS_CANREG) %>%
  arrange(Codigo) %>% 
  mutate(ID = row_number())



write_excel_csv2(DUPLICADOS_2022_resto, "DUPLICADOS_LOPEZ_2022 resto.csv" )

UNICOS_2022_resto <- POSITIVOS %>%  
  anti_join(DUPLICADOS_2022, by = "Codigo" ) %>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

##### HACER DNIS 

write_excel_csv2(UNICOS_2022_resto, 
                 "UNICOS_LOPEZ_2022 resto.csv")  
