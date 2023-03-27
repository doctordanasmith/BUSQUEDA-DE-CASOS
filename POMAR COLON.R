

library(tidyverse)

library(lubridate)

library(readxl)



BASE <- read_excel("informes pomar resto 2022.xlsx", sheet = 2) 


glimpse(BASE)


BASE <- BASE %>% rename(`No Documento` = numero_doc,
                        DIAGNOSTICO = diagnostico, LOCALIDAD = localidad) %>% 
  mutate(`FECHA RECEP` = ymd_hms(fecha_recepcion)) 

BASE <- BASE %>%   mutate(`FECHA RECEP` = as_date(`FECHA RECEP`))  



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


x <- c("MAR DEL PLATA", "BATAN", "LAGUNA DE LOS PADRES", "SAN FRANCISCO", 
       "LOS ORTIZ", "LOMA ALTA", "CHAPADMALAL",
       "EL TEJADO", "SANTA ISABEL", "ESTACION CAMET", "ESTACION CHAPADMALAL", 
       "EL BOQUERON", "COLONIA BARRAGAN", "VALLE HERMOSO", "EL COYUNCO",
       "GLORIA DE LA PEREGRINA", "EL DORADO", "LAS MARGARITAS", "2 DE ABRIL", 
       "LA ADELA", "EL SOSIEGO", "LAS QUINTAS", "LOS ACANTILADOS", "SAN EDUARDO",
       "MAR DEL PLATA (ESTACION CHAPADMALAL)", "CAMET NORTE", "SIERRA DE LOS PADRES",
       "MAR DE PLATA", "S. DE LOS PADRES", "EST. CHAPADMALAL", "CHAPADMAL", "MR DEL PLATA",
       "ESTAFETA CHAPADMALAL", "ACANTILADOS", "MAR DEL PALTA", "PLAYA CHAPADMALAL",
       "MAR DELPLATA", "NAR DEL PLATA", "CHAPAMALAL", "MAR DEL PLATA (ESTACION CHAPADMALAL)",
       "COLONIA CHAPADMALAL")


POSITIVOS <- BASE %>% filter(CANCER == "SI") %>% 
  select(`No Documento`, fecha_nac, `FECHA RECEP`, edad, sexo, topografias,
         DIAGNOSTICO, datos_clinicos,domicilio, LOCALIDAD,  AÑO, numero) %>% 
  filter(LOCALIDAD %in% x)


DNI_codificados_Canreg <- read_excel("2022 - 09 - 16 DNI codificados.xlsx") %>% 
  rename(Codigo = `Hash MD5`)


BASE_CANREG <- read_csv("2023 - 03 - 23.csv") 


write_excel_csv2(POSITIVOS, "POSITIVOS_colon_resto2022.csv")

## agrego códigos

POSITIVOS_con_codigos <- read_excel("POSITIVOS_colon_resto2022.xlsx") %>% 
  mutate(`No Documento` = as.character(`No Documento`))

DUPLICADOS_colon_resto2022_porDNI <- POSITIVOS_con_codigos %>%  
  semi_join(BASE_CANREG, by= "No Documento" ) 

DUPLICADOS_colon_resto2022_porcodigo <- POSITIVOS_con_codigos %>%  
  semi_join(DNI_codificados_Canreg, by= "Codigo") 

DUPLICADOS_colon_resto2022_porcodigo_2 <- POSITIVOS_con_codigos %>%  
  semi_join(BASE_CANREG, by= "Codigo") 

write_excel_csv2(DUPLICADOS_colon_resto2022_porcodigo_2, 
                 "DUPLICADOS_colon_resto2022_porcodigo_2.csv")

DUPLICADOS <- DUPLICADOS_colon_resto2022_porDNI %>% 
  bind_rows(DUPLICADOS_colon_resto2022_porcodigo,
            DUPLICADOS_colon_resto2022_porcodigo_2) %>% 
  arrange(`No Documento`) %>% 
  mutate(ID = row_number())


write_excel_csv2(DUPLICADOS, "DUPLICADOS_colon_resto2022.csv")  


UNICOS_colon_resto2022 <- POSITIVOS_con_codigos %>%  
  anti_join(DUPLICADOS, by= "No Documento") %>% 
  anti_join(DUPLICADOS, by= "Codigo") %>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number()) %>% 
  select(Codigo,fecha_nac, `FECHA RECEP`, edad,sexo, topografias, DIAGNOSTICO, datos_clinicos,
         domicilio, LOCALIDAD,  AÑO, `No Documento`,
         ID)

write_excel_csv2(UNICOS_colon_resto2022, "UNICOS_colon_resto2022.csv")


