library(tidyverse)

library(lubridate)

library(readxl)
 

HOUSSAY_INGRESOS_2022 <- read_excel("Ingresos HOUSSAY 2022.xlsx", sheet = 1) %>% 
  select(DOCUMENTO, F_NACIMIENTO, EDAD_Años_, 
         GÉNERO, DIRECCIÓN, F__INICIO,
        VÍA_DE_INGRESO,  DIAGNÓSTICO, FECHA_ACTIVACIÓN, TELÉFONO)

HOUSSAY_Hospital_de_día_2022 <- read_excel("Ingresos HOUSSAY 2022.xlsx", sheet = 2) %>% 
  select(DOCUMENTO, F_NACIMIENTO, EDAD_Años_, 
         GÉNERO, DIRECCIÓN,  F__INICIO,
         VÍA_DE_INGRESO,  DIAGNÓSTICO, FECHA_ACTIVACIÓN, TELÉFONO)

Base <- HOUSSAY_INGRESOS_2022 %>% 
  bind_rows(HOUSSAY_Hospital_de_día_2022)


Base <- Base %>% 
  mutate(`FECHA RECEP` = ymd_hms(F__INICIO), 
         `FECHA RECEP` = as_date(`FECHA RECEP`), 
         DIAGNOSTICO = str_to_upper(DIAGNÓSTICO), 
         NODOC = as.character(DOCUMENTO),
         `FECHA NAC` = ymd(F_NACIMIENTO),`FECHA NAC` = as_date(`FECHA NAC`)) %>% 
         rename(ver_si_emp_con_223 = TELÉFONO,
                LOCALIDAD = FECHA_ACTIVACIÓN,
                EDAD = EDAD_Años_) %>% 
  select(NODOC, `FECHA NAC`, EDAD, GÉNERO,`FECHA RECEP`, DIAGNOSTICO,
        VÍA_DE_INGRESO, DIRECCIÓN, LOCALIDAD, ver_si_emp_con_223)


Base <- Base %>%
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
                            str_detect(string = DIAGNOSTICO, pattern = "MAT[AÁ]STASIS") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "TUMOR MALIGNO") & 
                              str_detect(string = DIAGNOSTICO, pattern = "HISTORIA|CIRUGIA PROFILACTICA", negate = T   ) ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "NEOPLASIA MALIGNA") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "NEOPL[AÁ]SICO MALIGNO") ~ "SI",
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
                            str_detect(string = DIAGNOSTICO, pattern = "SINDROME DE S[ÉE]ZARY") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "MALTOMA") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "MASTOCITOMA") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICO") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICA") &
                              str_detect(string = DIAGNOSTICO, pattern = "MEGALOBLASTICA", negate = T) ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "MACROGLOBULINEMIA") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA") &
                              str_detect(string = DIAGNOSTICO, pattern = "POLICITEMIA SECUNDARIA", negate = T  ) ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "MIELOESCLEROSIS") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "MIELOFIBROSIS") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "MIELODISPL[ÁA]SICO") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "MIELODISPLASIA") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "INFILTRACI[ÓO]N") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "C[ÉE]LULAS CLARAS") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "BOWEN ") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "TROMBOCITEMIA ESENCIAL") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "S[ÉE]RTOLI") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "LEYDIG") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOMA FIBROSO MALIGNO") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "HISTIOCITOSIS") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "SENO ENDOD[EÉ]RMICO") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "TUMOR RABDOIDE") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "ANEMIA REFRACTARIA") ~ "SI",
                            str_detect(string = DIAGNOSTICO, pattern = "TROMBOCITEMIA") ~ "SI"
                            )) %>% 
  mutate (AÑO = case_when(str_detect(string = `FECHA RECEP`, pattern = "^2018") ~ "2018", 
                          str_detect(string = `FECHA RECEP`, pattern = "^2019") ~ "2019",
                          str_detect(string = `FECHA RECEP`, pattern = "^2020") ~ "2020",
                          str_detect(string = `FECHA RECEP`, pattern = "^2021") ~ "2021",
                          str_detect(string = `FECHA RECEP`, pattern = "^2022") ~ "2022")) 


POSITIVOS_HOUSSAY_2022 <- Base %>% 
  filter(CANCER == "SI")%>% 
  arrange(NODOC) %>% 
  distinct(NODOC, .keep_all = TRUE)

write_excel_csv2(POSITIVOS_HOUSSAY_2022,  "POSITIVOS_HOUSSAY_2022.csv")


POSITIVOS_HOUSSAY_2022 <- read_excel("POSITIVOS_HOUSSAY_2022.xlsx") %>% 
  mutate(DNI = as.character(NODOC))

BASE_CANREG<- read_csv("2023 - 05 - 09.csv")

DNI_y_codigo_CANREG <- read_excel("2022 - 09 - 09 codigos.xlsx") %>% 
  rename(Codigo = "Hash MD5")


DUPLICADOS_CODIGOS <- POSITIVOS_HOUSSAY_2022 %>%  
  semi_join(BASE_CANREG, by = "Codigo" ) %>% 
  select(- DNI, - VÍA_DE_INGRESO)%>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_CODIGOS, "DUPLICADOS_CODIGOS.csv")

DUPLICADOS_DNI <- POSITIVOS_HOUSSAY_2022 %>% 
  semi_join(DNI_y_codigo_CANREG, by = "DNI")%>% 
  select(- DNI, - VÍA_DE_INGRESO) %>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_DNI, "DUPLICADOS_DNI.csv")

DUPLICADOS_2022 <- DUPLICADOS_CODIGOS %>% 
  bind_rows(DUPLICADOS_DNI) 

UNICOS_2022 <- POSITIVOS_HOUSSAY_2022 %>%  
  anti_join(DUPLICADOS_2022, by = "Codigo" )%>% 
  select(- DNI, - VÍA_DE_INGRESO) %>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())


CLAVE_DUPLICADOS <-  UNICOS_2022  %>% 
  count(Codigo, sort = T) %>% 
  filter(n > 1) 

UNICOS_duplicadosdentrodemismabase <-  UNICOS_2022 %>% 
  semi_join(CLAVE_DUPLICADOS)%>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_duplicadosdentrodemismabase, 
                 "UNICOS_H2022_duplicadosdentrodemismabase.csv")

CLAVE_NODUPLICADOS <-  UNICOS_2022 %>% 
  count(Codigo, sort = T) %>% 
  filter(n == 1)  

UNICOS_solounavezenlabase <-  UNICOS_2022  %>% 
  semi_join(CLAVE_NODUPLICADOS)%>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_solounavezenlabase, 
                 "UNICOSH2022_solounavezenlabase.csv")  


