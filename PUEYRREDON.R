library(tidyverse)
library(readxl)
library(lubridate)

Base_1 <- read_excel("BASE PUEYRREDON NUMERO 1.xlsx") %>% 
  rename(`No Documento`= nroDocumento, 
         `FECHA RECEP` = fecha)


Base_2 <- read_excel("BASE PUEYRREDON NUMERO 2.xlsx") %>% 
   rename(`No Documento`= nroDocumento, 
         `FECHA RECEP` = fecha)


Base_PUEYRREDON <- Base_1 %>% bind_rows(Base_2) %>% 
  mutate(DIAGNOSTICO = str_to_upper(paste(diagnostico, evolucion, control, 
                                           sep = " "
                                           )))


BASE_CANREG <- read_csv("2022 - 09 - 14.csv.csv") %>% 
  unite("nombreCompleto", `Apellido Pat.`:Nombres, sep = " ", na.rm= T) %>% 
  rename(`FECHA RECEP`= `Fecha Diagnós.`)


##paste

POSITIVOS <- Base_PUEYRREDON %>% 
  mutate(CANCER = case_when(
    str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = " CA[. ] ") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "C[AÁ]NCER") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "ONCOL[OÓ]GIC") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = " QT ") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = " RT ") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "QUIMIOTERAPIA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "RADIOTERAPIA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "QMT") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "DOCETAXEL") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "CICLOFOSFAMIDA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "CARBOPLATINO") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "PACLITAXEL") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "TRASTUZUMAB") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "TRIPTORELINA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LEUPROLIDE") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "TAMOXIFENO") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "CMF") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "FULVESTRANT") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = " NV ") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "GEMCITABINE") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "ENZALUTAMIDA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "OXALIPLATINO") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "AZACITIDINA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "ZOLEDRONICO") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "PEMBROLIZUMAB") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "PEMETREXED") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "TRIFLURIDINA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "CAPECITABINE") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "METOTREXATO") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "BRENTUXIMAB") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "GAZYVA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LINFOMA") &
    str_detect(string = DIAGNOSTICO, pattern = "ADENOLINFOMA" , negate = T ) ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "CARCINOIDE") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "MELANOMA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LEUCEMIA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LLA") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "SMD") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LMMC") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LDCG") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LH") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "LNH") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "HODKIN") ~ "SI",
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
    str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICO") ~ "SI",
    str_detect(string = DIAGNOSTICO, pattern = "BL[ÁA]STICA")&
    str_detect(string = DIAGNOSTICO, pattern = "MEGALOBLASTICA" , negate = T ) ~ "SI",
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
                          str_detect(string = `FECHA RECEP`, pattern = "^2022") ~ "2022")) %>%  
  filter(CANCER == "SI") %>%  
  select(nombreCompleto ,`No Documento` , `FECHA RECEP` , DIAGNOSTICO , AÑO) %>% 
  arrange(`FECHA RECEP`) %>% 
  distinct(`No Documento`, .keep_all = T)

POSITIVOS2020_2021 <- POSITIVOS %>% filter(AÑO == c("2020", "2021"))

## sacar el ,,0 de muchos DNI!!!!!!!

DUPLICADOS_CONCANREG_2020_2021 <- POSITIVOS2020_2021 %>%  
  semi_join(BASE_CANREG, by = "No Documento" ) %>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_CONCANREG_2020_2021, "DUPLICADOS_PUEY__CONCANREG_2020_2021.csv")  
## Cargar lo que haya que cargar

POSITIVOS2020_2021_SINDUPLIDECANREG <- POSITIVOS2020_2021 %>% 
  anti_join(BASE_CANREG, by = "No Documento")

CLAVE_DUPLICADOS <- POSITIVOS2020_2021_SINDUPLIDECANREG %>% 
  count(`No Documento`, sort = T) %>% 
  filter(n > 1) 

DUPLICADOS_MISMABASE_2020_2021 <- POSITIVOS2020_2021_SINDUPLIDECANREG  %>% 
  semi_join(CLAVE_DUPLICADOS) %>%  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_MISMABASE_2020_2021, 
                 "DUPLICADOS_PUEY_MISMABASE_2020_2021.csv")

CLAVE_NODUPLICADOS <- POSITIVOS2020_2021_SINDUPLIDECANREG %>% 
  count(`No Documento`, sort = T) %>% 
  filter(n == 1)

UNICOS_2020_2021 <- POSITIVOS2020_2021_SINDUPLIDECANREG %>%  
  semi_join(CLAVE_NODUPLICADOS, by = "No Documento") %>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_2020_2021, "UNICOS_PUEY_2020_2021.csv")





