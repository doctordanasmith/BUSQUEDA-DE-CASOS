library(tidyverse)

library(lubridate)

library(readxl)

library(scales)

library(ggrepel)


BORDENAVE_1 <- read.csv2("BORDENAVE_2021.csv")


BORDENAVE_2 <- read.csv2("BORDENAVE_2022.csv")


BORDENAVE_2021_2022 <- BORDENAVE_1 %>% 
  bind_rows(BORDENAVE_2)

BORDENAVE_2021_2022 <- BORDENAVE_2021_2022 %>% 
  mutate(`FECHA RECEP`= fecha, `FECHA RECEP`= dmy(`FECHA RECEP`),
         `FECHA RECEP` = as_date(`FECHA RECEP`)) %>% 
  rename(TOPOGRAFIA= material,  EDAD = edad, DIAGNOSTICO = diagnostico) %>% 
  mutate(TOPOGRAFIA = str_to_upper(TOPOGRAFIA), DIAGNOSTICO = 
           str_to_upper(DIAGNOSTICO), CANCER = 
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

POSITIVOS <- BORDENAVE_2021_2022  %>% filter(AÑO %in% c("2018", "2019", "2020", "2021", "2022", "2023"),
                                       CANCER == "SI")

DNIs_CONSEGUIDOS <- read_excel("DNIs CED obtenidos el  23 06 2023.xlsx")
  

POSITIVOS <- POSITIVOS %>% 
  left_join(DNIs_CONSEGUIDOS, by = "protocolo")%>% 
  select(DNI,  Codigo, paciente, `FECHA RECEP`, EDAD, TOPOGRAFIA, DIAGNOSTICO) 

BASE_CANREG<- read_csv(".csv")

DNI_y_codigo_CANREG <- read_excel("2022 - 09 - 09 codigos.xlsx") %>% 
  rename(Codigo = "Hash MD5")

DUPLICADOS_CODIGOS <- POSITIVOS %>%  
  semi_join(BASE_CANREG, by = "Codigo" ) %>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_CODIGOS, "DUPLICADOS_CODIGOS BORDENAVE 2021 2022.csv")

DUPLICADOS_DNI <- POSITIVOS %>% 
  semi_join(DNI_y_codigo_CANREG, by = "DNI")%>% 
  arrange(DNI) %>% 
  mutate(ID = row_number())

write_excel_csv2(DUPLICADOS_DNI, "DUPLICADOS_DNI BORDENAVE 2021 2022.csv")

DUPLICADOS_2022 <- DUPLICADOS_CODIGOS %>% 
  bind_rows(DUPLICADOS_DNI) 

UNICOS_2022 <- POSITIVOS %>%  
  anti_join(DUPLICADOS_2022, by = "Codigo" )

CLAVE_DUPLICADOS <-  UNICOS_2022  %>% 
  count(Codigo, sort = T) %>% 
  filter(n > 1) 

UNICOS_duplicadosdentrodemismabase <-  UNICOS_2022 %>% 
  semi_join(CLAVE_DUPLICADOS)%>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_duplicadosdentrodemismabase, 
                 "UNICOS_duplicadosdentrodemismabase BORDENAVE 2021 2022.csv")

CLAVE_NODUPLICADOS <-  UNICOS_2022 %>% 
  count(Codigo, sort = T) %>% 
  filter(n == 1)  

UNICOS_solounavezenlabase <-  UNICOS_2022  %>% 
  semi_join(CLAVE_NODUPLICADOS)%>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number())

write_excel_csv2(UNICOS_solounavezenlabase, 
                 "UNICOS_solounavezenlabase BORDENAVE 2021 2022.csv")  









## SI NECESITO DNI
PEDIDO_DNI <- POSITIVOS %>% 
  distinct(paciente, .keep_all = T) %>% 
  select(protocolo_patologo, paciente, EDAD) %>% 
  arrange(paciente)


write_excel_csv2(PEDIDO_DNI, "PEDIDO DE DNI PACIENTES DEL CED PATOLOGO DR BORDENAVE.csv")



