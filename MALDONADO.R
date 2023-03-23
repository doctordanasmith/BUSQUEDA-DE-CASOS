library(tidyverse)
library(lubridate)
library(readxl)

Base_2021 <- read_excel("Biopsias 2021.xlsx")

Base_2022 <- read_excel("Biopsias 2022.xlsx")

Base <- Base_2021 %>% 
  bind_rows(Base_2022)%>% rename(DIAGNOSTICO = Diagnóstico)

Base <- Base %>%  mutate(INSTITUCION = case_when(C_Fact == "150201" ~ "OSUTHGRA",
                                                 C_Fact == "NyMpresCL" ~ "CLINICA DEL NIÑO Y LA MADRE",
                                                 C_Fact == "NyMpresCM" ~ "CLINICA DEL NIÑO Y LA MADRE",
                                                 C_Fact == "NyMdebeO"  ~ "CLINICA DEL NIÑO Y LA MADRE",
                                                 C_Fact == "NyMtrajoO" ~ "CLINICA DEL NIÑO Y LA MADRE",
                                                 C_Fact == "IOMA X CL."~ "CLINICA DEL NIÑO Y LA MADRE",
                                                 C_Fact == "SB"        ~ "SANATORIO BELGRANO",
                                                 C_Fact == "SBpresCM"  ~ "SANATORIO BELGRANO",
                                                 C_Fact == "SANpresCL" ~ "SANATORIO BELGRANO",
                                                 C_Fact == "SANpresCM" ~ "SANATORIO BELGRANO",
                                                 C_Fact == "SANdebeO"  ~ "SANATORIO BELGRANO",
                                                 C_Fact == "ASBdebe"   ~ "SANATORIO BELGRANO",
                                                 C_Fact == "ASBpresCL" ~ "SANATORIO BELGRANO",
                                                 C_Fact == "ASBtrajo"  ~ "SANATORIO BELGRANO",
                                                 C_Fact == "X PLANILLA"~ "SANATORIO BELGRANO",
                                                 C_Fact == "PAGADO"    ~ "SEGUN EL MEDICO",
                                                 C_Fact == "PRESENT CL"~ "SEGUN EL MEDICO",
                                                 C_Fact == "PRESENT CM" ~ "SEGUN EL MEDICO",
                                                 C_Fact == "TRAJO ORDEN" ~ "SEGUN EL MEDICO(UNIDAD OBSTETRICA?)",
                                                 C_Fact == "INCOBRABLE" ~ "SEGUN EL MEDICO",
                                                 C_Fact == "FICHA"     ~ "SEGUN EL MEDICO",
                                                 C_Fact == "DEBE ORDEN" ~ "SEGUN EL MEDICO",
                                                 C_Fact == "DEBE ESTUDIO" ~ "SEGUN EL MEDICO",
                                                 C_Fact == "SIN CARGO" ~ "SEGUN EL MEDICO",
                                                 C_Fact == "15.02.01" ~ "SEGUN EL MEDICO",
                                                 C_Fact == "MAR DE AJO"~ "ALFREDO CL. MAR DE AJO",
                                                 C_Fact == "150201 OSPAT" ~ "OSPAT"),
                         INSTITUCION = replace_na(INSTITUCION, "sin dato"))




Base <- Base %>% mutate(CANCER = case_when(
  str_detect(string = DIAGNOSTICO, pattern = "CARCINOMA") ~ "SI",
  str_detect(string = DIAGNOSTICO, pattern = " CA[. ] ") ~ "SI",
  str_detect(string = DIAGNOSTICO, pattern = "C[AÁ]NCER") ~ "SI",
  str_detect(string = DIAGNOSTICO, pattern = "ONCOL[OÓ]GIC") ~ "SI",
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
  mutate (AÑO = case_when(str_detect(string = Protocolo, pattern = "21$") ~ "2021", 
                          str_detect(string = Protocolo, pattern = "22$") ~ "2022",
                          str_detect(string = Protocolo, pattern = "23$") ~ "2023",
                          str_detect(string = Protocolo, pattern = "24$") ~ "2024"))


POSITIVOS_21_22 <- Base %>% filter(CANCER == "SI") %>% 
  select(HC, Paciente, Fecha, Edad, Procedencia, Material, DIAGNOSTICO, Protocolo, 
         Médico, C_Fact, AÑO, INSTITUCION)

write_excel_csv2(POSITIVOS_21_22, "POSITIVOS_21_22.csv")
