library(tidyverse)

library(lubridate)

library(readxl)

HEMATO_HIGA_2021_2022parte <- read_xlsx("2021 2022 HIGA HEMATO.xlsx") ## previo hacer un excel con el hash con dni y códigos

BASECANREG <- read_csv("2022 - 11 - 08.csv") %>% 
  mutate(`No Documento` = as.character(`No Documento`))

BASE_LARIOJA_2021 <- read_csv2("UNICOS_2021_LARIOJA.csv")

BASE_LARIOJA_2022 <- read_csv2("UNICOS_2022_LARIOJA.csv")

BASE_COLON_2021 <- read_csv2("UNICOS_2021.csv")

BASE_COLON_2022 <- read_csv2("UNICOS_2022.csv")

BASE_BUFFELLI <- read_csv2("UNICOS_BUFFELLI_RADIOLOGICO_H2021.csv")


DUPLICADOS_POMARCANREG_2021_porDNI <- HEMATO_HIGA_2021_2022parte %>%  
  semi_join(BASECANREG, by= "No Documento" ) 

DUPLICADOS_POMARCANREG_2021_porcodigo <- HEMATO_HIGA_2021_2022parte %>%  
  semi_join(BASECANREG, by = "Codigo") 

DUPLICADOS_LARIOJA_2021 <- HEMATO_HIGA_2021_2022parte %>%
  semi_join(BASE_LARIOJA_2021, by = "Codigo")

DUPLICADOS_LARIOJA_2022 <- HEMATO_HIGA_2021_2022parte %>%
  semi_join(BASE_LARIOJA_2022, by = "Codigo")

DUPLICADOS_COLON_2021 <- HEMATO_HIGA_2021_2022parte %>% 
  semi_join(BASE_COLON_2021, by = "Codigo")

DUPLICADOS_COLON_2022 <- HEMATO_HIGA_2021_2022parte %>% 
  semi_join(BASE_COLON_2022, by = "Codigo")

DUPLICADOS_BUFFELLI_2022 <- HEMATO_HIGA_2021_2022parte %>% 
  semi_join(BASE_BUFFELLI, by = "Codigo")

DUPLICADOS_2022 <- DUPLICADOS_POMARCANREG_2021_porDNI %>% 
  bind_rows(DUPLICADOS_POMARCANREG_2021_porcodigo, DUPLICADOS_LARIOJA_2021, 
            DUPLICADOS_LARIOJA_2022, DUPLICADOS_COLON_2021, DUPLICADOS_COLON_2022) %>%
  arrange(`No Documento`) %>% 
  mutate(ID = row_number()) 


write_excel_csv2(DUPLICADOS_2022, "DUPLICADOS_HEMATO_2021_2022parte.csv")  


UNICOS <- HEMATO_HIGA_2021_2022parte %>% 
  anti_join(DUPLICADOS_2022, by = "No Documento") %>% 
  arrange(Codigo) %>% 
  mutate(ID = row_number()) %>% 
  select(`No Documento`, Codigo, ID)

write_excel_csv2(UNICOS, "UNICOS_HEMATO_HIGA_2021_2022parte.csv")
