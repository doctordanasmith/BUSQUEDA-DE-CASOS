library(tidyverse)
library(readxl)
library(lubridate)

base_completa <- read_csv("2022 - 11 - 08.csv") %>% 
  filter(`Estado caso` == "1") %>%   
  mutate(`Fecha Diagnós.` = ymd(`Fecha Diagnós.`), 
         `Fecha Diagnós.` = as_date(`Fecha Diagnós.`) )

x <- c("9", "99", "999", "9999", "99999", "999999", "9999999", "99999999", "999999999")

Pedidos <- base_completa %>% 
  filter(!`No Documento`  %in% x) %>% 
  filter(`Fecha Diagnós.` >= "2018-01-01") %>% 
  select(- `Apellido Mat.`) %>% 
  unite("nombreCompleto", `Apellido Pat.`:Nombres, sep = " ") %>% 
  distinct(`No Documento`, .keep_all = T) %>% 
  arrange(`Fecha Notification`) 

Pedido_2 <- Pedidos %>% 
  filter(`Fecha Notification` >= "2022-01-01") %>% 
  mutate(ID = row_number()) %>% 
  select(`No Documento`, nombreCompleto ,`Fecha Nacimiento`, `Sexo (desc)`, ID)

write_csv(Pedido_2, "2019 2020 DR DANA SMITH.txt") 
