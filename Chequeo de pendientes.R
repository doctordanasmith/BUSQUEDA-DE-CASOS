library(tidyverse)
library(lubridate)


base <- read_csv("2022 - 11 - 08.csv") %>% 
  mutate(`Fecha Notification` = ymd(`Fecha Notification`)) %>% 
  mutate(`Fecha Notification` = as_date(`Fecha Notification`))

base <-  base %>% 
  mutate (AÑO = case_when(str_detect(string = `Fecha Diagnós.`, pattern = "^2013") ~ "2013",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2014") ~ "2014",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2015") ~ "2015",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2016") ~ "2016",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2017") ~ "2017",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2018") ~ "2018",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2019") ~ "2019",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2020") ~ "2020",
                          str_detect(string = `Fecha Diagnós.`, pattern = "^2021") ~ "2021")) 

  
  

base %>% 
  filter(`Fecha Notification` >= "2022/05/23" & `Fecha Notification` <= 
           "2022/06/03", `Estado caso` == "0") %>%
  select(`Estado caso`, Registrador, 
         `No Documento`, `Apellido Pat.`,
         `Institución 1 (desc)`, `Topografía (desc)`,
         `Morfología (desc)`, Comportamiento,
         `Base Diagnós.`, `Fecha Notification`) %>% 
  arrange(`Fecha Notification`) %>% 
  view()

DNI_para_codigos <- base %>% select(`No Documento`)

write_excel_csv2(DNI_para_codigos, "DNI_para_codigos.csv")

