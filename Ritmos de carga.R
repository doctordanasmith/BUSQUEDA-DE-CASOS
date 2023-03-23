library(tidyverse)
library(lubridate)


base <- read_csv("2022 - 06 - 21.csv")%>% 
  mutate(`Fecha Notification` = ymd(`Fecha Notification`)) %>% 
  mutate(`Fecha Notification` = as_date(`Fecha Notification`))

base %>% filter(`Fecha Notification` >= "2022/06/01" & `Fecha Notification` <= 
                  "2022/06/03") %>%
  group_by(Registrador) %>% count() %>%  view()

base %>%  group_by(`Fecha Notification`, Registrador) %>% 
  filter(`Fecha Notification` >= "2022/05/01" & `Fecha Notification` <= 
                   "2022/05/21") %>%
  count() %>%  view()

carga_Gaby <- base %>%  
  filter(Registrador == "G", `Fecha Notification` >= "2022/05/01" & 
           `Fecha Notification` <=  "2022/05/21") %>% 
  group_by(`Fecha Notification`) %>% 
  count() %>% 
  rename(casos_cargados = n) 



carga_Rodri <- base %>%  
  filter(Registrador == "Z", `Fecha Notification` >= "2022/05/01" & 
           `Fecha Notification` <=  "2022/05/21") %>% 
  group_by(`Fecha Notification`) %>% 
  count() %>% 
  rename(casos_cargados = n) 




  
  
 ## summarise(promedio = mean(, na.rm = T))


            


 




