library(tidyverse)
library(readxl)

fallecidos_A <- read_csv2("fallecidos_A.csv")

fallecidos_B <- read_csv2("fallecidos_B.csv")

fallecidos_A_mas_B <- fallecidos_A %>% 
  bind_rows(fallecidos_B) 

write_excel_csv2(fallecidos_A_mas_B, "fallecidos_A_mas_B.csv")