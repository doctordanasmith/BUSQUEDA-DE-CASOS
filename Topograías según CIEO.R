library(tidyverse)

library(lubridate)


Base <- read_csv("2023 - 07-04.csv")

Base  <- Base %>% mutate (SITIO_DE_PRIMARIO = case_when(str_detect(string = Topografía, pattern = "^0") ~ "Labio, cavidad oral, glándulas salivales y faringe",
                                          str_detect(string = Topografía, pattern = "^11") ~ "Labio, cavidad oral, glándulas salivales y faringe",
                                          str_detect(string = Topografía, pattern = "^12") ~ "Labio, cavidad oral, glándulas salivales y faringe",
                                          str_detect(string = Topografía, pattern = "^13") ~ "Labio, cavidad oral, glándulas salivales y faringe",
                                          str_detect(string = Topografía, pattern = "^14") ~ "Labio, cavidad oral, glándulas salivales y faringe",
                                          str_detect(string = Topografía, pattern = "^15") ~ "Esófago",
                                          str_detect(string = Topografía, pattern = "^16") ~ "Estómago",
                                          str_detect(string = Topografía, pattern = "^17") ~ "Intestino delgado",
                                          str_detect(string = Topografía, pattern = "^18") ~ "Colorrectal",
                                          str_detect(string = Topografía, pattern = "^19") ~ "Colorrectal",
                                          str_detect(string = Topografía, pattern = "^20") ~ "Colorrectal",
                                          str_detect(string = Topografía, pattern = "^21") ~ "Ano y conducto anal",
                                          str_detect(string = Topografía, pattern = "^22") ~ "Hígado y vías biliares intrahepáticas",
                                          str_detect(string = Topografía, pattern = "^23") ~ "Vesícula biliar",
                                          str_detect(string = Topografía, pattern = "^24") ~ "Otras partes y las no especificadas de las vías biliares",
                                          str_detect(string = Topografía, pattern = "^25") ~ "Páncreas",
                                          str_detect(string = Topografía, pattern = "^26") ~ "Otras localizaciones y las mal definidas del aparato digestivo",
                                          str_detect(string = Topografía, pattern = "^300") ~ "Cavidad nasal",
                                          str_detect(string = Topografía, pattern = "^301") ~ "Oído medio",
                                          str_detect(string = Topografía, pattern = "^31") ~ "Senos paranasales",
                                          str_detect(string = Topografía, pattern = "^32") ~ "Laringe",
                                          str_detect(string = Topografía, pattern = "^33") ~ "Tráquea",
                                          str_detect(string = Topografía, pattern = "^34") ~ "Bronquios y Pulmón",
                                          str_detect(string = Topografía, pattern = "^37") ~ "Timo",
                                          str_detect(string = Topografía, pattern = "^38") ~ "Corazón, mediastino y pleura",
                                          str_detect(string = Topografía, pattern = "^39") ~ "Otros sitios y los mal definidos del sistema respiratrio y los órganos intratorácicos",
                                          str_detect(string = Topografía, pattern = "^40") ~ "Huesos, articulaciones y cartílago articular",
                                          str_detect(string = Topografía, pattern = "^41") ~ "Huesos, articulaciones y cartílago articular",
                                          str_detect(string = Topografía, pattern = "^42") ~ "Sistemas hematopoyético y reticuloendotelial",
                                          str_detect(string = Topografía, pattern = "^44") != str_detect(string = `Morfología (desc)`, pattern = "melanoma") ~ "Piel no melanoma",
                                          str_detect(string = `Morfología (desc)`, pattern = "melanoma") ~ "Melanoma",
                                          str_detect(string = Topografía, pattern = "^47") ~ "Nervios periféricos y sistema nervioso autónomo",
                                          str_detect(string = Topografía, pattern = "^48") ~ "Peritoneo y retroperitoneo",
                                          str_detect(string = Topografía, pattern = "^49") ~ "Tejido conjuntivo, subcutáneo y de otros tejidos blandos",
                                          str_detect(string = Topografía, pattern = "^50") ~ "Mama",
                                          str_detect(string = Topografía, pattern = "^51") ~ "Vulva",
                                          str_detect(string = Topografía, pattern = "^52") ~ "Vagina",
                                          str_detect(string = Topografía, pattern = "^53") ~ "Cuello uterino",
                                          str_detect(string = Topografía, pattern = "^54") ~ "Utero",
                                          str_detect(string = Topografía, pattern = "^55") ~ "Utero",
                                          str_detect(string = Topografía, pattern = "^56") ~ "Ovario",
                                          str_detect(string = Topografía, pattern = "^57") ~ "Otros órganos genitales femeninos y los no especificados",
                                          str_detect(string = Topografía, pattern = "^58") ~ "Placenta",
                                          str_detect(string = Topografía, pattern = "^60") ~ "Pene",
                                          str_detect(string = Topografía, pattern = "^61") ~ "Próstata",
                                          str_detect(string = Topografía, pattern = "^62") ~ "Testículos",
                                          str_detect(string = Topografía, pattern = "^63") ~ "Otros órganos genitales masculinos y los no especificados",
                                          str_detect(string = Topografía, pattern = "^64") ~ "Riñón",
                                          str_detect(string = Topografía, pattern = "^65") ~ "Vejiga, uréteres o pelvis renal",
                                          str_detect(string = Topografía, pattern = "^66") ~ "Vejiga, uréteres o pelvis renal",
                                          str_detect(string = Topografía, pattern = "^67") ~ "Vejiga, uréteres o pelvis renal",
                                          str_detect(string = Topografía, pattern = "^68") ~ "Otros órganos urinarios y los no especificados",
                                          str_detect(string = Topografía, pattern = "^69") ~ "Encéfalo, ojo y otras partes del sistema nervioso central",
                                          str_detect(string = Topografía, pattern = "^70") ~ "Encéfalo, ojo y otras partes del sistema nervioso central",
                                          str_detect(string = Topografía, pattern = "^71") ~ "Encéfalo, ojo y otras partes del sistema nervioso central",
                                          str_detect(string = Topografía, pattern = "^72") ~ "Encéfalo, ojo y otras partes del sistema nervioso central",
                                          str_detect(string = Topografía, pattern = "^73") ~ "Tiroides",
                                          str_detect(string = Topografía, pattern = "^74") ~ "Suprarrenales",
                                          str_detect(string = Topografía, pattern = "^75") ~ "Otras glándulas endocrinas y estructuras afines",
                                          str_detect(string = Topografía, pattern = "^76") ~ "Sitios mal definidos de las distintas localizaciones",
                                          str_detect(string = Topografía, pattern = "^77") ~ "Ganglios linfáticos",
                                          str_detect(string = Topografía, pattern = "^80") ~ "Sitio primario desconocido"))
                                          
                                          
diezprimeros <- Base %>%  count(SITIO_DE_PRIMARIO, sort = T) %>% 
  slice(1:10) %>% pull(SITIO_DE_PRIMARIO)

Base %>% select(SITIO_DE_PRIMARIO) %>% 
  filter(SITIO_DE_PRIMARIO %in% diezprimeros) %>% 
  ggplot(aes(x =(fct_infreq(SITIO_DE_PRIMARIO)), fill = fct_infreq(SITIO_DE_PRIMARIO))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.position = "none") + 
  xlab("Sitio Primario")
                
                                          
                                          
                                          
                                          
                                          
                                          