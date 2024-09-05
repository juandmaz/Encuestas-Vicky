str(base)
map(base, table)

#tabla general
base %>%
  select(-c(id, 
            otra_forma, 
            p1_edad, 
            p9_que_celular_tiene_marca_modelo, 
            p12_como_se_conecta_a_internet_en_el_celular_datos_wifi, 
            p13_que_aplicaciones_utiliza_en_su_celular, 
            otras)) %>%
  filter(!str_detect(grupo, ',')) %>%
  mutate_all(as.factor) %>%
  tbl_summary(by= grupo,
              missing = "always",
              statistic = list(all_categorical() ~ "{n} {p}"),
              missing_text= "Casos perdidos",
              digits = list(all_categorical() ~ c(0,1))) %>%
  modify_header(label = "") %>%
  add_p() %>%
  add_overall() %>%
  gtsummary::as_tibble() %>% 
  write.csv(., na = "")

#grupo
base %>%
  count(grupo) %>%
  write.csv

#edad
summary(base$p1_edad) %>%
  #as_tibble #%>%
  write.csv

#modelo celular
base %>%
  select(p9_que_celular_tiene_marca_modelo) %>%
  pull %>%
  sort

base %>%
  select(p9_que_celular_tiene_marca_modelo) %>%
  mutate(across(everything(),
                           ~case_when(str_detect(., 'Samsung') ~ 'Samsung',
                                      str_detect(., 'Motorola') ~ 'Motorola',
                                      str_detect(., 'Moto') ~ 'Motorola',
                                      str_detect(., 'Xiaomi') ~ 'Xiaomi',
                                      TRUE ~.
                                      ))) %>%
  table(., useNA = 'always') %>%
  as_tibble %>%
  write.csv

#conectarse a wifi
base %>%
  select(p12_como_se_conecta_a_internet_en_el_celular_datos_wifi) %>% 
  pull() %>%
  str_split(., ",\\s*") %>%
  unlist %>% 
  table %>%
  as_tibble %>%
  write.csv


base %>%
  select(p12_como_se_conecta_a_internet_en_el_celular_datos_wifi) %>% 
  pull() %>%
  str_split(., ",\\s*") %>%
  sapply(., length) %>%
  table %>%
  as.tibble %>%
  write.csv


#apps mas usadas
base %>%
  select(p13_que_aplicaciones_utiliza_en_su_celular) %>% 
  pull() %>%
  str_split(., ",\\s*") %>%
  unlist %>% 
  table %>%
  as.tibble %>%
  write.csv
  
#frecuencia de cantidad de apps
base %>%
  select(p13_que_aplicaciones_utiliza_en_su_celular) %>% 
  pull() %>%
  str_split(., ",\\s*") %>%
  sapply(., length) %>%
  table %>%
  as.tibble %>%
  write.csv

#otras apps
base %>%
  select(otras) %>% 
  pull() %>%
  str_split(., ",\\s*") %>%
  unlist %>% 
  table %>%
  as.tibble %>%
  write.csv


