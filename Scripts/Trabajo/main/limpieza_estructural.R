#base cruda
base<- read_excel(Sys.glob("Data/Bases/Cruda/*.xlsx"), 
                        guess_max = 100000,
                        na = c("NA"))

#base canonica
base<- read_excel(Sys.glob("Data/Bases/Canonica/*.xlsx"), 
                  guess_max = 115000)


#modificar errores de data entry
base<-
base %>%
  mutate(`P12. ¿Como se conecta a Internet en el celular? (datos, wifi)` = case_when(
    str_detect(`P12. ¿Como se conecta a Internet en el celular? (datos, wifi)`, "Wifi$") ~ str_replace_all(`P12. ¿Como se conecta a Internet en el celular? (datos, wifi)`, "Wifi$", "Wifi en casa"),
    str_detect(`P12. ¿Como se conecta a Internet en el celular? (datos, wifi)`, "Wifi,") ~ str_replace_all(`P12. ¿Como se conecta a Internet en el celular? (datos, wifi)`, "Wifi,", "Wifi en casa,"),
    TRUE ~ `P12. ¿Como se conecta a Internet en el celular? (datos, wifi)`)) 

#modificar nombres
base<-
  clean_names(base)

#crear variable id
base<-
base %>%
  mutate(id= 1:nrow(.)) %>%
  select(id, everything())

#exportar---

#write_xlsx(base, "Data/Bases/Canonica/pv_base_2024_09_02.xlsx")
dim(base)


