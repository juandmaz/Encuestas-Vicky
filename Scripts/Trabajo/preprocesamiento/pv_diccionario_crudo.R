#base cruda
base_cruda<- read_excel(Sys.glob("Data/Bases/Cruda/*.xlsx"), 
                  guess_max = 100000,
                  na = c("NA"))


#diccionario----

diccionario<- 
tibble(posicion= 1:length(names(base_cruda)),
       variable= names(base_cruda),
       nombre_nuevo= names(clean_names(base_cruda)),
       explicacion_campo= NA,
       clase= map_chr(base_cruda, ~class(.)[1]),
       valores_unicos2= map_dbl(base_cruda, ~length(unique(.))),
       porcentaje_valores_unicos2= map_dbl(base_cruda, ~round(length(unique(.))/length(.),2)),
       cantidad_NA2= map_int(base_cruda,~sum(is.na(.))),
       porcentaje_NA2= map_dbl(base_cruda, ~round(sum(is.na(.))/length(.),2)),
       primeros_valores= paste(map(base_cruda, ~names(rev(sort(table(.)))[1])), 
                               map(base_cruda, ~names(rev(sort(table(.)))[2])), 
                               sep=";")) #%>% 
  view()




#exportar diccionario----
diccionario %>% 
  write_xlsx("Data/pv_diccionario_crudo.xlsx")
  