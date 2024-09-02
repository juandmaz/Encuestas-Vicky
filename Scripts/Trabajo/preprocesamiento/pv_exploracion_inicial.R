#exploracion----

#duplicados fila----
get_dupes(base)


#ver que clases de columnas hay-----
map(base, class) %>% 
  unique() %>% 
  unlist()


#cantidad categorias----
map_chr(base, ~class(.)[1]) %>% 
  table() 

map_chr(base, ~class(.)[1]) %>% 
  table() %>%
  prop.table() %>%
  round(.,2)

#ver columnas que son clase character----

base %>%
  select(where(is.character)) %>%
  names()

base %>%
  select(where(~is.character(.)| is.numeric(.))) 


#porcentaje NA----

#por columna
map_dbl(base, 
        ~round(sum(is.na(.)/length(.)),2)) %>%
  as.vector() %>% 
  summary()

#% de variables que tienen 100% NA
round(sum(as.vector(map_dbl(base, ~round(sum(is.na(.)/length(.)),2)))==1)/length(base),2)

#% de variables que tienen al menos la mitad de NA
round(sum(as.vector(map_dbl(base, ~round(sum(is.na(.)/length(.)),2)))>=0.5)/length(base),2)


#por fila
apply(base, 1, function(x) round(sum(is.na(x)/length(x)),2)) %>%
  as.vector() %>% 
  summary()

#filas enteramente vacias
sum(rowSums(is.na(base)) == ncol(base))

#graficoNA
naniar::vis_miss(base, warn_large_data = F)

