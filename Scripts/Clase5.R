#Clas 5 - tidyverse

library(tidyverse)

#tibbles
df<- data.frame(letras=c("a", "b", "c"),
                números=1:3)
tib<- as_tibble(df)
class(df)
class(tib)
print(df)
print(tib)

#pipe
#asignando la data a la variable "mi_data"
mi_data<-ToothGrowth
head(mi_data)
#escoger sólo las columnas len y dose
mi_data<- mi_data[c("len", "dose")]
head(mi_data)
#hacer una nueva columna declarando la variable "dose" como un factor 
mi_data$dose<- factor(mi_data$dose, levels = c(0.5,1.0, 2.0), labels =c("D0.5", "D1", "2"))
head(mi_data)
#filtrar solo los que sean de dosis = 1
mi_data<- mi_data[mi_data$dose=="D1",]
head(mi_data)

mi_data<- ToothGrowth %>% select(len, dose) %>% mutate(
  dose=case_when(dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% filter(dose=="D1")
head(mi_data)

#pipe
16 %>% sqrt()
16 %>% sqrt() %>% log2()
log2(sqrt(16))

#select
head(select(.data = ToothGrowth, len, dose))
#seleccionando columnas que queremos
ToothGrowth %>% select(len, dose) %>% head()
ToothGrowth %>% select(len:dose) %>% glimpse()
#Seleccionando columnas que no queremos
ToothGrowth %>% select(-len) %>% glimpse()
ToothGrowth %>% select(starts_with("d")) %>% glimpse()
ToothGrowth %>% select(contains("ose")) %>% glimpse()
ToothGrowth %>% select(ends_with("ose")) %>% glimpse()
ToothGrowth %>% select(matches("o.+e")) %>% glimpse()
colum <- c("supp", "len")
ToothGrowth  %>%  select(!!colum) %>% glimpse()
#selección positiva
ToothGrowth %>% select_if(is.numeric) %>% glimpse()
#selección negativa
ToothGrowth %>% select_if(~!is.numeric(.)) %>% glimpse()
ToothGrowth %>% select_all(toupper) %>% glimpse()
#npositiva
ToothGrowth%>% select_at(vars(contains("ose"))) %>% glimpse()
#negativa
ToothGrowth%>% select_at(vars(-contains("ose"))) %>% glimpse()
ToothGrowth%>% select_at(vars(!contains("ose"))) %>% glimpse()



#filter
head(filter(ToothGrowth, supp=="OJ") )
ToothGrowth %>% filter(supp=="OJ") %>% glimpse()
ToothGrowth %>% filter(len>30) %>% glimpse()
ToothGrowth %>% filter(supp=="OJ", len>30) %>% glimpse()
len_quiero<-c(26.4, 27.3 ,29.4, 23.0)
ToothGrowth %>% filter(len %in% len_quiero) %>% glimpse()
ToothGrowth %>% filter(!len %in% len_quiero) %>% glimpse()
ToothGrowth %>% filter(!is.na(len))
ToothGrowth %>% filter_if(is.numeric, all_vars(between(.,2,20)))
ToothGrowth %>% filter_all(any_vars(. > 30))
ToothGrowth %>% filter_at(vars(len, dose), all_vars(.>1)) %>% head()

#mutate
ToothGrowth %>% mutate(lenlog = log(len)) %>% head()
ToothGrowth %>% mutate_if(is.numeric, round) %>% head()
ToothGrowth %>% mutate_all(tolower) %>% head()
ToothGrowth %>% mutate_all(round) %>% head()
ToothGrowth %>%  mutate_at(vars(contains("ose")), ~(.*100)) %>% head()
ToothGrowth %>%  mutate_at("dose", ~(.*100)) %>% head()

#datos discretos
ToothGrowth %>%  mutate(supp2 = recode_factor(supp,
                                              "OJ" = "Jugo",
                                              "VC" = "Ascórbico",
                                              .default = "other",
                                              .ordered = TRUE)) %>% tail()
ToothGrowth%>%  mutate(dose2 = ifelse(dose > 1, "alto", "bajo")) %>% head()

ToothGrowth%>%mutate(dose = case_when(
  dose == 0.5 ~ "D_0.5",
  dose == 1 ~ "D_1",
  dose == 2 ~ "D_2")) %>%  mutate(
    dose = factor(dose, levels = c("D_2", "D_1", "D_0.5"))) %>% head()

#separate y unite
ToothGrowth %>% unite("interaccion", supp:dose, sep = "_") %>% head()
ToothGrowth%>%mutate(dose = case_when(
  dose == 0.5 ~ "D_0.5",
  dose == 1 ~ "D_1",
  dose == 2 ~ "D_2")) %>% separate(dose, c("D", "dose"),
                                   sep = "_") %>% head()
#summarise
ToothGrowth %>% count(supp, sort=TRUE)
ToothGrowth %>% count(dose, supp, sort=TRUE)
ToothGrowth %>% group_by(supp) %>%count()
ToothGrowth %>% group_by(supp) %>% summarise(promedio = mean(len), suma = sum(len),
                                             n = n(), mediana =median(len))
ToothGrowth %>%  group_by(supp) %>% summarise_if(is.numeric, mean)
ToothGrowth%>%  group_by(supp) %>% summarise_at(vars(contains("ose")), mean)
ToothGrowth %>%  group_by(supp) %>%  summarise_all(mean)

#renombrando columnas
ToothGrowth %>% select(dosis=dose, longitud=len, suplemento=supp) %>% glimpse()
ToothGrowth %>% rename(dosis=dose) %>% glimpse()
ToothGrowth %>% rename_if(is.numeric, ~paste0("Num_", .)) %>% glimpse()
ToothGrowth%>% rename_at(vars(contains("ose")), ~paste0("Num_", .)) %>% glimpse()
ToothGrowth%>% rename_all(toupper) %>% glimpse()

#ordenando tablas
ToothGrowth %>% arrange(len) %>% head()
ToothGrowth %>% arrange(-len) %>% head()
ToothGrowth%>%  arrange(dose, supp) %>%  head()

#join
?inner_join
band_members; band_instruments
band_members %>% full_join(band_instruments)
band_members %>% inner_join(band_instruments, by = "name")
band_members %>% left_join(band_instruments)
band_members %>% right_join(band_instruments)

#otras funciones
ToothGrowth %>% top_n(5, len)
ToothGrowth %>% rownames_to_column(var = "row") %>% head()
ToothGrowth %>% rownames_to_column(
  var = "row") %>% column_to_rownames(var = "row") %>% head()
data(iris)
data_iris<- iris %>% select(Sepal.Length, Sepal.Width) %>% rownames_to_column(var = "ids")
head(data_iris)
tidy_iris<-pivot_longer(names_to = "variable", 
                        values_to = "longitud", 
                        data = data_iris, 
                        cols = Sepal.Length:Sepal.Width) 
head(tidy_iris)
notidy_iris<- tidy_iris  %>% pivot_wider(names_from = variable,
                                         values_from = longitud)
head(notidy_iris)

#Ejemplo aplicado
primera<- data.frame(Nombre= c("Astrid", "Lea", "Sarina", "Remon", "Letizia", 
                               "Babice", "Jonas", "Wendy", "Nivedithia", "Gioia"),
                     Sexo= c("F", "F", "F", "M", "F", "F", "M", "F", "F", "F"),
                     Edad= c(30,25,25,29,22,22,35,19,32,21))
segunda<- data.frame(Nombre= c("Astrid", "Lea", "Sarina", "Remon", "Letizia", 
                               "Babice", "Jonas", "Wendy", "Nivedithia", "Gioia"),
                     Superhéroe= c("Batman", "Superman", "Batman", "Spiderman", "Batman", 
                                   "Antman", "Batman", "Superman", "Maggot", "Superman"),
                     Tatuajes= c(11,15,12,5,65,3,9,13,900,0))

### Resolviendo

#1.  Combina las dos tablas en una sola y completa las siguientes asignaciones.\

encuestas<-primera %>% full_join(segunda)

#2.¿Cuál es la edad media de las mujeres y hombres por separado?
  
encuestas %>% group_by(Sexo) %>% summarise_at(c("Edad"), mean)

#3.¿Cuál fue el número más alto de tatuajes en un hombre?
  
encuestas %>% filter(Sexo=="M") %>% filter(Tatuajes == max(Tatuajes)) 

#4.¿Cuál es el porcentaje de mujeres debajo de 32 años?
fem<- encuestas %>% filter(Sexo=="F")
fem_32<- encuestas %>% filter( Sexo=="F", Edad<32)
(nrow(fem_32)/nrow(fem))*100

#5.Agrega una nueva columna a a la data llamada `tatuajes.por.año` que muestre cuántos tatuajes por año se ha hecho cada persona por cada año en su vida.

encuestas<- encuestas %>% mutate("tatuajesporaño"=Tatuajes/Edad)
encuestas$tatuajesporaño

#6.¿Cuál persona tiene el mayor número de tatuajes por año?
encuestas  %>% filter(tatuajesporaño == max(tatuajesporaño)) 

#7.¿Cuáles son los nombres de las mujeres a las que su superheroe favorito es superman?
 
encuestas %>% filter(Sexo=="F", Superhéroe=="Superman") %>% select(Nombre)

#8.¿Cuál es la mediana del número de tatuajes de cada persona que está por encima de los 20 años y que su personaje favorito es batman?
 
encuestas %>% filter(Edad>20, Superhéroe =="Batman") %>% summarise(mediana=median(Tatuajes))


