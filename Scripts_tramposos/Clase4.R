#Clase 4

data("ToothGrowth")

#Seleccionando columnas
head(ToothGrowth[1:2])
head(ToothGrowth[-3])
head(ToothGrowth[c("dose", "len")])
head(ToothGrowth[-1:-2,1:2])

#Filtrando filas
head(ToothGrowth[-1:-2,])
head(ToothGrowth[which(ToothGrowth$supp == "VC"),])

ind<-ToothGrowth$len>27
ind2<- ToothGrowth$sup=="OJ"
head(ToothGrowth[ind,])
head(ToothGrowth[ind2,])

#Filtrando NA's
promedio_clases<- data.frame(clase=c("M", "M", "M", "B", "B", "B"),
                             notas=c(7,8,9, 10, 9, NA))
mean(promedio_clases$notas)

mean(promedio_clases$notas, na.rm=TRUE)

is.na(promedio_clases$notas)

promedio_clases[!is.na(promedio_clases$notas),]

na.omit(promedio_clases)

#Subset
subset(ToothGrowth, dose=="0.5")
subset(ToothGrowth, dose=="0.5" & supp =="OJ" & len >10)

#Creando una nueva columna
promedio_notas<- data.frame(estudiante = c("E1", "E2", "E3",
                                           "E1", "E2", "E3"),
                            clase=c("M", "M", "M",
                                    "B", "B", "B"),
                            notas=c(7,8,9, 10, 9, 8))
promedio_notas

promedio_notas<-within(promedio_notas,
                       nota_ponderada <- notas*ponderacion) 

promedio_notas<-transform(promedio_notas,
                          nota_ponderada = notas*ponderacion) 

promedio_notas$ponderacion <- c(0.7,0.7,0.7, 0.3,0.3,0.3)

#Aggregate()
aggregate(notas ~ clase, data = promedio_notas, mean)

aggregate(notas ~ estudiante, data = promedio_notas, median)

aggregate(notas ~ clase+estudiante, data = promedio_notas, sum)

#Renombrando columnas y datos
colnames(promedio_notas)
colnames(promedio_notas)[2] <- "curso"
colnames(promedio_notas)

promedio_notas2<- promedio_notas
names(promedio_notas2) <- c("a", "b", "c", "d")

#Renombrando valores en una columna
promedio_notas$curso <- ifelse(promedio_notas$curso == "M", "Matemáticas", "Biología")   

promedio_notas$curso 

#cbind y rbind
correcion_nota<- c(10,9,8,8,9,10)
cbind(promedio_notas, correcion_nota)
cbind(promedio_notas, promedio_notas)

#rbind -error
notas_fisica<- data.frame(estudiante = c("E1", "E2", "E3"),
                          curso=c("F", "F", "F", "F", "F", "F"),
                          notas=c(7, 9, 8))
rbind(promedio_notas, notas_fisica)

#rbind - bueno
notas_fisica<- data.frame(estudiante = c("E1", "E2", "E3"),
                          curso=c("F", "F", "F"),
                          notas=c(7, 9, 8),
                          ponderacion = c(0.1, 0.1, 0.1), 
                          nota_ponderada= c(10, 9, 8))
rbind(promedio_notas, notas_fisica)

#merge
x <- data.frame(k1 = c(1,3,3,4,5), k2 = c("a1","a2","a3","a4","a5"), data = 1:5)
y <- data.frame(k3 = c(2,2,6,4,5), k2 = c("a1","a2","a3","a4","a5"), data = 1:5)

merge(x, y, by = "k2")
merge(x, y, by = c("k2", "data"), all = TRUE) 

#Ordenando tablas en base a una columna/criterio
promedio_notas <- promedio_notas[order(promedio_notas$notas, 
                                       decreasing = TRUE),]
promedio_notas

promedio_notas <- promedio_notas[order(promedio_notas$notas, 
                                       promedio_notas$ponderacion),]
promedio_notas

#Funciones adicionales
examen <- data.frame("q1" = c(1, 0, 0, 0, 0),
                     "q2" = c(1, 0, 1, 1, 0),
                     "q3" = c(1, 0, 1, 0, 0),
                     "q4" = c(1, 1, 1, 1, 1),
                     "q5" = c(1, 0, 0, 1, 1))
rowMeans(examen)
colMeans(examen)
rowSums(examen)
colSums(examen)
nrow(examen)
ncol(examen)

#ejemplo aplicado
primera<- data.frame(Nombre= c("Astrid", "Lea", "Sarina","Remon",
                               "Letizia", "Babice", "Jonas", 
                               "Wendy","Nivedithia", "Gioia"),
                     Sexo= c("F", "F", "F", "M", "F", "F", 
                             "M", "F", "F", "F"),
                     Edad= c(30,25,25,29,22,22,35,19,32,21))

segunda<- data.frame(Nombre= c("Astrid", "Lea", "Sarina", "Remon",
                               "Letizia", "Babice", "Jonas", "Wendy",
                               "Nivedithia", "Gioia"),
                     Superheroe= c("Batman", "Superman", "Batman",
                                   "Spiderman","Batman","Antman",
                                   "Batman","Superman",
                                   "Maggot", "Superman"),
                     Tatuajes= c(11,15,12,5,65,3,9,13,900,0))


primera
segunda

# Resolviendo
#1. Combina las dos tablas en una sola y completa las siguientes asignaciones.\

encuestas<- merge(primera, segunda, by = "Nombre")

#2. ¿Cuál es la edad media de las mujeres y hombres por separado? cómo podemos hacer esto?\

aggregate(Edad ~ Sexo, data = encuestas, mean)

#3. ¿Cuál fue el número más alto de tatuajes en un hombre?\

males<- subset(encuestas, Sexo=="M")
max(males$Tatuajes)

#4. ¿Cuál es el porcentaje de mujeres debajo de 32 años?\

#5. fem<- subset(encuestas, Sexo=="F")
fem_32<- fem[fem$Edad<32,]

(nrow(fem_32)/nrow(fem))*100

#5.Agrega una nueva columna a a la data llamada `tatuajes.por.año` que muestre cuántos tatuajes por año se ha hecho cada persona por cada año en su vida.\

encuestas$tatuajes.por.año<- encuestas$Tatuajes/encuestas$Edad
encuestas$tatuajes.por.año

#6. ¿Cuál persona tiene el mayor número de tatuajes por año?\

mayor_tatuaje<-which.max(encuestas$tatuajes.por.año)
encuestas[mayor_tatuaje,]

#7.¿Cuáles son los nombres de las mujeres a las que su superheroe favorito es superman?\
sup<-fem[fem$Superheroe=="Superman",]
sup$Nombre


#8.¿Cuál es la mediana del número de tatuajes de cada persona que está por encima de los 20 años y que su personaje favorito es superman?\

ocho<- subset(encuestas, Edad>20 & Superheroe =="Batman")
median(ocho$Tatuajes)
