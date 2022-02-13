#Clase 1

# Reconociendo el entorno de R studio

# Abriendo un nuevo script y guardarlo con un nombre

# Abriendo un script ya creado

source("C:/mi_script.R")


#Corriendo un script de prueba
library(datasets)
data(iris)
summary(iris)
boxplot(iris)

#Identificado y estableciendo el directorio de trabajo 
getwd()
setwd("/home/steph/Desktop/")

#Contenido del directorio
list.files()
list.dirs()

#Contenido de la sesión
ls()

#Abriendo un proyecto nuevo

#Instalando un paquete 
install.packages("tidyverse")

#llamando un paquete
library(tidyverse)

#Más instalación de paquetes
devtools::install_github('rstudio/rmarkdown')

#tipos de objetos en R
a <- 1                                                   #escalar
letra <- "a"                                             #caracter
b <- c(1,2,3)                                            #vector
c<- matrix(1:10)                                         #matriz
d<- data.frame(Especie=c("A", "B"), Longitud=c(c(1,2)))  #dataframe
e<- list(c(1:20), c(1:10))                               #lista

#explorando objetos
a
print(a)
print(f)

#install.packages <- 2  equivocado!

#Guardar los espacios de trabajo y exportar objetos de R
library(readr)
write_tsv(d, "data.tsv")
write.table(d, "data.txt", sep = "\t")
write_csv(d, "data.csv")

#guardando y  abriendo un RDS
saveRDS(d, "data.RDS")

#funciones
log(a)
help("log")
?log
args(log)

log(x = 8, base = 2)
log(8,2)

#excepciones a los paréntesis
a + a
a - 2
1 * pi
2 / 3
4 ^ a

#declarando funciones
average<- function(x){sum(x)/length(x)}
x<- 1:100
average(x)
mean(x)

#tipos de objetos
a<- 5
as.character(a)
as.factor("medio")

#vectores
colores<- c("red", "black", "blue")
is.vector(colores)
class(colores)

#operaciones vectores
un_vector <- c(1:10)
un_vector*10
un_vector+1
un_vector + un_vector


#matrices
vect<- 1:20
matr<- matrix(1:20)
matri<-matrix(1:20, nrow = 5, ncol = 4)
dim(matri)
matri

#operaciones matrices
matri*2
t(matri)

#listas
un_vector <- 1:20
una_matriz <- matrix(1:20, nrow = 2)
una_df     <- data.frame("numeros" = 1:3, "letras" = c("a", "b", "c"))
una_lista <- list("vec" = un_vector,"mat" = una_matriz,"df" = una_df)
una_lista


#coercionando una matriz o dataframe
df_transpuesta<- t(una_df)
class(df_transpuesta)

df_transpuesta<- as.data.frame(t(una_df))
class(df_transpuesta)
