####CLASE 1####

#MI SCRIPT

#Cargar datos
data()
#Abrir archivo
read.csv()

#Declarar objetos
a <- 1

#Explorando datasets
library(datasets)
data(iris)
summary(iris)
boxplot(iris)
hist(iris$Sepal.Width)

#Directorio de trabajo 
getwd() #Conocer directorio de trabajo 
setwd() #Cambiar directorio de trabajo
#Conocer archivos en mi directorio de trabajo 
list.files()
list.dirs()

#Contenido de la sesión
ls()

#Proyecto nuevo

#Instalando paquetes 
install.packages("tidyverse")
install.packages("iNEXT")

#Más instalación de paquetes
devtools::install_github('rstudio/rmarkdown')

#Cargar paquetes
library(tidyverse)
library(ggplot2)

#Tipos de objetos en R
a <- 1                                                  
letra <- "a"                                           
b <- c(1,2,3)                                          
c <- matrix(1:10)                                      
d <- data.frame(Especie = c("A", "B"), Longitud=c(1,2)) 
e <- list(c(1:20), c(1:10))    

#Explorando objetos
a
print(a)
View(d)

#Guardar y exportar objetos de R 
library(readr)
write_tsv(d, "data.tsv")
write_csv(d, "data.csv")
write.table(d, "data.txt", sep = "\t", row.names = FALSE)

#Exportar a excel
library(openxlsx)
write.xlsx(d, "data.xlsx")

#Exportar como objeto de R
saveRDS(d, "data.RDS")
#Importar a R
data <- readRDS("data.RDS")

#Funciones
log(a)
help(log)
?log 
args(log)

#Operadores matematicos
2+3
a+a
3-a
10*pi

#Declarando funciones
average <- function(x){sum(x)/length(x)}
x <- 1:100
average(x)
average(1:10)

#Tipos de datos
letra <- "a"
caracteres <- c("1", "letras", "mi espacio", "¡Hola!")
a <- 5
b<- c(1,2,3)

#Función class()
class(d)
class(letra)
class(a) 
class(b)

#Coerción de datos
caracter <- as.character(b)
caracter
#Puedo pasar de caracter a numerico
as.numeric(caracteres) 

#Tipos de estructura de datos 
#Vector
colores <- c("red", "blue", "black")
class(colores)
is.vector(colores)

#Vectorización
vec<- 1:10
vec
vec*10

#Matriz
vec
matriz<- matrix(1:10)
matriz
matriz <- matrix(1:10, nrow = 2, ncol = 5)
dim(matriz)
matriz*10
matriz+matriz
t(matriz)

#Lista
vector <- c(1:20)
mat <- matrix(1:30, nrow = 2)
df <- data.frame("numeros"=1:3, "letras"=c("a","b","c"))
lista <- list(vector, mat, df)
lista <- list("vec"= vector,"mat" = mat, "df"= df)
lista[["df"]]
class(lista)
class(lista[[3]])

#Coerción
df_transpuesta <-t(df)
class(df_transpuesta)
df_transpuesta2 <- as.data.frame(df_transpuesta)
class(df_transpuesta2)
df_trans <- as.data.frame(t(df_transpuesta))
class(df_trans)




