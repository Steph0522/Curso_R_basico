# Este es mi primer script
library(datasets)
data("iris")
summary(iris)
boxplot(iris)
#directorio y sesión
getwd()  #esta función es para saber donde estoy
list.files()
list.dirs()
ls()
#instalar paquetes en R
install.packages("tidyverse")
library(tidyverse)
#tipos de objetos
a <- 1 #es un numero
letra <- "a" #esto es una letra
b <- c(1,2,3) #esto es un vector
c <- matrix(1:10) #esto es una matrix
v <- c("a", "b", "c")

d <- data.frame(Especie=c("especie1", "especie2"), 
                Long=c(10,20))#es una data frame
e <- list(c(1:20), c(1:10)) #es una lista




library(readr) #esta librería es para exportar los datos
write_tsv(d, "data.tsv")
write.table(d, "data.txt")
write.csv(d, "data.csv")

saveRDS(d, "data.RDS") #no es práctico




#práctica
class(a)
class(c)
class(b)
class(d)
class(iris)


as.character(a)
as.numeric(letra)

data$Especie <- as.factor(data$Especie)
str(data)


colores <- c("blue", "pink", "green", "black", "red")
class(colores)
is.vector(colores)
is.data.frame(colores)


un_vector <- c(1:10)
un_vector

un_vector+1
un_vector*2


matri <- matrix(1:20, nrow = 3, ncol = 4)
matri
t(matri)


log(a)
sqrt(a)
?log
?write.csv

1+2
3*4
10/2
2-1
