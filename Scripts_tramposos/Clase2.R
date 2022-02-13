#Dataframes , explorando...

data(ToothGrowth)
str(ToothGrowth)
dim(ToothGrowth)
head(ToothGrowth)

rownames(ToothGrowth)
rows<- paste0(rep("SAMPLE"), 1:60)
rows

rownames(ToothGrowth)<-rows

#Creando una dataframe 
 
df <- data.frame(
  "entero" = 1:3,
  "factor" = c("alto", "medio", "bajo"),
  "letras" = as.character(c("a", "b", "c")))
df

names(df)
colnames(df)

#El operado $ y otras formas de acceso a las dataframes 
df$factor
class(df$factor)
is.vector(df$factor)


#Accediendo a una lista
notas_estudiantes <- list(nombres = c("Ana", "Clara", "Sofy"),
                          id_estudiante = c("i1", "i2", "i3"),
                          notas = c(10, 9,7))

notas_estudiantes$nombres
notas_estudiantes[["nombres"]]

#Accediendo a una matriz
mat<- matrix(1:10, ncol = 5, nrow = 2)
mat[1,1]
mat[1 ,] #acceder primera fila
mat[, 1] #acceder a la primera columna
is.vector(mat[, 1])
mat[1:2 , 2:4]
as.data.frame(mat)


#Creando subconjuntos
evaluaciones<- as.data.frame(notas_estudiantes)
evaluaciones[c(1,2)]
evaluaciones[1:2]
evaluaciones[1]
evaluaciones[-1]
evaluaciones[-1,]
evaluaciones[c("nombres","notas")]
evaluaciones$notas > 8
mas_de_8<-evaluaciones[evaluaciones$notas > 8,]
mas_de_8
evaluaciones[!(evaluaciones$notas > 8 | evaluaciones$nombres == "Clara"), ]
evaluaciones[evaluaciones$nombres == "Sofy",]


#Importando datos
data()

getwd()
setwd("/home/steph/Desktop/")
data<- read.delim("/home/steph/Documents/18S_RESULTS/ALPHA/alpha_mean_treat_data.txt")
#utilizar import dataset 
download.file(
  url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", 
  destfile = "iris.data")
iris_dat<-readr::read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data")

#Funciones de importación
iris_data<- read.csv("iris.data", header = F)
iris_data<- read.delim("iris.data",header = F, sep = ",")
library(readr)
iris_data<-read_csv("iris.data", col_names = c("Longitud.sepalo", "Ancho.Sepalo" ,
                                               "Longitud.Petalo" ,"Ancho.Petalo" , "Especies"  ))

data<- read_csv("../Data/penguins_size.csv")

#importar con "import dataset" botón en el panel derecho supeior
library(readxl)
datos<- read_excel("datos_excel.xlsx")

#Funciones básicas en R

sort(evaluaciones$notas)
order(evaluaciones$notas)
max(evaluaciones$notas)
which.max(evaluaciones$notas)
ind <- which(evaluaciones$nombres == "Ana")
ind
evaluaciones[ind,]
evaluaciones[order(evaluaciones$notas),]


#Match función
v1<- c("Uvas", "Peras", "Mandarinas", "Plátanos", "Manzanas")
v2<- c("Uvas","Cerezas", "Mandarinas", "Naranjas", "Manzanas") 
match(v1, v2)
match(c("Peras", "Plátanos"), v1)
ind<-match(c("Peras", "Plátanos"), v1)
v1[ind]

ind2<- match(v1, v2)
frutas<- data.frame(persona1=v1,persona2=v2)
frutas[ind,]
na.omit(frutas[ind2,])     #na.omit() nos permite quitar las celdas que contienen NA's

#%in%
c("Peras", "Plátanos") %in% frutas$persona1
match(c("Peras", "Plátanos"), frutas$persona1)
which(frutas$persona1 %in% c("Peras", "Plátanos"))

#familia apply
apply(array, margin, ...)
matriz<- matrix(1:20, ncol = 5, nrow = 4)
matriz
apply(X = matriz, MARGIN = 2, FUN = sum)
colSums(matriz)
multiples.func <- function(x) {
  c(sum = sum(x), prom = mean(x), max = max(x))}
apply(X = matriz, MARGIN = 2, FUN = multiples.func)

#Estructuras de control
#if-else

if(10>2) {"Verdadero"
} else {
  "Falso"
}

if(10<2) {"Verdadero"
} else {
  "Falso"
}

ifelse((10>2), "Verdadero", "Falso")
ifelse(evaluaciones$notas>7,  "Aprobado", "Reprobado")

#for
un_vector<- 1:10
for(i in un_vector) {
  print(i*2)
}

#while
umbral <- 3
valor <- 0
while(valor < umbral) {
  print("Aún no llegas al umbral")
  valor <- valor + 1
}