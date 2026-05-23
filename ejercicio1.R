data("iris")

#1. Usa str(), head() y dim() para explorar iris. ¿Cuántas filas y columnas tiene?
str(iris)
head(iris)
dim(iris) # 150 filas y 5 columnas

#Reizmel Erazo

#2. Accede a la columna Sepal.Length con $. ¿Cuál es su valor máximo y mínimo?
max(iris$Sepal.Length)
min(iris$Sepal.Length)

#Wilman Mejia


#3. ¿Qué fila tiene el pétalo más largo (Petal.Length)? Usa which.max().
which.max(iris$Petal.Length)
#Vilma GP

#4. Crea una nueva columna razon en iris calculando Sepal.Length / Petal.Length.

iris$razon <- iris$Sepal.Length / iris$Petal.Length

#Jorge Montoya

#5. Ordena razon de mayor a menor con sort(). ¿Cuál es el valor más alto?

sort(iris$razon, decreasing = TRUE)
#Jessica Aguilar
  
#6. ¿En qué posición está el valor mínimo de razon? Usa which.min() 
#y accede a esa fila completa.

iris[which.min(iris$razon),]
