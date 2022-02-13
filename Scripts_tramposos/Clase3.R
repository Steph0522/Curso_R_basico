#Data
data(iris)
cols<- c("Largo_Sepalo", "Ancho_Sepalo", "Largo_Petalo", 
         "Ancho_Petalo", "Especies")
colnames(iris)<- cols

#Estadísticos descriptivos
str(iris)
summary(iris)
mean(iris$Largo_Sepalo)
min(iris$Largo_Sepalo)
max(iris$Largo_Sepalo)
median(iris$Largo_Sepalo)
quantile(iris$Largo_Sepalo, 0.25) # primer cuantil
quantile(iris$Largo_Sepalo, 0.75) # tercer cuantil

sd(iris$Largo_Sepalo)  #desviación estándar
range(iris$Largo_Sepalo) #min y max
IQR(iris$Largo_Sepalo) #diferencia entre el tercer y primer cuantil
var(iris$Largo_Sepalo) #varianza
sd(iris$Largo_Sepalo) / mean(iris$Largo_Sepalo) #Coeficiente variación

lapply(iris[, 1:4], sd)

#Gráficos descriptivos
boxplot(iris$Largo_Sepalo ~ iris$Especies)
library(ggpubr)
ggbarplot(data = iris, x = "Especies", y = "Largo_Sepalo", add = "mean_sd")

#Normalidad

hist(iris$Ancho_Sepalo)
plot(density(iris$Ancho_Sepalo))

qqnorm(iris$Ancho_Sepalo)
qqline(iris$Ancho_Sepalo)

library(car) # cargamos el paquete car
qqPlot(iris$Ancho_Sepalo )

shapiro.test(iris$Ancho_Sepalo)

#Correlación
cor.test(iris$Largo_Petalo, iris$Ancho_Petalo)

cor(iris$Largo_Petalo, iris$Ancho_Petalo)                           #datos normales
cor(iris$Largo_Petalo, iris$Ancho_Petalo, method = "spearman")      #datos no normales

#Regresión
modelo <- lm(Ancho_Petalo ~ Largo_Petalo, data = iris)
plot(modelo, which = 2)
summary(modelo)

#Anova 1 vía
modelo_ancho <- lm(Ancho_Sepalo ~ Especies, data = iris) 
plot(modelo_ancho, which = c(1,2) )
anova(modelo_ancho)

#Tukey
fm1<- aov(modelo_ancho)
TukeyHSD(fm1, "Especies", ordered = TRUE)

library(agricolae)
tuk<-HSD.test(fm1, "Especies", group = TRUE, console = FALSE)
tuk$groups

library(ggpubr)
ggbarplot(data = iris, x = "Especies" ,
          y = "Ancho_Sepalo", 
          fill="Especies",
          position = position_dodge(),
          add = c("mean_sd"),
          width = 0.4)+theme(legend.position = "none") +    
  annotate(geom = "text",     
    label = c("a", "c", "b"),   
     x = c(1, 2, 3),            
     y = c(4, 3.5, 3.7),   
     size = 5, fontface = "bold") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.line = element_line(size = 1.2)) +
  theme(text = element_text(size = 12, color = "black", face = "bold")) +
  theme(axis.text = element_text(size = 12, color = "black", face = "bold"))

#Anova 2 vías
data("ToothGrowth")
str(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose, 
                           levels = c(0.5, 1, 2),
                           labels = c("D0.5", "D1", "D2"))
head(ToothGrowth)

library(ggpubr)
ggboxplot(data = ToothGrowth, x = "supp", y = "len", fill = "supp")
ggboxplot(data = ToothGrowth, x = "dose", y = "len", fill = "dose")

ggboxplot(data = ToothGrowth, x = "dose", y = "len", fill = "supp")
ggline(ToothGrowth, x = "dose", y = "len", color = "supp", add = c("mean_se", "jitter"))

anova1<- aov(len ~ supp + dose, data = ToothGrowth)
anova2<-  aov(len ~ supp * dose, data = ToothGrowth)
summary(anova1)
summary(anova2)

#Otras

#dos niveles
t.test(len ~ supp, data = ToothGrowth, paired = TRUE)
wilcox.test(len ~ supp, data = ToothGrowth, paired = TRUE)

#más de dos niveles
kruskal.test(len ~ dose, data = ToothGrowth)
