#1 EXPLORACIÓN Y LLAMADO DE LA DATA
#ESDRAS ESPINOZA
data("chickwts")
str(chickwts)
summary(chickwts)

#2. ESTADISTICA DESCRIPTIVA BÁSICA
#PABLO RICO
mean(chickwts$weight)
sd(chickwts$weight)
min(chickwts$weight)
max(chickwts$weight)

#3. BOXPLOT
#ROMI PADGETT
boxplot(weight ~ feed, data = chickwts)

#4. NORMALIDAD
#REIZMEL ERAZO
#Verifica la normalidad de weight con shapiro.test(). ¿Se cumple el supuesto?
shapiro.test(chickwts$weight)

#ANOVA- ELVIN ZEPEDA
anova_feed <- aov(weight ~ feed, data = chickwts)
anova(anova_feed)

#TUKEY - JORGE MONTOYA
TukeyHSD(aov(weight ~ feed, data = chickwts))


# HOMOCEDASTICIDAD? REIZMEL
library(car)
leveneTest(weight ~ feed, data = chickwts)


