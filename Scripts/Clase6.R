#Rbase

#tipos de gráficas
plot(x=iris$Sepal.Length, y=iris$Sepal.Width)
plot(x=iris$Sepal.Length, y = iris$Species)
plot(x=iris$Sepal.Length)
plot(x = iris$Species, y = iris$Sepal.Length)
plot(x=iris$Species, y=iris$Species)
plot(x=iris$Species)

#histogramas
hist(x = iris$Sepal.Length, main = "Histograma de longitud de sepalo", 
     xlab = "Longitud", ylab = "Frecuencia",
     col = "purple")

#Dispersión
plot(x = iris$Petal.Length, y = iris$Petal.Width, col = iris$Species, xlab = "Largo", ylab = "Ancho")
plot(x = iris$Petal.Length, y = iris$Petal.Width, col = iris$Species, 
     main = "Pétalo Iris por especie", xlab = "Largo", ylab = "Ancho")
legend(x = "topleft", legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = c("black", "red", "green"), title = "Especie")
#Boxplots
plot(x=iris$Species, y = iris$Sepal.Length, xlab = "Especie", ylab = "Longitud Sépalo", 
     col = c("purple", "pink", "blue"))
boxplot(formula = Sepal.Length ~ Species, data =  iris, xlab = "Especie", 
        ylab = "Longitud Sépalo",   col = c("purple", "pink", "blue"))

#Otros gráficos
?plot
df<- data.frame(x= c(1:5),
                y= c(200, 400, 600, 700, 500))
plot(df$x, df$y, type = "p", main = 'type = "p"')
plot(df$x, df$y, type = "l", main = 'type = "l"')
plot(df$x, df$y, type = "b", main = 'type = "b"')
plot(df$x, df$y, type = "c", main = 'type = "c"')
plot(df$x, df$y, type = "s", main = 'type = "s"')
plot(df$x, df$y, type = "h", main = 'type = "h"')

plot(df$x, df$y, type = "l", lty=1, main = 'type = "l1"')
plot(df$x, df$y, type = "l", lty=2, main = 'type = "l2"')
plot(df$x, df$y, type = "l", lty=3, main = 'type = "l3"')
plot(df$x, df$y, type = "l", lty=4, main = 'type = "l4"')
plot(df$x, df$y, type = "l", lty=5, main = 'type = "l5"')
plot(df$x, df$y, type = "l", lty=6, main = 'type = "l6"')

#Barplots
barplot(y ~ x , data = df, main = "Barplot", col = "darkred")
plot(df$x, df$y, type = "p", main = 'type = "p-capas"')
abline(h=400, v=3, col="red", lty=2)
text(df, labels=rownames(df), cex=0.7, pos=2, col="blue")

#https://www.rstudio.com/resources/cheatsheets/

##GGPLOT2
data("ToothGrowth")
library(dplyr)
library(ggplot2)
ggplot(data = ToothGrowth)
ToothGrowth %>% ggplot()
ToothGrowth %>% ggplot(aes(x = dose, y = len, fill=supp))

ToothGrowth %>% ggplot(aes(x = dose, y = len, color=supp)) +
  geom_point()

ToothGrowth %>% ggplot(aes(x = dose, y = len, color=supp)) +
  geom_line()

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, color=supp)) +  geom_boxplot()

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes( x = dose, y = len, fill=supp)) +  geom_bar(stat = "identity", position = "dodge")

ToothGrowth %>% ggplot(aes(x = dose, y = len, color=supp)) +
  geom_point()+
  geom_text(aes(label=rownames(ToothGrowth)))+
  geom_line()


#colores
ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+
  scale_fill_manual(values = c("#FF00FF","#00FFFF"))

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+
  scale_fill_viridis_d(option = "C")

#titulos
ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+
  ylab("Longitud diente")+
  xlab("Dosis")+
  ggtitle("Longitud de dientes por Dosis aplicada")


#escalas
ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+
  ylab("Longitud diente")+
  xlab("Dosis")+
  ggtitle("Longitud de dientes por Dosis aplicada") +
  scale_x_discrete(limits = c("D2", "D0.5", "D1"), position ="bottom" )+
  scale_y_continuous(breaks = c(0, 20,40), limits = c(0,40), labels = c("L0", "L20", "L40"))

#temas
ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ theme_light()+
  ggtitle("theme_light()")

#theme()
ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ 
  theme(axis.title = element_text(size = "14", colour = "blue"),
        title = element_text(size = 16, colour = "red"),
        legend.position = "top")

#facets
ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ 
  facet_grid(supp~.)


ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ 
  facet_grid(.~supp)

ToothGrowth %>%  mutate(dose=case_when(
  dose==0.5~"D0.5",dose==1~"D1", dose==2~"D2")) %>% ggplot(
    aes(x = dose, y = len, fill=supp)) +  geom_boxplot()+ 
  facet_wrap(~supp+dose, ncol = 3, nrow = 2)

#guardar
ggsave(filename = "plot.png", plot = a1, dpi = 300,width = 4, height = 3.5)

#stat_summary
ToothGrowth %>% 
  ggplot(aes(x = dose, y = len)) +
  stat_summary(geom = "line", fun = mean, aes(group = supp, color = supp), size = 1.2) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, aes(group = supp), width = 0.1)

#ggpubr
library(ggpubr)
ggboxplot(ToothGrowth, x = "dose", y = "len", color = "dose",add = "jitter", shape = "dose")
ggbarplot(ToothGrowth, x = "dose", y = "len", fill = "dose", add = "mean_se")

ggboxplot(ToothGrowth, x = "dose", y = "len", color = "dose",facet.by = "supp")


comparaciones <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )

ggbarplot(ToothGrowth, x = "dose", y = "len", fill = "dose", add = "mean_sd")+
  stat_compare_means(comparisons = comparaciones, label = "p.signif")

#ggcats
#> install.packages("magick")
# remotes::install_github("R-CoderDotCom/ggcats@main")

library(ggcats)

ToothGrowth$cats <- factor(ToothGrowth$dose,
                           levels = c(0.5,1,2),
                           labels = c("mouth", "grumpy", "pusheen_pc"))

ToothGrowth %>%  ggplot(aes(y = len, x = dose)) + 
  geom_cat(aes(cat = cats), size = 4)  +  xlim(c(0.25, 2.25))

# ggtexttable()
library(dplyr)
df<- iris %>% slice(c(1:4))
ggtexttable(df, rows = NULL)
ggtexttable(df, rows = NULL, theme = ttheme("blank"))
ggtexttable(df, rows = NULL, theme = ttheme("light"))
ggtexttable(df, rows = NULL, theme = ttheme("classic"))
ggtexttable(df, rows = NULL, theme = ttheme("minimal"))
ggtexttable(df, rows = NULL, theme = ttheme("lVioletWhite"))
ggtexttable(df, rows = NULL, theme = ttheme("mVioletWhite"))

ggtexttable(df, rows = NULL,  theme = ttheme("classic")) %>%  table_cell_font(row = 3, column = 2, face = "bold", color = "red")
ggtexttable(df, rows = NULL,  theme = ttheme("classic")) %>%  table_cell_bg(row = 2:5, column = 3, fill="yellow")
ggtexttable(df, rows = NULL,  theme = ttheme("classic")) %>% tab_add_title(text = "Data iris",  size = 14, face="bold") %>%
  tab_add_footnote(text = "*Alguna nota", size = 10, face = "italic")
