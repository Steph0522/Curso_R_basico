#PCA - tema extra

data(iris)
head(iris)

#prcomp

pca_prcomp<- prcomp(iris[,-5], scale. = TRUE)
names(pca_prcomp)

head(pca_prcomp$x)
head(pca_prcomp$rotation)

pca_prcomp$sdev^2
pca_prcomp$sdev^2 / sum(pca_prcomp$sdev^2)

biplot(x = pca_prcomp, scale = 0, cex = 0.6, col = c("blue4", "brown3"))


#vegan-rda
#install.packages("vegan")
library(vegan)
pca_vegan<- rda(iris[,-5], scale=TRUE)
#summary(pca_vegan)
pca_vegan_sum<-summary(pca_vegan)
names(pca_vegan_sum)


biplot(pca_vegan, type = c("text","points"))
ordihull(pca_vegan, group = iris$Species, col = c("red","green","blue"))
spp.names <- levels(iris$Species)
legend("topright", col = c("red","green","blue"), lty = 1, legend = spp.names)

#factominer y factoextra
#install.packages("factoextra")
#install.packages("FactoMineR")
library(FactoMineR)
pca_facto <- PCA(iris[,-5], graph = FALSE)
print(pca_facto)

fviz_pca_var(pca_facto, col.var="steelblue", labelsize=2)

fviz_pca_ind(pca_facto, geom.ind = "point",  axes = c(1, 2),  habillage=iris$Species,
             addEllipses=TRUE, ellipse.level=0.95) 

fviz_pca_biplot(pca_facto,  habillage = iris$Species, 
                addEllipses = TRUE, col.var = "red",  label = "var", labelsize=3) 

fviz_screeplot(pca_facto)

## PCA gráfica con ggplot2
library(tidyverse)

individuales <- data.frame(pca_prcomp$x) %>% select(PC1, PC2)%>% rownames_to_column(
  var="inds") %>% inner_join(iris %>% rownames_to_column(var="inds"))
vars<-data.frame(pca_prcomp$rotation) %>% select(PC1, PC2)%>% rownames_to_column(var="vars")
componentes<-pca_prcomp$sdev^2 / sum(pca_prcomp$sdev^2)
PC1_label <- paste("PC1", round(componentes[1]*100,1), "%")
PC2_label <- paste("PC2", round(componentes[2]*100,1), "%")

ggplot() +
  geom_point(data = individuales, aes(x=PC1, y=PC2, color =Species), size=2)

ggplot() +
  geom_point(data = individuales, aes(x=PC1, y=PC2, color =Species), size=2)+
  geom_segment(data=vars, aes(x=0, xend=PC1, y=0, yend=PC2),
               arrow = arrow(length = unit(0.3,"cm")))


ggplot() +
  geom_point(data = individuales, aes(x=PC1, y=PC2, color =Species), size=1)+
  geom_segment(data=vars %>% mutate(PC1=PC1*2, PC2=PC2*2),
               aes(x=0, xend=PC1, y=0, yend=PC2),
               arrow = arrow(length = unit(0.3,"cm")))+
  geom_text(data=vars %>% mutate(PC1=PC1*2, PC2=PC2*2), 
            aes(x=PC1, y=PC2, label= vars),
            nudge_x = 0.7, nudge_y = 0.2)


ggplot() +
  geom_point(data = individuales, aes(x=PC1, y=PC2, color =Species), size=2)+
  geom_segment(data=vars %>% mutate(PC1=PC1*2, PC2=PC2*2), aes(x=0, xend=PC1, y=0, yend=PC2),
               arrow = arrow(length = unit(0.3,"cm")))+
  geom_text(data=vars %>% mutate(PC1=PC1*2, PC2=PC2*2), aes(x=PC1, y=PC2, label= vars),
            nudge_x = 0.7, nudge_y = 0.2)+
  geom_vline(xintercept = 0, linetype = 5) +
  geom_hline(yintercept = 0, linetype = 5) +theme_classic()+
  stat_ellipse(data = individuales, aes(x=PC1, y=PC2, color =Species))+
  scale_color_manual(values = c("red", "blue", "green"))+
  ylab(PC2_label)+xlab(PC1_label)

coef<- 2
ggplot() +
  geom_point(data = individuales, aes(x=PC1, y=PC2, color =Species), size=2)+
  geom_segment(data=vars[-1]*coef, aes(x=0, xend=PC1, y=0, yend=PC2),
               arrow = arrow(length = unit(0.3,"cm")))+
  scale_y_continuous(name = "Individuals", sec.axis = sec_axis(
    ~./coef, name="vars"))+theme_linedraw()+xlab("")



