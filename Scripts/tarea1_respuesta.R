# ============================================================
# Tarea 1 — R básico para ciencia de datos
# Nombre: Apellido Nombre
# Fecha:
# ============================================================


# ============================================================
# PARTE 1 — Abre y explora los datos
# ============================================================

# Llamamos el dataset PlantGrowth que viene en R base
data(PlantGrowth)

# str() muestra la estructura: 30 filas, 2 variables
str(PlantGrowth)

# head() muestra las primeras 6 filas
head(PlantGrowth)

# dim() indica que el dataset tiene 30 filas y 2 columnas
dim(PlantGrowth)

# summary() da un resumen de cada variable
summary(PlantGrowth)

# La variable categórica (factor) es: group (ctrl, trt1, trt2)
# La variable numérica es: weight
# R muestra group como Factor y weight como numeric — correcto

# Exportamos el dataset como CSV
library(readr)
write_csv(PlantGrowth, "PlantGrowth_exportado.csv")


# ============================================================
# PARTE 2 — Estadísticos descriptivos y gráficos
# ============================================================

# Estadísticos descriptivos de weight
mean(PlantGrowth$weight)     # promedio ≈ 5.07 g
median(PlantGrowth$weight)   # mediana ≈ 5.15 g
sd(PlantGrowth$weight)       # desviación estándar ≈ 0.70 g
min(PlantGrowth$weight)      # mínimo ≈ 3.59 g
max(PlantGrowth$weight)      # máximo ≈ 6.31 g
summary(PlantGrowth$weight)  # resumen completo

# El peso promedio es ~5.07 g con poca dispersión (sd ≈ 0.70),
# lo que indica que las plantas tienen pesos relativamente similares.
# La mediana y la media son muy cercanas, señal de simetría en los datos.

# Boxplot de weight por group (R base)
boxplot(weight ~ group, data = PlantGrowth,
        main = "Peso seco de plantas por tratamiento",
        xlab = "Grupo", ylab = "Peso seco (g)",
        col = c("lightgray", "lightblue", "lightgreen"))

# Gráfica de barras con media ± sd usando ggpubr
library(ggpubr)
ggbarplot(data = PlantGrowth, x = "group", y = "weight",
          add = "mean_sd",
          fill = "group",
          palette = "jco",
          xlab = "Grupo",
          ylab = "Peso seco (g)")


# ============================================================
# PARTE 3 — Análisis estadístico
# ============================================================

# -- Normalidad --
hist(PlantGrowth$weight, main = "Histograma de weight", xlab = "Peso seco (g)")

shapiro.test(PlantGrowth$weight)

# p-value ≈ 0.09 > 0.05 → NO rechazamos H0
# Los datos SÍ siguen una distribución normal.
# Se cumple el supuesto de normalidad, podemos usar ANOVA.

# -- ANOVA de una vía --
modelo <- lm(weight ~ group, data = PlantGrowth)
anova(modelo)

# p-value ≈ 0.016 < 0.05 → rechazamos H0
# El tratamiento SÍ tiene un efecto significativo sobre el peso de las plantas.
# Al menos uno de los grupos difiere de los demás.

# -- Prueba de Tukey --
fm <- aov(modelo)
TukeyHSD(fm, "group")

# trt1 vs trt2: p ajustado < 0.05 → diferencia significativa
# ctrl vs trt1: p > 0.05 → no hay diferencia significativa
# ctrl vs trt2: p > 0.05 → no hay diferencia significativa
# El tratamiento 2 produce plantas más pesadas que el tratamiento 1,
# pero ninguno difiere significativamente del control.
