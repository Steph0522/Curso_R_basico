data(PlantGrowth)
cat("Shapiro p =", shapiro.test(PlantGrowth$weight)$p.value, "\n")
print(summary(aov(weight ~ group, data = PlantGrowth)))
print(TukeyHSD(aov(weight ~ group, data = PlantGrowth)))
