# 1. Ler e explorar os dados

View(lituratus)

str(lituratus)

summary(lituratus)

# 2. Calcular a média e o desvío padrão da variável selecionada por tipo de Hábitat

medias <- tapply(lituratus$Forearm, lituratus$Habitat, mean, na.rm=TRUE)

medias

desvios <- tapply(lituratus$Forearm, lituratus$Habitat, sd, na.rm=TRUE)

desvios

# 3. Esplorar graficamente os dados

# HISTOGRAMA

hist(lituratus$Forearm[lituratus$Habitat == "Forest"],
     xlab = "diferença de envergadura de asa", ylab = "Frequência", 
     main = " Histograma da diferença de envergadura de asa por grupo",
     probability = TRUE, col = "lightgreen", breaks = 10,
     xlim = c(40,70), ylim = c(0, 0.3))

hist(lituratus$Forearm[lituratus$Habitat == "Urban"],
     probability = TRUE, col = "gray", breaks = 20, add = TRUE)

box()

abline(v = medias[1], col = "darkgreen", lwd = 2.5, lty = 3) # Forest
abline(v = medias[2], col = "black", lwd = 2.5, lty = 3) # Urban

# adicionamos a legenda
legend(65, 0.2, legend = c("Forest", "Urban"), bty = "n",
       col = c("lightgreen", "lightgray"), pch = 15, pt.cex = 3)

# BOXPLOT

boxplot(Forearm ~ Habitat, 
        data = lituratus, 
        ylab = "diferença de envergadura de asa", 
        xlab = "Habitat",
        col = c("lightgreen", "lightgray"), 
        pch = 16)

# VIOLINPLOT

vioplot(Forearm ~ Habitat, 
        data = lituratus, 
        ylab = "diferença de envergadura de asa", 
        xlab = "Habitat",
        col = c("lightgreen", "lightgray"))

# 4. Calcular intervalos de 95% de confiança para a média da variável por tipo de Hábitat

intervalos <- tapply(lituratus$Forearm, lituratus$Habitat, intconf, alpha = 0.01)

intervalos

# 5. Testar a hipótese usando o teste t de Student com α=0.05

teste_t <- t.test(Forearm ~ Habitat, data = lituratus)

teste_t

curve(dt(x, 78), from = -25, to = 25, col = "black", xlab = "Valores de t", 
      main = "Distribuição t Student da hipótese nula", 
      ylab = "Densidade probabilística", lwd = 2.5)

abline(v = qt(1-0.05/2, 78), col = "darkred", lwd = 2.5, lty = 3)
abline(v = qt(0.05/2, 78), col = "darkred", lwd = 2.5, lty = 3)

points(x = teste_t$statistic, y = 0, pch = 18, col = "darkblue", cex = 2)