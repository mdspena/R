load("suplemento.RData")

View(suplemento)

str(suplemento)

summary(suplemento)

suplemento$dif_massa <- suplemento$massa_final - suplemento$massa_inicial

medias <- tapply(suplemento$dif_massa, suplemento$grupo, mean)

medias

# HISTOGRAMA

# grupo controle
hist(suplemento$dif_massa[suplemento$grupo == "Controle"], # os valores do grupo Controle
     xlab = "diferença de massa", ylab = "Frequência", 
     main = " Histograma da diferença de massa por grupo",
     probability = TRUE, col = "lightpink", breaks = 10,
     xlim = c(-4,6), ylim = c(0, 0.6))

hist(suplemento$dif_massa[suplemento$grupo == "Tratamento"], # grupo tratamento
     probability = TRUE, col = "lightgreen", breaks = 10, add = TRUE)

box()

abline(v = medias[1], col = "darkred", lwd = 2.5, lty = 3) # grupo controle
abline(v = medias[2], col = "darkgreen", lwd = 2.5, lty = 3) # tratamento

legend(4, 0.6, legend = c("Controle", "Tratamento"), bty = "n",
       col = c("lightpink", "lightgreen"), pch = 15, pt.cex = 3)

# BOXPLOT

boxplot(dif_massa ~ grupo, data = suplemento, 
        ylab = "diferença de massa", xlab = "Grupo",
        col = c("lightpink", "lightgreen"), 
        pch = 16
)

# VIOLIN PLOT

install.packages("vioplot")

require("vioplot")

vioplot(dif_massa ~ grupo, 
        data = suplemento, 
        ylab = "diferença de massa", 
        xlab = "Grupo",
        col = c("lightpink", "lightgreen"))

intconf <- function(x, alpha) {
  
  media <- mean(x, na.rm = TRUE)
  dp <- sd(x, na.rm = TRUE)
  n <- length(x)
  
  nomes <- c(paste0(alpha/2*100, "%"), paste0((1-alpha/2)*100, "%"))
  
  resultado <- c(media + (qt(alpha/2, n-1) * dp / sqrt(n)),
                 media + (qt(1-alpha/2, n-1) * dp / sqrt(n)))
  names(resultado) <- nomes
  resultado
}

intervalos <- tapply(suplemento$dif_massa, suplemento$grupo, intconf, alpha = 0.01)
intervalos # para chamar o resultado

# Gráfico vazio
plot("n", xlim = c(0.5, 2.5), ylim = c(-1,4), xaxt = "n", 
     xlab = " Grupo", ylab = "Diferênca de massa", cex.lab = 1.2)

# Eixo X
axis(side = 1, at = c(1,2), labels = c("Controle", "Tratamento"))

# Médias
points(y = medias, x = c(1,2), cex = 2, pch = 16)

# Intervalos para cada média
lines(y = (intervalos$Controle), x = c(1,1), lwd = 3) # Controle
lines(y = (intervalos$Tratamento), x = c(2,2), lwd = 3) # Tratamento

# Linha horizontal no zero
abline(h = 0, lwd = 3, lty = 2, col = "red")

teste_t <- t.test(dif_massa ~ grupo, data = suplemento)

teste_t

curve(dt(x, 78), from = -11, to = 11, col = "black", xlab = "Valores de t", 
      main = "Distribuição t Student da hipótese nula", 
      ylab = "Densidade probabilística", lwd = 2.5)

# cotas em alfa 0.01
abline(v = qt(1-0.01/2, 78), col = "darkred", lwd = 2.5, lty = 3)
abline(v = qt(0.01/2, 78), col = "darkred", lwd = 2.5, lty = 3)

# valor de t
points(x = teste_t$statistic, y = 0, pch = 18, col = "darkblue", cex = 2)