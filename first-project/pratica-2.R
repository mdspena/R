set.seed(123)

universo_1 <- rnorm(n = 5000000, mean = 5, sd = 2.5)

mean(universo_1)

sd(universo_1)

hist(universo_1, main = "Histograma de Variável Gaussiana",
     col = "lightgreen", xlab = "Variável", ylab = "Frequência", probability = TRUE)

abline(v = mean(universo_1), col = "red", lwd = 2)

amostra <- sample(universo_1, size = 150)

amostra

mean(amostra)

sd(amostra)

hist(amostra, main = "Histograma de Variável Gaussiana",
     col = "lightgreen", xlab = "Variável", ylab = "Frequência", probability = TRUE)

abline(v = mean(amostra), col = "red", lwd = 2)

amostras_1 <- matrix(nrow = 150, ncol = 200)

# Amostragem

for (i in 1:200)
{
  amostras_1[,i] = sample(universo_1, size = 150)
}

dim(amostras_1)

summary(amostras_1)

medias_1 <- apply(amostras_1, 2, mean)

hist(medias_1, col = "lightpink", main = "Histograma das médias amostrais (Universo 1)", xlab = "Médias", 
     ylab = "Frequência", probability = TRUE, xlim = c(4,6), ylim = c(0,2), nclass = 11)

curve(dnorm(x, mean = mean(medias_1), sd = sd(medias_1)),
      lwd = 2.5, add = TRUE, yaxt = "n")

mean(medias_1)
mean(universo_1)

universo_2 <- runif(n = 5000000, min = 0, max = 10)

hist(universo_2, main = "Histograma de Variável Uniforme",
     col = "lightgreen", xlab = "Variável", ylab = "Frequência")
abline(v = mean(universo_2), col = "red", lwd = 2)

amostras_2 <- matrix(nrow = 150, ncol = 200)

# Amostragem

for (i in 1:200)
{
  amostras_2[,i] = sample(universo_2, size = 150)
}

medias_2 <- apply(amostras_2, 2, mean)

hist(medias_2, col = "lightpink", main = "Histograma das médias amostrais (Universo 2)", xlab = "Médias", 
     ylab = "Frequência", probability = TRUE, xlim = c(4,6), ylim = c(0,2), nclass = 11)

curve(dnorm(x, mean = mean(medias_2), sd = sd(medias_2)),
      lwd = 2.5, add = TRUE, yaxt = "n")

mean(medias_2)
mean(universo_2)

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

# A amostra de n = 50
amostra <- sample(universo_1, size = 50)
# A média
mean(amostra)
intconf(amostra, 0.05)

# A amostra de n = 100
amostra <- sample(universo_1, size = 100)
# A média
mean(amostra)
intconf(amostra, 0.05)

# A amostra de n = 500
amostra <- sample(universo_1, size = 500)
# A média
mean(amostra)
intconf(amostra, 0.05)

# A amostra de n = 1000
amostra <- sample(universo_1, size = 1000)
# A média
mean(amostra)
intconf(amostra, 0.05)

# A amostra de n = 1500
amostra <- sample(universo_1, size = 1500)
# A média
mean(amostra)
intconf(amostra, 0.05)

# As 3 curvas com distintos graus de liberdade

curve(dt(x, 20), from = -4, to = 4, col = "black", main = "Distribuição t Student com distintos graus de liberdade",
      xlab = "Valores de t", ylab = "Densidade de probabilidades", lwd = 2)

curve(dt(x, 3), from = -4, to = 4, col = "blue", add = TRUE, lwd = 2)

curve(dt(x, 1), from = -4, to = 4, col = "red", add = TRUE, lwd = 2)

# legenda
legend(x = 2, y = 0.4, legend = c("20 g.l", "3 g.l", "1 g.l"), 
       col = c("black", "blue", "red"), bty = "n", lwd = 2, lty = 1)


# A curva de 20 g.l. acotada em 95%
curve(dt(x, 20), from = -4, to = 4, col = "black", main = "Distribuição t Student com 20 graus de liberdade",
      xlab = "Valores de t", ylab = "Densidade de probabilidades", lwd = 2)

abline(v = qt(0.025, 20), col = "darkred", lwd = 2, lty = 3)
abline(v = qt(0.975, 20), col = "darkred", lwd = 2, lty = 3)