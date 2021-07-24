install.packages("datasets")

require(datasets)

data(chickwts)
View(chickwts)

# Separar os dados da dieta baseada em soja
soybean <- subset(chickwts, feed == "soybean")
View(soybean)

# Separar os dados da dieta baseada em farinha de carne
meatmeal <- subset(chickwts, feed == "meatmeal")
View(meatmeal)

# juntar tudo em um data frame único
chick <- rbind(soybean, meatmeal)

# usar dois fatores
chick$feed <- factor(chick$feed, levels = c("soybean", "meatmeal"))
View(chick)

medianas <- tapply(chick$weight, chick$feed, median)

medianas

vioplot::vioplot(weight ~ feed, data = chick,
                 ylab = "Ganho de massa", 
                 xlab = "Dieta",
                 col = c("lightgreen", "brown"))

dif_medianas <- abs(medianas[[1]] - medianas[[2]])
dif_medianas

# amostragem com substituição
amostra_bot <- sample(chick$weight, size = nrow(chick), replace = TRUE)

medianas_bot <- tapply(amostra_bot, chick$feed, median)

(abs(medianas_bot[[1]] - medianas_bot[[2]]))

n <- nrow(chick)
B <- 10000
Var <- chick$weight
Grupo <- chick$feed

set.seed(123)

# B amostras
boot_samples <- matrix( sample(Var, size = n*B, replace = TRUE),
                        nrow = n, ncol = B)

dim(boot_samples)

str(boot_samples)

boot_dif <- numeric(length = B)

# diferenças de medias para as 10000 amostras usando o loop 
for (i in 1:B) {
  mediana_bot <- tapply(boot_samples[,i], Grupo, median)
  
  boot_dif[i] <- (abs(mediana_bot[[1]] - mediana_bot[[2]]))
}

# gráfico da densidade probabilística
plot(density(boot_dif), lwd = 2,
     xlab = "Diferença de medianas", ylab = "Densidade probabilística",
     main = "Distribuição Bootstrap do Estatístico de contraste", las = 1)

# diferença de medianas calculada
points(dif_medianas, 0, cex = 2, pch = 19, col = "darkred")

p_valor <- (sum(boot_dif >= dif_medianas))/B

p_valor