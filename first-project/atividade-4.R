# 1. Ler e explorar os dados

View(tennis)

str(tennis)

summary(tennis)

# 2. Calcular o estatístico de contraste (você pode selecionar diferença de médias ou medianas)

tennis$dif_motora <- tennis$pos_test - tennis$pre_test

medianas <- tapply(tennis$dif_motora, tennis$grupo, median, na.rm=TRUE)
medianas

# 3. Explorar graficamente os dados

vioplot::vioplot(dif_motora ~ grupo, 
                 data = tennis,
                 ylab = "Diferença Motora", 
                 xlab = "Grupo",
                 col = c("lightgreen", "brown"))

# 4. Testar a hipótese por Bootstrap (Você tem liberdade de selecionar o valor de B)

dif_medianas <- abs(medianas[[1]] - medianas[[2]])

dif_medianas

amostra_bot <- sample(tennis$dif_motora, size = nrow(tennis), replace = TRUE)

medianas_bot <- tapply(amostra_bot, tennis$grupo, median, na.rm=TRUE)

(abs(medianas_bot[[1]] - medianas_bot[[2]]))

n <- nrow(tennis)
B <- 10000
Var <- tennis$dif_motora
Grupo <- tennis$grupo

set.seed(123)

boot_samples <- matrix( sample(Var, size = n*B, replace = TRUE),
                        nrow = n, ncol = B)

dim(boot_samples)

str(boot_samples)

boot_dif <- numeric(length = B)

for (i in 1:B) {
  mediana_bot <- tapply(boot_samples[,i], Grupo, median, na.rm=TRUE)
  
  boot_dif[i] <- (abs(mediana_bot[[1]] - mediana_bot[[2]]))
}

plot(density(boot_dif), lwd = 2,
     xlab = "Diferença de medianas", ylab = "Densidade probabilística",
     main = "Distribuição Bootstrap do Estatístico de contraste", las = 1)

points(dif_medianas, 0, cex = 2, pch = 19, col = "darkred")

dif_medianas

# 5. Comparar seus resultados com um teste t de Student

teste_t <- t.test(dif_motora ~ grupo, data = tennis)

teste_t