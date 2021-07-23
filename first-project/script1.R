###############################################################################
#Prática 1
###############################################################################

# o link do arquivo

link_metad <- "https://raw.githubusercontent.com/gflorezm/SemanaBiologiaUFABC_2021/main/Dados/metadados.txt"

# Salvamos na pasta do nosso Projeto de trabalho

download.file(link_metad, "metadados.txt")

# Abrimos o arquivo

file.edit("metadados.txt")

# O link da fonte

link_tennis <- "https://raw.githubusercontent.com/gflorezm/SemanaBiologiaUFABC_2021/main/Dados/tennis.csv"

# Criamos um objeto com o dados

tennis <- read.csv(link_tennis, sep = ";")

head(tennis)

tail(tennis)

View(tennis)

summary(tennis)

str(tennis)

unique(tennis$pos_test)

tennis$pos_test[tennis$pos_test == "Nao"] <- NA

tennis$pos_test <- as.integer(tennis$pos_test)

unique(tennis$grupo)

# quantos erros há? 

sum(tennis$grupo == "tratamento")

# Substituímos pela forma correta

tennis$grupo[tennis$grupo == "tratamento"] <- "Tratamento"

# Transformamos a variável num Fator

tennis$grupo <- factor(tennis$grupo, levels = c("Controle", "Tratamento"))

write.csv(tennis, file = "tennis.csv", row.names = FALSE)

###############################################################################
#Atividade 1
###############################################################################

lituratus <- read.csv(
  "https://raw.githubusercontent.com/gflorezm/SemanaBiologiaUFABC_2021/main/Dados/a.lituratus.csv", 
  sep = ",")

summary(lituratus)

str(lituratus)

unique(lituratus$Forearm)

unique(lituratus$body_mass)

unique(lituratus$Habitat)

lituratus$Forearm[lituratus$Forearm == 0] <- NA

lituratus$Forearm <- as.numeric(lituratus$Forearm)

lituratus$body_mass[lituratus$body_mass == 0] <- NA

lituratus$body_mass <- as.integer(lituratus$body_mass)

sum(lituratus$Habitat == "forest")

lituratus$Habitat[lituratus$Habitat == "forest"] <- "Forest"

lituratus$Habitat <- factor(lituratus$Habitat, levels = c("Forest", "Urban"))

write.csv(lituratus, file = "lituratus.csv", row.names = FALSE)

###############################################################################
#Prática 2
###############################################################################

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

# Primeiro criamos uma matriz para salvar essas amostras que faremos no loop
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
  
  # parâmetros
  media <- mean(x, na.rm = TRUE)
  dp <- sd(x, na.rm = TRUE)
  n <- length(x)
  
  # nomes dos intervalos
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

###############################################################################
#Prática 3
###############################################################################

load("suplemento.RData")

View(suplemento)

str(suplemento)

summary(suplemento)

suplemento$dif_massa <- suplemento$massa_final - suplemento$massa_inicial

medias <- tapply(suplemento$dif_massa, suplemento$grupo, mean)

medias # para chamar a saída do objeto

# HISTOGRAMA

# primeiro para o grupo controle
hist(suplemento$dif_massa[suplemento$grupo == "Controle"], # os valores do grupo Controle
     xlab = "diferença de massa", ylab = "Frequência", 
     main = " Histograma da diferença de massa por grupo",
     probability = TRUE, col = "lightpink", breaks = 10,
     xlim = c(-4,6), ylim = c(0, 0.6))

# adicionamos os do tratamento
hist(suplemento$dif_massa[suplemento$grupo == "Tratamento"], # os valores do grupo Tratamento
     probability = TRUE, col = "lightgreen", breaks = 10, add = TRUE)

# Eu gosto dos gráficos fechados então...
box()

# adicionamos umas linhas verticais nas médias
abline(v = medias[1], col = "darkred", lwd = 2.5, lty = 3) # grupo controle
abline(v = medias[2], col = "darkgreen", lwd = 2.5, lty = 3) # tratamento

# adicionamos a legenda
legend(4, 0.6, legend = c("Controle", "Tratamento"), bty = "n",
       col = c("lightpink", "lightgreen"), pch = 15, pt.cex = 3)

# BOXPLOT

boxplot(dif_massa ~ grupo, data = suplemento, 
        "ylab" = "diferença de massa", xlab = "Grupo",
        col = c("lightpink", "lightgreen"), 
        pch = 16
        )#,notch = TRUE)

# VIOLIN PLOT

# Para instalar o pacote use

install.packages("vioplot")

# Antes de usar o pacote precisa chamá-lo

require("vioplot")

# Agora vamos construir o gráfico

vioplot(dif_massa ~ grupo, 
        data = suplemento, 
        ylab = "diferença de massa", 
        xlab = "Grupo",
        col = c("lightpink", "lightgreen"))

intconf <- function(x, alpha) {
  
  # parâmetros
  media <- mean(x, na.rm = TRUE)
  dp <- sd(x, na.rm = TRUE)
  n <- length(x)
  
  # nomes dos intervalos
  nomes <- c(paste0(alpha/2*100, "%"), paste0((1-alpha/2)*100, "%"))
  
  resultado <- c(media + (qt(alpha/2, n-1) * dp / sqrt(n)),
                 media + (qt(1-alpha/2, n-1) * dp / sqrt(n)))
  names(resultado) <- nomes
  resultado
}

intervalos <- tapply(suplemento$dif_massa, suplemento$grupo, intconf, alpha = 0.01)
intervalos # para chamar o resultado

# Criamos um gráfico vazio
plot("n", xlim = c(0.5, 2.5), ylim = c(-1,4), xaxt = "n", 
     xlab = " Grupo", ylab = "Diferênca de massa", cex.lab = 1.2)

# Adicionamos o eixo X
axis(side = 1, at = c(1,2), labels = c("Controle", "Tratamento"))

# Adicionamos as médias
points(y = medias, x = c(1,2), cex = 2, pch = 16)

# Adicionamos os intervalos para cada média
lines(y = (intervalos$Controle), x = c(1,1), lwd = 3) # Controle
lines(y = (intervalos$Tratamento), x = c(2,2), lwd = 3) # Tratamento

# Adicionamos uma linha horizontal no zero
abline(h = 0, lwd = 3, lty = 2, col = "red")

teste_t <- t.test(dif_massa ~ grupo, data = suplemento)

teste_t # para chamar o resultado  

curve(dt(x, 78), from = -11, to = 11, col = "black", xlab = "Valores de t", 
      main = "Distribuição t Student da hipótese nula", 
      ylab = "Densidade probabilística", lwd = 2.5)

# adicionamos as cotas en alfa 0.01
abline(v = qt(1-0.01/2, 78), col = "darkred", lwd = 2.5, lty = 3)
abline(v = qt(0.01/2, 78), col = "darkred", lwd = 2.5, lty = 3)

# adicionamos o nosso valor de t
points(x = teste_t$statistic, y = 0, pch = 18, col = "darkblue", cex = 2)

##############################################################################
# ATIVIDADE 3
##############################################################################

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

# adicionamos o nosso valor de t
points(x = teste_t$statistic, y = 0, pch = 18, col = "darkblue", cex = 2)

###############################################################################
#Prática 4
###############################################################################

# Se não tiver o pacote pode instalá-lo
install.packages("datasets")

# Chamar o pacote ao seu ambiente
require(datasets)

# Chamar o conjunto de dados
data(chickwts)
View(chickwts)

# Separar os dados da dieta baseada em soja
soybean <- subset(chickwts, feed == "soybean")
View(soybean)

# Separar os dados da dieta baseada em farinha de carne
meatmeal <- subset(chickwts, feed == "meatmeal")
View(meatmeal)

# Juntamos tudo em um data frame único
chick <- rbind(soybean, meatmeal)

# Redefinimos a variável feed com os dois níveis que temos (originalmente tem mais)
chick$feed <- factor(chick$feed, levels = c("soybean", "meatmeal"))
View(chick)

medianas <- tapply(chick$weight, chick$feed, median)

medianas # para chamar a saída do objeto

vioplot::vioplot(weight ~ feed, data = chick,
                 ylab = "Ganho de massa", 
                 xlab = "Dieta",
                 col = c("lightgreen", "brown"))

dif_medianas <- abs(medianas[[1]] - medianas[[2]])
dif_medianas

# primeiro fazemos uma amostragem com substituição
amostra_bot <- sample(chick$weight, size = nrow(chick), replace = TRUE)

# calculamos as medianas
medianas_bot <- tapply(amostra_bot, chick$feed, median)

# calculamos a diferença das medianas
(abs(medianas_bot[[1]] - medianas_bot[[2]]))

# Parámetros

n <- nrow(chick)
B <- 10000
Var <- chick$weight
Grupo <- chick$feed

# Plantamos a semente para garantir a reprodutibilidade
set.seed(123)

# Criamos as B amostras
boot_samples <- matrix( sample(Var, size = n*B, replace = TRUE),
                        nrow = n, ncol = B)

# exploramos um pouco
dim(boot_samples)

str(boot_samples)

boot_dif <- numeric(length = B)

# Calculamos as diferenças de medias para as 10000 amostras usando o loop 
for (i in 1:B) {
  mediana_bot <- tapply(boot_samples[,i], Grupo, median)
  
  boot_dif[i] <- (abs(mediana_bot[[1]] - mediana_bot[[2]]))
}

# graficamos a densidade probabilística
plot(density(boot_dif), lwd = 2,
     xlab = "Diferença de medianas", ylab = "Densidade probabilística",
     main = "Distribuição Bootstrap do Estatístico de contraste", las = 1)

# adicionamos a diferença de medianas calculada (ponto vermelho)
points(dif_medianas, 0, cex = 2, pch = 19, col = "darkred")

p_valor <- (sum(boot_dif >= dif_medianas))/B

p_valor # chamamos

################### ATIVIDADE 4 #############################################

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
