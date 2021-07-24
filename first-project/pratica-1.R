link_metad <- "https://raw.githubusercontent.com/gflorezm/SemanaBiologiaUFABC_2021/main/Dados/metadados.txt"

download.file(link_metad, "metadados.txt")

file.edit("metadados.txt")

link_tennis <- "https://raw.githubusercontent.com/gflorezm/SemanaBiologiaUFABC_2021/main/Dados/tennis.csv"

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

sum(tennis$grupo == "tratamento")

tennis$grupo[tennis$grupo == "tratamento"] <- "Tratamento"

tennis$grupo <- factor(tennis$grupo, levels = c("Controle", "Tratamento"))

write.csv(tennis, file = "tennis.csv", row.names = FALSE)