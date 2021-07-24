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