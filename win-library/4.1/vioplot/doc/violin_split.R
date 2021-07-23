## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
library("vioplot")

## ---- message=FALSE-----------------------------------------------------------
data(iris)
summary(iris$Sepal.Width)
table(iris$Sepal.Width > mean(iris$Sepal.Width))
iris_large <- iris[iris$Sepal.Width > mean(iris$Sepal.Width), ]
iris_small <- iris[iris$Sepal.Width <= mean(iris$Sepal.Width), ]

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
boxplot(Sepal.Length~Species, data=iris, col="grey")

## ----  fig.align = 'center', fig.height = 6, fig.width = 6, fig.keep = 'last'----
{
  par(mfrow=c(2,1))
boxplot(Sepal.Length~Species, data=iris_small, col = "lightblue")
boxplot(Sepal.Length~Species, data=iris_large, col = "palevioletred")
par(mfrow=c(1,1))
}

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
vioplot(Sepal.Length~Species, data=iris)

## ----  fig.align = 'center', fig.height = 6, fig.width = 6, fig.keep = 'last'----
{
  par(mfrow=c(2,1))
vioplot(Sepal.Length~Species, data=iris_small, col = "lightblue", plotCentre = "line")
vioplot(Sepal.Length~Species, data=iris_large, col = "palevioletred", plotCentre = "line")
par(mfrow=c(1,1))
}

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
vioplot(Sepal.Length~Species, data=iris_large, col = "palevioletred", plotCentre = "line", side = "right")
vioplot(Sepal.Length~Species, data=iris_small, col = "lightblue", plotCentre = "line", side = "left", add = T)
title(xlab = "Species", ylab = "Sepal Length")
legend("topleft", fill = c("lightblue", "palevioletred"), legend = c("small", "large"), title = "Sepal Width")

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
vioplot(Sepal.Length~Species, data=iris_large, col = "palevioletred", plotCentre = "line", side = "right", xlab = "Iris species", ylab = "Length", main = "Sepals", names=paste("Iris", levels(iris$Species)))
vioplot(Sepal.Length~Species, data=iris_small, col = "lightblue", plotCentre = "line", side = "left", add = T)
legend("topleft", fill = c("lightblue", "palevioletred"), legend = c("small", "large"), title = "Width")

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
vioplot(Sepal.Length~Species, data=iris_large, col = "palevioletred", plotCentre = "line", side = "right")
vioplot(Sepal.Length~Species, data=iris_small, col = "lightblue", plotCentre = "line", side = "left", add = T, xlab = "Iris species", ylab = "Length", main = "Sepals", names=paste("Iris", levels(iris$Species)))
legend("topleft", fill = c("lightblue", "palevioletred"), legend = c("small", "large"), title = "Width")

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
vioplot(Sepal.Length~Species, data=iris_large, col = "palevioletred", plotCentre = "point", side = "right", pchMed = 21, colMed = "palevioletred4", colMed2 = "palevioletred2")
vioplot(Sepal.Length~Species, data=iris_small, col = "lightblue", plotCentre = "point", side = "left", pchMed = 21, colMed = "lightblue4", colMed2 = "lightblue2", add = T)
title(xlab = "Species", ylab = "Sepal Length")
legend("topleft", fill = c("lightblue", "palevioletred"), legend = c("small", "large"), title = "Sepal Width")

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
vioplot(Sepal.Length~Species, data=iris_large, col = "palevioletred", plotCentre = "point", side = "right", pchMed = 21, colMed = "palevioletred4", colMed2 = "palevioletred2")
vioplot(Sepal.Length~Species, data=iris_small, col = "lightblue", plotCentre = "point", side = "left", pchMed = 21, colMed = "lightblue4", colMed2 = "lightblue2", add = T)
points(1:length(levels(iris$Species)), as.numeric(sapply(levels(iris$Species), function(species) median(iris_large[grep(species, iris_large$Species),]$Sepal.Length))), pch = 21, col = "palevioletred4", bg = "palevioletred2")
title(xlab = "Species", ylab = "Sepal Length")
legend("topleft", fill = c("lightblue", "palevioletred"), legend = c("small", "large"), title = "Sepal Width")

## ---- fig.align = 'center', fig.height = 3, fig.width = 6, fig.keep = 'last'----
vioplot(Sepal.Length~Species, data=iris_large, col = "palevioletred", plotCentre = "line", side = "right", pchMed = 21, colMed = "palevioletred4", colMed2 = "palevioletred2")
vioplot(Sepal.Length~Species, data=iris_small, col = "lightblue", plotCentre = "line", side = "left", pchMed = 21, colMed = "lightblue4", colMed2 = "lightblue2", add = T)
points(1:length(levels(iris$Species)), as.numeric(sapply(levels(iris$Species), function(species) median(iris_large[grep(species, iris_large$Species),]$Sepal.Length))), pch = 21, col = "palevioletred4", bg = "palevioletred2")
points(1:length(levels(iris$Species)), as.numeric(sapply(levels(iris$Species), function(species) median(iris_small[grep(species, iris_small$Species),]$Sepal.Length))), pch = 21, col = "lightblue4", bg = "lightblue2")
title(xlab = "Species", ylab = "Sepal Length")
legend("topleft", fill = c("lightblue", "palevioletred"), legend = c("small", "large"), title = "Sepal Width")

