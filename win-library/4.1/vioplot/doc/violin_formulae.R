## -----------------------------------------------------------------------------
library("vioplot")

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  data(iris)
#  boxplot(Sepal.Length~Species, data = iris)

## ---- message=FALSE, echo=FALSE-----------------------------------------------
data(iris)
boxplot(Sepal.Length~Species, data = iris, main = "Sepal Length")

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  devtools::install_version("vioplot", version = "0.2")
#  library("vioplot")
#  vioplot(Sepal.Length~Species, data = iris)

## ---- message=FALSE, eval=FALSE-----------------------------------------------
#  vioplot(Sepal.Length~Species, data = iris)

## ---- message=FALSE, echo=FALSE-----------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length", col="magenta")

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length")

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length", col="lightblue")

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length", col=c("lightgreen", "lightblue", "palevioletred"))
legend("topleft", legend=c("setosa", "versicolor", "virginica"), fill=c("lightgreen", "lightblue", "palevioletred"), cex = 0.5)

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length", col="lightblue", border="royalblue")

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length", rectCol="palevioletred", lineCol="violetred")

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length", colMed="violet")

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length", col="lightblue", border="royalblue", rectCol="palevioletred", lineCol="violetred", colMed="violet")

## -----------------------------------------------------------------------------
vioplot(Sepal.Length~Species, data = iris, main="Sepal Length", col=c("lightgreen", "lightblue", "palevioletred"), border=c("darkolivegreen4", "royalblue4", "violetred4"), rectCol=c("forestgreen", "blue", "palevioletred3"), lineCol=c("darkolivegreen", "royalblue", "violetred4"), colMed=c("green", "cyan", "magenta"), pchMed=c(15, 17, 19))

