## -----------------------------------------------------------------------------
library("vioplot")

## ---- message=FALSE-----------------------------------------------------------
data(iris)
boxplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))
vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))

## ---- echo=FALSE, message=FALSE-----------------------------------------------
par(mar=rep(1,4))

## -----------------------------------------------------------------------------
par(mfrow=c(3, 1))
par(mar=rep(2, 4))
plot(density(iris$Sepal.Length[iris$Species=="setosa"]), main="Sepal Length: setosa", col="green")
plot(density(iris$Sepal.Length[iris$Species=="versicolor"]), main="Sepal Length: versicolor", col="blue")
plot(density(iris$Sepal.Length[iris$Species=="virginica"]), main="Sepal Length: virginica", col="palevioletred4")
par(mfrow=c(1, 1))

## ---- echo=FALSE, message=FALSE-----------------------------------------------
par(mar=c(5, 4, 4, 2) + 0.1)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
par(mar=rep(2,4))

## -----------------------------------------------------------------------------
par(mfrow=c(3, 1))
par(mar=rep(2, 4))
xaxis <- c(3, 9)
yaxis <- c(0, 1.25)
plot(density(iris$Sepal.Length[iris$Species=="setosa"]), main="Sepal Length: setosa", col="green", xlim=xaxis, ylim=yaxis)
plot(density(iris$Sepal.Length[iris$Species=="versicolor"]), main="Sepal Length: versicolor", col="blue", xlim=xaxis, ylim=yaxis)
plot(density(iris$Sepal.Length[iris$Species=="virginica"]), main="Sepal Length: virginica", col="palevioletred4", xlim=xaxis, ylim=yaxis)
par(mfrow=c(1, 1))

## ---- echo=FALSE, message=FALSE-----------------------------------------------
par(mar=c(5, 4, 4, 2) + 0.1)

## -----------------------------------------------------------------------------
par(mfrow=c(1, 1))
xaxis <- c(3, 9)
yaxis <- c(0, 1.25)
plot(density(iris$Sepal.Length[iris$Species=="setosa"]), main="Sepal Length", col="green", xlim=xaxis, ylim=yaxis)
lines(density(iris$Sepal.Length[iris$Species=="versicolor"]), col="blue")
lines(density(iris$Sepal.Length[iris$Species=="virginica"]), col="palevioletred4")
legend("topright", fill=c("green", "blue", "palevioletred4"), legend=levels(iris$Species), cex=0.5)

## -----------------------------------------------------------------------------
vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", areaEqual = T)

## ---- echo=FALSE, message=FALSE-----------------------------------------------
par(mar=rep(2, 4))

## -----------------------------------------------------------------------------
par(mfrow=c(2,1))
par(mar=rep(2, 4))
vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Width)", areaEqual = F)
vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Area)", areaEqual = T)
par(mfrow=c(1,1))

## ---- echo=FALSE, message=FALSE-----------------------------------------------
par(mar=c(5, 4, 4, 2) + 0.1)

## -----------------------------------------------------------------------------
vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Area)", areaEqual = T, col=c("lightgreen", "lightblue", "palevioletred"), rectCol=c("green", "blue", "palevioletred3"), lineCol=c("darkolivegreen", "royalblue", "violetred4"), border=c("darkolivegreen4", "royalblue4", "violetred4"))

## -----------------------------------------------------------------------------
vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Area)", areaEqual = T, col=c("lightgreen", "lightblue", "palevioletred"), rectCol=c("green", "blue", "palevioletred3"), lineCol=c("darkolivegreen", "royalblue", "violetred4"), border=c("darkolivegreen4", "royalblue4", "violetred4"), wex=1.25)

## -----------------------------------------------------------------------------
vioplot(rnorm(200, 3, 0.5), rpois(200, 2.5),  rbinom(100, 10, 0.4), rlnorm(200, 0, 0.5), rnbinom(200, 10, 0.9), rlogis(20, 0, 0.5), areaEqual = F, main="Equal Width", xlab="distribution", ylab="data value", names=c("normal", "poisson", "binomial", "log-normal", "neg-binomial", "logistic"))
vioplot(rnorm(200, 3, 0.5), rpois(200, 2.5),  rbinom(100, 10, 0.4), rlnorm(200, 0, 0.5), rnbinom(200, 10, 0.9), rlogis(20, 0, 0.5), areaEqual = T, main="Equal Area", xlab="distribution", ylab="data value", names=c("normal", "poisson", "binomial", "log-normal", "neg-binomial", "logistic"))

