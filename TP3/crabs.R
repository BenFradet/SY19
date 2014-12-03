library(MASS)
library(nnet)

source('cv.R')

n <- dim(crabs)[1]
k <- 5
nk <- n / k
hidden <- 6
decay <- 0.001

# split crabs according to their sex
x <- log(cbind(crabs$FL, crabs$RW))
t <- class.ind(crabs$sex)
shuffle <- cbind(x, t, crabs$sex)
shuffle <- shuffle[sample(nrow(shuffle)), ]
x <- shuffle[, c(1, 2)]
t <- shuffle[, c(3, 4)]
sex <- shuffle[, 5]
color <- rep('red', n)
color[crabs$sex == 'M'] <- 'blue'
plot(x, col = color)

# neural net training regarding their sex with CV
# pour les frontieres prend-on le modele de cv avec la plus petite proba?
cat('proba with sex nnet:', runCV(x, t, k, nk, hidden, decay), '\n')

# comparison with lda
appRange <- c(1:(4 * nk))
xApp <- x[appRange, ]
tApp <- sex[appRange]
xTest <- x[-appRange, ]
tTest <- sex[-appRange]
lda <- lda(xApp, tApp, prior = c(1/2, 1/2))
tPredicted <- predict(lda, xTest)$class
cat('proba with sex lda:', length(which(tTest != tPredicted)) / nk, '\n')

# split according to their color
x <- log(cbind(crabs$FL, crabs$RW))
t <- class.ind(crabs$sp)
shuffle <- cbind(x, t, crabs$sp)
shuffle <- shuffle[sample(nrow(shuffle)), ]
x <- shuffle[, c(1, 2)]
t <- shuffle[, c(3, 4)]
sp <- shuffle[, 5]
color <- rep('red', n)
color[crabs$sp == 'B'] <- 'blue'
plot(x, col = color)

# neural net training regarding their species with CV
hidden <- 5
cat('proba with species:', runCV(x, t, k, nk, hidden, decay), '\n')

# comparison with lda
xApp <- x[appRange, ]
tApp <- sp[appRange]
xTest <- x[-appRange, ]
tTest <- sp[-appRange]
lda <- lda(xApp, tApp, prior = c(1/2, 1/2))
tPredicted <- predict(lda, xTest)$class
cat('proba with species lda:', length(which(tTest != tPredicted)) / nk, '\n')
