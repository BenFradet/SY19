library(MASS)
library(nnet)

source('cv.R')

n <- dim(crabs)[1]
k <- 5
nk <- n / k
hidden <- 6
decay <- 0.001

x <- cbind(scale(crabs$FL), scale(crabs$RW))

# grid
len <- 50
xp <- seq(min(x[, 1]), max(x[, 1]), length = len)
yp <- seq(min(x[, 2]), max(x[, 2]), length = len)
grid <- expand.grid(z1 = xp, z2 = yp)

# split crabs according to their sex
t <- class.ind(crabs$sex)
shuffle <- cbind(x, t, crabs$sex)
shuffle <- shuffle[sample(nrow(shuffle)), ]
x <- shuffle[, c(1, 2)]
t <- shuffle[, c(3, 4)]
sex <- shuffle[, 5]
color <- rep('red', n)
color[crabs$sex == 'M'] <- 'blue'

# neural net training regarding their sex with CV
cv <- runCV(x, t, k, nk, hidden, decay)
cat('proba with sex nnet:', cv$error, '\n')
Z <- predict(cv$model, grid)
pngName <- 'nnetCrabsSex.png'
png(pngName)
plot(x, col = color)
zp <- Z[, 1] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
zp <- Z[, 2] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')

# comparison with lda
appRange <- c(1:(4 * nk))
xApp <- x[appRange, ]
tApp <- sex[appRange]
xTest <- x[-appRange, ]
tTest <- sex[-appRange]
lda <- lda(xApp, tApp, prior = c(1/2, 1/2))
tPredicted <- predict(lda, xTest)$class
cat('proba with sex lda:', length(which(tTest != tPredicted)) / nk, '\n')
Z <- predict(lda, grid)
pngName <- 'ldaCrabsSex.png'
png(pngName)
plot(x, col = color)
zp <- Z$posterior[, 1] - Z$posterior[, 2]
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')

x <- cbind(scale(crabs$FL), scale(crabs$RW))

# split according to their color
t <- class.ind(crabs$sp)
shuffle <- cbind(x, t, crabs$sp)
shuffle <- shuffle[sample(nrow(shuffle)), ]
x <- shuffle[, c(1, 2)]
t <- shuffle[, c(3, 4)]
sp <- shuffle[, 5]
color <- rep('red', n)
color[crabs$sp == 'B'] <- 'blue'

# neural net training regarding their species with CV
hidden <- 5
cv <- runCV(x, t, k, nk, hidden, decay)
cat('proba with species nnet:', cv$error, '\n')
Z <- predict(cv$model, grid)
pngName <- 'nnetCrabsSpecies.png'
png(pngName)
plot(x, col = color)
zp <- Z[, 1] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
zp <- Z[, 2] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')

# comparison with lda
xApp <- x[appRange, ]
tApp <- sp[appRange]
xTest <- x[-appRange, ]
tTest <- sp[-appRange]
lda <- lda(xApp, tApp, prior = c(1/2, 1/2))
tPredicted <- predict(lda, xTest)$class
cat('proba with species lda:', length(which(tTest != tPredicted)) / nk, '\n')
Z <- predict(lda, grid)
pngName <- 'ldaCrabsSpecies.png'
png(pngName)
plot(x, col = color)
zp <- Z$posterior[, 1] - Z$posterior[, 2]
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')
