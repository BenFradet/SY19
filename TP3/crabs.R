library(MASS)
library(nnet)

source('runCV.R')

n <- nrow(crabs)
crabs <- crabs[sample(n), ]
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
color <- rep('red', n)
color[crabs$sex == 'M'] <- 'blue'

# neural net training regarding their sex with CV
cv <- runCV(x, t, k, nk, hidden, decay)
cat('proba with sex nnet:', cv$error, '\n')
Z <- predict(cv$model, grid)
pngName <- 'nnetCrabsSex.png'
png(pngName)
plot(x, col = color,
     xlab = 'Frontal lobe size (mm)',
     ylab = 'Rear width (mm)',
     main = 'Frontieres de decision issues de nnet')
zp <- Z[, 1] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
zp <- Z[, 2] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')

# comparison with lda
appRange <- c(1:(4 * nk))
xApp <- x[appRange, ]
tApp <- crabs$sex[appRange]
xTest <- x[-appRange, ]
tTest <- crabs$sex[-appRange]
lda <- lda(xApp, tApp, prior = c(1/2, 1/2))
tPredicted <- predict(lda, xTest)$class
cat('proba with sex lda:', length(which(tTest != tPredicted)) / nk, '\n')
Z <- predict(lda, grid)
pngName <- 'ldaCrabsSex.png'
png(pngName)
plot(x, col = color,
     xlab = 'Frontal lobe size (mm)',
     ylab = 'Rear width (mm)',
     main = 'Frontieres de decision issues de lda')
zp <- Z$posterior[, 1] - Z$posterior[, 2]
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')

# split according to their color
t <- class.ind(crabs$sp)
color <- rep('red', n)
color[crabs$sp == 'B'] <- 'blue'

# neural net training regarding their species with CV
hidden <- 5
cv <- runCV(x, t, k, nk, hidden, decay)
cat('proba with species nnet:', cv$error, '\n')
Z <- predict(cv$model, grid)
pngName <- 'nnetCrabsSpecies.png'
png(pngName)
plot(x, col = color,
     xlab = 'Frontal lobe size (mm)',
     ylab = 'Rear width (mm)',
     main = 'Frontieres de decision issues de nnet')
zp <- Z[, 1] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
zp <- Z[, 2] - pmax(Z[, 1], Z[, 2])
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')

# comparison with lda
xApp <- x[appRange, ]
tApp <- crabs$sp[appRange]
xTest <- x[-appRange, ]
tTest <- crabs$sp[-appRange]
lda <- lda(xApp, tApp, prior = c(1/2, 1/2))
tPredicted <- predict(lda, xTest)$class
cat('proba with species lda:', length(which(tTest != tPredicted)) / nk, '\n')
Z <- predict(lda, grid)
pngName <- 'ldaCrabsSpecies.png'
png(pngName)
plot(x, col = color,
     xlab = 'Frontal lobe size (mm)',
     ylab = 'Rear width (mm)',
     main = 'Frontieres de decision issues de lda')
zp <- Z$posterior[, 1] - Z$posterior[, 2]
contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = F)
dev.off()
cat(pngName, 'sauvegardee\n')
