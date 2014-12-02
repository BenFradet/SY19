library(nnet)
library(neuralnet)

source('plotnn.R')

p1 <- 0.25
p2 <- 0.25
p3 <- 0.25
p4 <- 0.25

n <- 200

s1 <- c(1, 2)
s2 <- c(2, 1)
s3 <- c(1.5, 2)
s4 <- c(1, 1)
s <- rbind(s1, s2, s3, s4)

m1 <- c(4, 6)
m2 <- c(6, 1)
m3 <- c(-4, -4)
m4 <- c(0, 0)
m <- rbind(m1, m2, m3, m4)

c <- sample(c(1, 2, 3, 4), size = n, prob = c(p1, p2, p3, p4), replace = TRUE)
x <- cbind(rnorm(n, m[c, 1], s[c, 1]), rnorm(n, m[c, 2], s[c, 2]))

color <- rep('red', n)
color[c == 2] <- 'blue'
color[c == 3] <- 'green'
color[c == 4] <- 'yellow'

# Bayes border
len <- 50

xp <- seq(min(x[, 1]), max(x[, 1]), length = len)
yp <- seq(min(x[, 2]), max(x[, 2]), length = len)

grid <- expand.grid(z1 = xp, x2 = yp)

Z <- p1 * dnorm(grid[, 1], m[1, 1], s[1, 1]) *
    dnorm(grid[, 2], m[1, 2], s[1, 2])
Z <- cbind(Z, p2 * dnorm(grid[, 1], m[2, 1], s[2, 1]) *
           dnorm(grid[, 2], m[2, 2], s[2, 2]))
Z <- cbind(Z, p3 * dnorm(grid[, 1], m[3, 1], s[3, 1]) *
           dnorm(grid[, 2], m[3, 2], s[3, 2]))
Z <- cbind(Z, p4 * dnorm(grid[, 1], m[4, 1], s[4, 1]) *
           dnorm(grid[, 2], m[4, 2], s[4, 2]))

pngName <- 'bayes.png'
png(pngName)
plot(x, col = color)
zp <- Z[, 4] - pmax(Z[, 1], Z[, 3], Z[, 2])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z[, 1] - pmax(Z[, 2], Z[, 3], Z[, 4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
zp <- Z[, 2] - pmax(Z[, 1], Z[, 3], Z[, 4])
contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
dev.off()
cat(pngName, 'sauvegardee\n')

# neural net
t <- class.ind(color)

# viz
neuralnetData <- as.data.frame(x)
neuralnetData <- cbind(x, c)
colnames(neuralnetData) <- c('x1', 'x2', 'color')
neuralnetModel <- neuralnet(color ~ x1 + x2, data = neuralnetData, hidden = 5)
pngName <- 'neuralnetViz.png'
png(pngName)
plot(neuralnetModel)
dev.off()
cat(pngName, 'sauvegardee\n')

proba <- c()
set.seed(1)
for (i in 1:10) {
    model <-
        nnet(x, t, size = i, decay = 0, softmax = T, maxit = 500, trace = F)

    T <- predict(model, x)
    classT <- round(T)
    count <- 0
    for (j in 1:n) {
        if (!all(classT[j, ] == t[j, ])) {
            count <- count + 1
        }
    }
    proba[i] <- count / length(classT)
    cat('proba:', proba[i], '\n')

    Z <- predict(model, grid)

    pngName <- paste('nnet', i, 'HiddenNeurones.png', sep = '')
    png(pngName)
    plot(x, col = color)
    zp <- Z[, 4] - pmax(Z[, 1], Z[, 3], Z[, 2])
    contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
    zp <- Z[, 1] - pmax(Z[, 2], Z[, 3], Z[, 4])
    contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
    zp <- Z[, 2] - pmax(Z[, 1], Z[, 3], Z[, 4])
    contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
    dev.off()
    cat(pngName, 'sauvegardee\n')
}

for (i in 1:6) {
    decay <- 0.0001 * 10 ^ i
    model <-
        nnet(x, t, size = 5, decay = decay, softmax = T, maxit = 500, trace = F)

    T <- predict(model, x)
    classT <- round(T)

    Z <- predict(model, grid)

    pngName <- paste('nnet', decay, 'Decay.png', sep = '')
    png(pngName)
    plot(x, col = color)
    zp <- Z[, 4] - pmax(Z[, 1], Z[, 3], Z[, 2])
    contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
    zp <- Z[, 1] - pmax(Z[, 2], Z[, 3], Z[, 4])
    contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
    zp <- Z[, 2] - pmax(Z[, 1], Z[, 3], Z[, 4])
    contour(xp, yp, matrix(zp, len), add = TRUE, levels = 0, drawlabels = FALSE)
    dev.off()
    cat(pngName, 'sauvegardee\n')
}

pngName <- 'probaSize.png'
png(pngName)
plot(c(1:10), proba,
     type = 'b',
     main = paste('Probabilite d\'erreur empirique en fonction\n',
                  'du nombre de neurones dans la couche cachee', sep = ''),
     xlab = 'Nombre de neurones dans la couche cachee',
     ylab = 'Probabilite d\'erreur empirique')
dev.off()
cat(pngName, 'sauvegardee\n')
