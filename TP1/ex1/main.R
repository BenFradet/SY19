x <- matrix(
       c(8.5, 1.5, 3.5, 5, 2, 6.5, 9.5, 1.5, 8.5, 2.5, 3, 6.5, 9, 2.5, 2, 5.5),
       nrow = 8, ncol = 2, byrow = T)

n <- dim(x)[1]
p <- dim(x)[2]

# centers x by columns
x <- apply(x, 2, function(col) { col - mean(col) })

# computes the covariance matrix
sigma <- 1/n * t(x) %*% x

# computes the eigen values and vectors
eigenSigma <- eigen(sigma)
print(eigenSigma$vectors)

# computes the percentage of explained inertia
cat(paste('first factorial axis',
          100 * eigenSigma$values[1] / sum(eigenSigma$values), '%\n'))
cat(paste('second factorial axis',
          100 * eigenSigma$values[2] / sum(eigenSigma$values), '%\n'))

# computes the principal components
comps <- x %*% eigenSigma$vectors
print(comps)

# plots the principal components
pngName <- 'pca.png'
png(pngName)
plot(x[, 1], x[, 2],
     asp = 1,
     xlab = 'Premier axe factoriel', ylab = 'Second axe factoriel',
     main = paste('Comparaison de la représentation de l\'ACP dans\n',
                  'le premier plan factoriel et le nuage original'),
     col = 'red',
     pch = 1,
     xlim = range(comps[, 1]))
points(comps,
       col = ' blue',
       pch = 2)
legend(1.5, 3.5,
       c('Nuage original', 'ACP'),
       col = c('red', 'blue'),
       pch = c(1,2))
dev.off()
cat(paste(pngName, 'sauvegardee\n'))

# computes the sqaured euclidean distances for X
dSquared <- as.matrix(dist(x)^2)
cat('D^2\n')
print(dSquared)
cat('\n\n')

# dot product matrices
w1 <- x %*% t(x)
qn <- diag(1, n) - 1/n * matrix(1, ncol = n, nrow = n)
w2 <- -1/2 * qn %*% dSquared %*% qn
stopifnot(all.equal(w1, w2))
cat('W\n')
print(w1)
cat('\n\n')

# check that w1 and w2 are positive semi-definite
eigenW <- eigen(1/n * w1)
moreThan0 <- function(x) { return(x > 0 || abs(x) < .Machine$double.eps) }
stopifnot(moreThan0(eigenW$values))

v <- eigenW$vectors
l <- diag(eigenW$values)
cat('V\n')
print(v)
cat('\n\n')

cat('L\n')
print(l)
cat('\n\n')
components <- v %*% sqrt(l)

pngName2 <- 'aftd.png'
png(pngName2)

plot(x[,1], x[,2],
     asp = 1,
     xlab = 'Premier axe factoriel', ylab = 'Second axe factoriel',
     main = paste('Comparaison entre la representation de l\'AFTD \net le',
                  'nuage original'),
     col = 'red',
     pch = 1)
points(components[,1], components[,2],
       col = ' blue',
       pch = 2)
legend(1.5, 3,
       c('Nuage original', 'AFTD'),
       col = c('red', 'blue'),
       pch = c(1,2))

dev.off()
cat(paste(pngName2, 'sauvegardee\n'))
