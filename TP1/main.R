x <- matrix(
       c(8.5, 1.5, 3.5, 5, 2, 6.5, 9.5, 1.5, 8.5, 2.5, 3, 6.5, 9, 2.5, 2, 5.5),
       nrow = 8, ncol = 2, byrow = T)

n <- dim(X)[1]
p <- dim(X)[2]

# center x by columns
x <- apply(x, 2, function(col) { col - mean(col) })

# compute the covariance matrix
sigma <- 1/n * t(x) %*% x

# compute the eigen values and vectors
eigens <- eigen(sigma)

# compute the percentage of explained inertia
cat(paste('first factorial axis',
          100 * eigens$values[1] / sum(eigens$values), '%\n'))
cat(paste('second factorial axis',
          100 * eigens$values[2] / sum(eigens$values), '%\n'))

# compute the principal components
comps <- x %*% eigens$vectors

# plot the principal components
pngname <- 'principalComponents.png'
png(pngname, width = 500, height = 500)

plot(comps, asp = 1,
     xlab = 'First factorial axis', ylab = 'Second factorial axis',
     main = 'Principal components analysis')

dev.off()
cat(paste(pngname, 'sauvegardee\n'))

dSquared <- as.matrix(dist(X)^2)
#dSquared <- matrix(0, nrow = dim(X)[1], ncol = dim(X)[1])
#for (i in 1:(dim(X)[1] - 1)) {
#    for (j in 1:(dim(X)[1] - 1)) {
#        dSquared[i + 1, j] <-
#            (X[i, j] - X[i + 1, j])^2 + (X[i, j + 1] - X[i + 1, j + 1])^2
#    }
#}

# dot product matrices
w1 <- x %*% t(x)
qn <- diag(1, n) - 1/n * matrix(1, ncol = n, nrow = n)
w2 <- -1/2 * qn %*% dSquared %*% qn
stopifnot(all.equal(w1, w2))

eigenValues <- eigen(w1)$values
moreThan0 <- function(x) { return(abs(x) > .Machine$double.eps / 10) }
stopifnot(moreThan0(eigenValues))
