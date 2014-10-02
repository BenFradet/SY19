aftd <- function(d) {
    dMat <- as.matrix(d)
    dSquared <- dMat ^ 2

    n <- dim(dMat)[1]

    # orthogonal projection matrix, column centering matrix
    qn <- diag(1, n) - 1/n * matrix(1, ncol = n, nrow = n)

    # dot products matrix
    w <- -1/2 * qn %*% dSquared %*% qn

    # diagonalization of w
    eigenW <- eigen(1/n * w)
    v <- sqrt(n) * eigenW$vectors
    l <- diag(eigenW$values)

    res <- NULL

    # computes the percentage of explained inertia by the first two principal
    # components
    res$quality <- 100 * sum(diag(l)[c(1,2)]) / sum(diag(l)[diag(l) > 0])

    # computes the principal components
    res$points <- (v %*% sqrt(l))[, c(1,2)]

    return(res)
}
