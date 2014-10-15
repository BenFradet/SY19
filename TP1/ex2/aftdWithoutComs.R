aftd <- function(d) {
    dMat <- as.matrix(d)
    dSquared <- dMat ^ 2

    n <- dim(dMat)[1]

    qn <- diag(1, n) - 1/n * matrix(1, ncol = n, nrow = n)
    w <- -1/2 * qn %*% dSquared %*% qn
    eigenW <- eigen(1/n * w)

    moreThanZero <- function(x) {
        return(x > 0 || abs(x) < .Machine$double.eps)
    }
    stopifnot(moreThanZero(eigenW$values))

    v <- sqrt(n) * eigenW$vectors
    l <- diag(eigenW$values)

    res <- NULL
    res$quality <- 100 * sum(diag(l)[c(1,2)]) / sum(diag(l)[diag(l) > 0])
    res$points <- (v %*% sqrt(l))[, c(1,2)]
    return(res)
}
