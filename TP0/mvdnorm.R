mvdnorm <- function(x, mu, sigma) {
    # computes the density function of a multidimensional normal distribution
    xMinusMu <- x - mu
    exp <- exp(-1/2 * sum(xMinusMu * solve(sigma, xMinusMu)))
    density <- 1 / (2 * pi * sqrt(det(sigma))) * exp
    return(density)
}

mvdnorm2 <- function(x, mu, sigma) {
    n <- dim(x)[1]
    p <- dim(x)[2]
    xc <- x - matrix(rep(mu, n), nrow = n, byrow = T)
    density <- exp(-1/2 * diag(xc %*% ginv(sigma) %*% t(xc))) /
        ((2 * pi)^(p/2) * det(sigma)^1/2)
    return(density)
}
