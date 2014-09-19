library(MASS)

simul <- function(n, pis, mu1, mu2, mu3, sigma1, sigma2, sigma3) {
    multinomial <- rmultinom(1, size = n, prob = pis)
    n1 <- multinomial[1,1]
    n2 <- multinomial[2,1]
    n3 <- multinomial[3,1]

    d1 <- mvrnorm(n1, mu1, sigma1)
    d1 <- cbind(d1, matrix(1, nrow = n1))

    d2 <- mvrnorm(n2, mu2, sigma2)
    d2 <- cbind(d2, matrix(2, nrow = n2))

    d3 <- mvrnorm(n3, mu3, sigma3)
    d3 <- cbind(d3, matrix(3, nrow = n3))

    d <- rbind(d1, d2)
    d <- rbind(d, d3)

    return(d)
}
