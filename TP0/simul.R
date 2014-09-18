library(MASS)

simul <- function(n, pi1, pi2, pi3, mu1, mu2, mu3, sigma1, sigma2, sigma3) {
    #replace rbinom by multinomial
    n1 <- rbinom(1, n, pi1)
    n2 <- rbinom(1, n, pi2)
    n3 <- n - n1 - n2

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
