source('gaussianDensity.R')

n <- 1000
n1 <- rbinom(1, n, 0.5)
n1Plus1 <- n1 + 1
x <- c(rnorm(n1), rnorm(n - n1, mean = 6, sd = 5))

# Expectation-maximization algorithm
# computation of the initial parameters
pis <- matrix(0, nrow = n, ncol = 2)
pis[1, 1] <- n1 / n
pis[1, 2] <- (n - n1) / n

mus <- matrix(0, nrow = n, ncol = 2)
mus[1, 1] <- mean(x[1:n1])
mus[1, 2] <- mean(x[n1Plus1:n])

sigmas <- matrix(0, nrow = n, ncol = 2)
sigmas[1, 1] <- sd(x[1:n1]) ^ 2
sigmas[1, 2] <- sd(x[n1Plus1:n]) ^ 2

t <- matrix(0, nrow = n, ncol = 2)
iter <- 0
stoppingCriterion <- 10^-16

repeat {
    # expectation step, computation of the t_ik
    iter <- iter + 1
    for (i in 1:n) {
        den <- pis[iter, 1] *
                gaussianDensity(x[i], mus[iter, 1], sigmas[iter, 1]) +
            pis[iter, 2] * gaussianDensity(x[i], mus[iter, 2], sigmas[iter, 2])
        t[i, 1] <- pis[iter, 1] *
            gaussianDensity(x[i], mus[iter, 1], sigmas[iter, 1]) / den
        t[i, 2] <- pis[iter, 2] *
            gaussianDensity(x[i], mus[iter, 2], sigmas[iter, 2]) / den
    }

    # maximization step, update of the parameters
    pis[iter + 1, 1] <- sum(t[1:n, 1]) / n
    pis[iter + 1, 2] <- 1 - pis[iter + 1, 1]
    mus[iter + 1, 1] <- sum(t[1:n, 1] * x[1:n]) / sum(t[1:n, 1])
    mus[iter + 1, 2] <- sum(t[1:n, 2] * x[1:n]) / sum(t[1:n, 2])
    sigmas[iter + 1, 1] <- sum(t[1:n, 1] * (x[1:n] - mus[iter + 1, 1]) ^ 2) /
        sum(t[1:n, 1])
    sigmas[iter + 1, 2] <- sum(t[1:n, 2] * (x[1:n] - mus[iter + 1, 2]) ^ 2) /
        sum(t[1:n, 2])

    # check for convergence
    #diff <- sqrt(abs(pis[iter, 1] - pis[iter + 1, 1]) +
    #          abs(pis[iter, 2] - pis[iter + 1, 2]) +
    #          abs(mus[iter, 1] - mus[iter + 1, 1]) +
    #          abs(mus[iter, 2] - mus[iter + 1, 2]) +
    #          abs(sigmas[iter, 1] - sigmas[iter + 1, 1]) +
    #          abs(sigmas[iter, 2] - sigmas[iter + 1, 2]))
    diff <- sqrt(abs(pis[iter, 1] - pis[iter + 1, 1]) +
                 abs(pis[iter, 2] - pis[iter + 1, 2]))
    if (diff < stoppingCriterion) {
        cat('Converged in', iter, 'iterations\n')
        cat('pi1 =', pis[iter + 1, 1], 'pi2 =', pis[iter + 1, 2], '\n')
        cat('mu1 =', mus[iter + 1, 1], 'mu2 =', mus[iter + 1, 2], '\n')
        cat('sigma1 =', sigmas[iter + 1, 1], 'sigma2 =', sigmas[iter + 1, 2],
            '\n')
        break
    }
}
