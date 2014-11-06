source('gaussian.R')

em <- function(x, nbClasses) {

    n <- length(x)

    # standardize the input data
    x <- x - mean(x)
    x <- x / sd(x)

    # classification à partir des tik de la dernière itération?
    # sensible aux outliers
    # faire 100 fois et prendre la solution avec la plus grande vraisemblance

    # Expectation-maximization algorithm
    # computation of the initial parameters

    # 1 / k (k = # nb of classes)
    pis <- matrix(0, nrow = n, ncol = 2)
    pis[1, 1] <- 1 / nbClasses
    pis[1, 2] <- 1 / nbClasses

    # random values between -1 and 1 because x centered
    mus <- matrix(0, nrow = n, ncol = 2)
    mus[1, 1] <- runif(1, -1, 1)
    print(mus[1, 1])
    mus[1, 2] <- runif(1, -1, 1)
    print(mus[1,2])

    # initialized to 1 because x standardized
    sigmas <- matrix(0, nrow = n, ncol = 2)
    sigmas[1, 1] <- 1
    sigmas[1, 2] <- 1

    t <- matrix(0, nrow = n, ncol = 2)
    iter <- 0
    stoppingCriterion <- 10^-9

    repeat {
        # expectation step, computation of the t_ik
        iter <- iter + 1
        for (i in 1:n) {
            den <- pis[iter, 1] *
                    gaussian(x[i], mus[iter, 1], sigmas[iter, 1]) +
                pis[iter, 2] * gaussian(x[i], mus[iter, 2], sigmas[iter, 2])
            t[i, 1] <- pis[iter, 1] *
                gaussian(x[i], mus[iter, 1], sigmas[iter, 1]) / den
            t[i, 2] <- pis[iter, 2] *
                gaussian(x[i], mus[iter, 2], sigmas[iter, 2]) / den
        }

        # maximization step, update of the parameters
        pis[iter + 1, 1] <- sum(t[1:n, 1]) / n
        pis[iter + 1, 2] <- 1 - pis[iter + 1, 1]
        mus[iter + 1, 1] <- sum(t[1:n, 1] * x[1:n]) / sum(t[1:n, 1])
        mus[iter + 1, 2] <- sum(t[1:n, 2] * x[1:n]) / sum(t[1:n, 2])
        sigmas[iter + 1, 1] <- sum(t[1:n, 1] * (x[1:n] -
            mus[iter + 1, 1]) ^ 2) / sum(t[1:n, 1])
        sigmas[iter + 1, 2] <- sum(t[1:n, 2] * (x[1:n] -
            mus[iter + 1, 2]) ^ 2) / sum(t[1:n, 2])

        # check for convergence
        likelihood <- 0
        diff <- abs(pis[iter, 1] - pis[iter + 1, 1]) +
                abs(pis[iter, 2] - pis[iter + 1, 2]) +
                abs(mus[iter, 1] - mus[iter + 1, 1]) +
                abs(mus[iter, 2] - mus[iter + 1, 2]) +
                abs(sigmas[iter, 1] - sigmas[iter + 1, 1]) +
                abs(sigmas[iter, 2] - sigmas[iter + 1, 2])
        if (diff < stoppingCriterion) {
            # computes the likelihood of our solution
            for (i in 1:n) {
                likelihood <- likelihood +
                    t[i, 1] * log(pis[iter + 1, 1] *
                        gaussian(x[i], mus[iter + 1, 1], sigmas[iter + 1, 1])) +
                    t[i, 2] * log(pis[iter + 1, 2] *
                        gaussian(x[i], mus[iter + 1, 2], sigmas[iter + 1, 2]))
            }

            res <- NULL
            res$iter <- iter
            res$pi1 <- pis[iter + 1, 1]
            res$pi2 <- pis[iter + 1, 2]
            res$mu1 <- mus[iter + 1, 1]
            res$mu2 <- mus[iter + 1, 2]
            res$sigma1 <- sigmas[iter + 1, 1]
            res$sigma2 <- sigmas[iter + 1, 2]
            res$likelihood <- likelihood

            # computes a class vector
            # res$class <- apply(

            return(res)
        }
    }
}
