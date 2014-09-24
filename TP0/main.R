source("getSigma.R", local = T)
source("simul.R", local = T)
source("mvdnorm.R", local = T)

sigma1 <- getSigma(-pi/3, 2, 1.5)
sigma2 <- getSigma(-pi/6, 1, 1.5)
sigma3 <- getSigma(pi/6, 1, 1.5)

mu1 <- c(-3, 8)
mu2 <- c(-5, 10)
mu3 <- c(-1, 10)

saveImages <- function(pis) {
    # simulate a sample from the different mus and covariance matrices and
    # proportions (pis)
    sample <- simul(1000, pis,
                 mu1, mu2, mu3,
                 sigma1, sigma2, sigma3)

    # generate a grid to display the contour of the density functions
    x <- seq(from = -8, to = 6, by = 0.4)
    nx <- length(x)
    y <- seq(from = -2, to = 14, by = 0.4)
    ny <- length(y)
    grid <- cbind(rep.int(x, times = rep(ny, nx)), rep(y, nx))

    pngname <- paste("plot", pis[1], pis[2], pis[3], ".png", sep = "")
    png(pngname, width = 500, height = 500)

    plot(sample[, c(1, 2)],
         col = c("red", "blue", "green")[sample[, 3]],
         pch = c(1, 2, 3)[sample[, 3]],
         cex = 0.5,
         main = "Combination of multivariate normal distributions",
         xlab = "x", ylab = "y")

    # compute the density of the different multivariate normal distributions
    # and add contours to the plot
    density1 <- mvdnorm2(grid, mu1, sigma1)
    contour(x, y, matrix(density1, nrow = nx, byrow = T),
            add = T, drawlabels = F, col = "red", nlevels = 5)

    density2 <- mvdnorm2(grid, mu2, sigma2)
    contour(x, y, matrix(density2, nrow = nx, byrow = T),
            add = T, drawlabels = F, col = "blue", nlevels = 5)

    density3 <- mvdnorm2(grid, mu3, sigma3)
    contour(x, y, matrix(density3, nrow = nx, byrow = T),
            add = T, drawlabels = F, col = "green", nlevels = 5)

    dev.off()
    cat(paste(pngname, "sauvegardee\n"))
}

saveImages(c(1/2, 1/4, 1/4))
saveImages(c(0.6, 0.2, 0.2))
saveImages(c(0.98, 0.01, 0.01))
