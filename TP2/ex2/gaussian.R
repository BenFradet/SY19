# function to compute the density of a normal distribution at a given point
gaussian <- function(x, mu, sigma) {
    return(1 / sqrt(sigma * 2 * pi) * exp(-(x - mu) ^ 2 / (2 * sigma)))
}
