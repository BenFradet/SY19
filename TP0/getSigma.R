getSigma <- function(theta, lambda, a) {
    D <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)),
                ncol = 2, nrow = 2)
    A <- diag(c(a, 1 / a))
    sigma <- lambda * t(D) * A * D
    return(sigma)
}
