runCV <- function(x, t, k, nk, hidden, decay) {
    proba <- c()
    for (i in 1:5) {
        range <- c(((i - 1) * nk + 1):(i * nk))
        xCV <- x[-range, ]
        tCV <- t[-range, ]
        xTest <- x[range, ]
        tTest <- t[range, ]
        model <- nnet(xCV, tCV, size = hidden, decay = decay, softmax = T,
                      maxit = 500, trace = F)
        tPredicted <- round(predict(model, xTest))
        count <- 0
        for (j in 1:nk) {
            if (!all(tTest[j, ] == tPredicted[j, ])) {
                count <- count + 1
            }
        }
        proba[i] <- count / nk
    }
    error <- sum(nk * proba) / n
    return(error)
}
