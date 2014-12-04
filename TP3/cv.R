runCV <- function(x, t, k, nk, hidden, decay) {
    probas <- c()
    models <- list()
    for (i in 1:5) {
        range <- c(((i - 1) * nk + 1):(i * nk))
        xCV <- x[-range, ]
        tCV <- t[-range, ]
        xTest <- x[range, ]
        tTest <- t[range, ]
        models[[i]] <- nnet(xCV, tCV, size = hidden, decay = decay, softmax = T,
                      maxit = 500, trace = F)
        tPredicted <- round(predict(model, xTest))
        count <- 0
        for (j in 1:nk) {
            if (!all(tTest[j, ] == tPredicted[j, ])) {
                count <- count + 1
            }
        }
        probas[i] <- count / nk
    }
    error <- sum(nk * probas) / n

    res <- NULL
    res$error <- error
    res$model <- models[[which.min(probas)]]
    return(res)
}
