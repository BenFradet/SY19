source('em.R')

run <- function(data, dataClass, nbClasses, nbIters) {
    hashmapEM <- list()
    dfEM <- data.frame(iter = c(),
                     pi1 = c(),
                     pi2 = c(),
                     mu1 = c(),
                     mu2 = c(),
                     sigma1 = c(),
                     sigma2 = c(),
                     likelihood = c())
    hashmapCEM <- list()
    dfCEM <- data.frame(iter = c(),
                     pi1 = c(),
                     pi2 = c(),
                     mu1 = c(),
                     mu2 = c(),
                     sigma1 = c(),
                     sigma2 = c(),
                     likelihood = c())
    for (i in 1:nbIters) {
        resEM <- em(data, nbClasses, 1000)
        hashmapEM[[paste(resEM$likelihood)]] <- resEM$class
        dfEM <- rbind(dfEM, resEM[-length(resEM)])

        resCEM <- em(data, nbClasses, 1000, TRUE)
        hashmapCEM[[paste(resCEM$likelihood)]] <- resCEM$class
        dfCEM <- rbind(dfCEM, resCEM[-length(resCEM)])
    }

    maxLhEM <- min(dfEM$likelihood)
    rowMaxLhEM <- dfEM[dfEM$likelihood == maxLhEM, ][1, ]
    classMaxLhEM <- hashmapEM[[paste(maxLhEM)]]

    maxLhCEM <- min(dfCEM$likelihood)
    rowMaxLhCEM <- dfCEM[dfCEM$likelihood == maxLhCEM, ][1, ]
    classMaxLhCEM <- hashmapCEM[[paste(maxLhCEM)]]

    kmeansI <- kmeans(data, nbClasses)

    cat('\nEM:\n')
    cat('Converged in', rowMaxLhEM$iter, 'iterations\n\n')
    cat('pi1 =', rowMaxLhEM$pi1, 'pi2 =', rowMaxLhEM$pi2, '\n')
    cat('mu1 =', rowMaxLhEM$mu1, 'mu2 =', rowMaxLhEM$mu2, '\n')
    cat('sigma1 =', rowMaxLhEM$sigma1, 'sigma2 =', rowMaxLhEM$sigma2, '\n')
    cat('likelihood =', rowMaxLhEM$likelihood, '\n')
    randIndex <- randindex(classMaxLhEM, dataClass)
    cat('rand index =', randIndex$rate, '\n')

    cat('\nCEM:\n')
    cat('Converged in', rowMaxLhCEM$iter, 'iterations\n\n')
    cat('pi1 =', rowMaxLhCEM$pi1, 'pi2 =', rowMaxLhCEM$pi2, '\n')
    cat('mu1 =', rowMaxLhCEM$mu1, 'mu2 =', rowMaxLhCEM$mu2, '\n')
    cat('sigma1 =', rowMaxLhCEM$sigma1, 'sigma2 =', rowMaxLhCEM$sigma2, '\n')
    cat('likelihood =', rowMaxLhCEM$likelihood, '\n')
    randIndex <- randindex(classMaxLhCEM, dataClass)
    cat('rand index =', randIndex$rate, '\n')

    cat('\nKMeans:\n')
    randIndex <- randindex(kmeansI$cluster, dataClass)
    cat('rand index =', randIndex$rate, '\n\n')

    return(list(em = rowMaxLhEM, cem = rowMaxLhCEM))
}

