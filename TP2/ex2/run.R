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
        resEM <- em(data, nbClasses)
        hashmapEM[[paste(resEM$likelihood)]] <- resEM$class
        dfEM <- rbind(dfEM, resEM[-length(resEM)])

        resCEM <- em(data, nbClasses, TRUE)
        hashmapCEM[[paste(resCEM$likelihood)]] <- resCEM$class
        dfCEM <- rbind(dfCEM, resCEM[-length(resCEM)])
    }

    maxLhEM <- max(dfEM$likelihood)
    rowMaxLhEM <- dfEM[dfEM$likelihood == maxLhEM, ][1, ]
    classMaxLhEM <- hashmapEM[[paste(maxLhEM)]]

    maxLhCEM <- max(dfCEM$likelihood)
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
    cat('piEmp1 =', n1 / n, 'piEmp2 =', (n - n1) / n, '\n\n')
    cat('mu1 =', rowMaxLhCEM$mu1, 'mu2 =', rowMaxLhCEM$mu2, '\n')
    cat('muEmp1 =', mean(x[1:n1]), 'muEmp2 =', mean(x[n1Plus1:n]), '\n\n')
    cat('sigma1 =', rowMaxLhCEM$sigma1, 'sigma2 =', rowMaxLhCEM$sigma2, '\n')
    cat('sigmaEmp1 =', sd(x[1:n1]) ^ 2, 'sigmaEmp2 =', sd(x[n1Plus1:n]) ^ 2, '\n\n')
    cat('likelihood =', rowMaxLhCEM$likelihood, '\n')
    randIndex <- randindex(classMaxLhCEM, dataClass)
    cat('rand index =', randIndex$rate, '\n')

    cat('\nKMeans:\n')
    randIndex <- randindex(kmeansI$cluster, dataClass)
    cat('rand index =', randIndex$rate, '\n\n')
}

