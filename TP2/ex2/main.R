source('em.R')
source('../randIndex.R')

n <- 1000
n1 <- rbinom(1, n, 0.5)
n1Plus1 <- n1 + 1
x <- c(rnorm(n1), rnorm(n - n1, mean = 6, sd = 5))

x <- x - mean(x)
x <- x / sd(x)

xClass <- c(rep(1, n1), rep(2, n - n1))

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
for (i in 1:10) {
    resEM <- em(x, 2)
    cat('resEM:', resEM$likelihood, '\n')
    hashmapEM[[paste(resEM$likelihood)]] <- resEM$class
    dfEM <- rbind(dfEM, resEM[-length(resEM)])

    resCEM <- em(x, 2, TRUE)
    cat('resCEM:', resCEM$likelihood, '\n')
    hashmapCEM[[paste(resCEM$likelihood)]] <- resCEM$class
    dfCEM <- rbind(dfCEM, resCEM[-length(resCEM)])
}

maxLhEM <- max(dfEM$likelihood)
maxLhCEM <- max(dfCEM$likelihood)
rowMaxLhEM <- dfEM[dfEM$likelihood == maxLhEM, ]

rowMaxLhCEM <- dfCEM[dfCEM$likelihood == maxLhCEM, ]
classMaxLhEM <- hashmapEM[[paste(maxLhEM)]]
classMaxLhCEM <- hashmapCEM[[paste(maxLhCEM)]]

cat('\nEM:\n')
cat('Converged in', rowMaxLhEM$iter, 'iterations\n\n')
cat('pi1 =', rowMaxLhEM$pi1, 'pi2 =', rowMaxLhEM$pi2, '\n')
cat('piEmp1 =', n1 / n, 'piEmp2 =', (n - n1) / n, '\n\n')
cat('mu1 =', rowMaxLhEM$mu1, 'mu2 =', rowMaxLhEM$mu2, '\n')
cat('muEmp1 =', mean(x[1:n1]), 'muEmp2 =', mean(x[n1Plus1:n]), '\n\n')
cat('sigma1 =', rowMaxLhEM$sigma1, 'sigma2 =', rowMaxLhEM$sigma2, '\n')
cat('sigmaEmp1 =', sd(x[1:n1]) ^ 2, 'sigmaEmp2 =', sd(x[n1Plus1:n]) ^ 2, '\n\n')
cat('likelihood =', rowMaxLhEM$likelihood, '\n')
randIndex <- randindex(classMaxLhEM, xClass)
cat('rand index =', randIndex$rate, '\n')

cat('\n\nCEM:\n')
cat('Converged in', rowMaxLhCEM$iter, 'iterations\n\n')
cat('pi1 =', rowMaxLhCEM$pi1, 'pi2 =', rowMaxLhCEM$pi2, '\n')
cat('piEmp1 =', n1 / n, 'piEmp2 =', (n - n1) / n, '\n\n')
cat('mu1 =', rowMaxLhCEM$mu1, 'mu2 =', rowMaxLhCEM$mu2, '\n')
cat('muEmp1 =', mean(x[1:n1]), 'muEmp2 =', mean(x[n1Plus1:n]), '\n\n')
cat('sigma1 =', rowMaxLhCEM$sigma1, 'sigma2 =', rowMaxLhCEM$sigma2, '\n')
cat('sigmaEmp1 =', sd(x[1:n1]) ^ 2, 'sigmaEmp2 =', sd(x[n1Plus1:n]) ^ 2, '\n\n')
cat('likelihood =', rowMaxLhCEM$likelihood, '\n')
randIndex <- randindex(classMaxLhCEM, xClass)
cat('rand index =', randIndex$rate, '\n')
