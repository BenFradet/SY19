source('em.R')
source('randIndex.R')

n <- 1000
n1 <- rbinom(1, n, 0.5)
n1Plus1 <- n1 + 1
x <- c(rnorm(n1), rnorm(n - n1, mean = 6, sd = 5))

x <- x - mean(x)
x <- x / sd(x)

xClass <- c(rep(1, n1), rep(2, n - n1))

hashmap <- list()
df <- data.frame(iter = c(),
                 pi1 = c(),
                 pi2 = c(),
                 mu1 = c(),
                 mu2 = c(),
                 sigma1 = c(),
                 sigma2 = c(),
                 likelihood = c())
for (i in 1:5) {
    resEM <- em(x, 2)
    print(resEM$likelihood)
    hashmap[[paste(resEM$likelihood)]] <- resEM$class
    df <- rbind(df, resEM[-length(resEM)])
}

maxLikelihood <- max(df$likelihood)
rowMaxLh <- df[df$likelihood == maxLikelihood, ]
classMaxLh <- hashmap[[paste(maxLikelihood)]]

cat('Converged in', rowMaxLh$iter, 'iterations\n\n')
cat('pi1 =', rowMaxLh$pi1, 'pi2 =', rowMaxLh$pi2, '\n')
cat('piEmp1 =', n1 / n, 'piEmp2 =', (n - n1) / n, '\n\n')
cat('mu1 =', rowMaxLh$mu1, 'mu2 =', rowMaxLh$mu2, '\n')
cat('muEmp1 =', mean(x[1:n1]), 'muEmp2 =', mean(x[n1Plus1:n]), '\n\n')
cat('sigma1 =', rowMaxLh$sigma1, 'sigma2 =', rowMaxLh$sigma2, '\n')
cat('sigmaEmp1 =', sd(x[1:n1]) ^ 2, 'sigmaEmp2 =', sd(x[n1Plus1:n]) ^ 2, '\n\n')
cat('likelihood =', rowMaxLh$likelihood, '\n')

randIndex <- randindex(classMaxLh, xClass)
cat('rand index:', randIndex$rate, '\n')
