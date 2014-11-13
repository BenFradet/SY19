library(MASS)
source('run.R')
source('../randIndex.R')


# artificial data
n <- 1000
n1 <- rbinom(1, n, 0.5)
n1Plus1 <- n1 + 1

x <- c(rnorm(n1), rnorm(n - n1, mean = 6, sd = 5))

x <- x - mean(x)
x <- x / sd(x)

xClass <- c(rep(1, n1), rep(2, n - n1))

run(x, xClass, 2, 10)

cat('\ndonnees:\n')
cat('piEmp1 =', n1 / n, 'piEmp2 =', (n - n1) / n, '\n')
cat('muEmp1 =', mean(x[1:n1]), 'muEmp2 =', mean(x[n1Plus1:n]), '\n')
cat('sigmaEmp1 =', sd(x[1:n1]) ^ 2, 'sigmaEmp2 =', sd(x[n1Plus1:n]) ^ 2, '\n')

# application to the crabs dataset
data(crabs)
crabsQuant <- crabs[, 4:8]
crabsQuant <- crabsQuant / crabsQuant[, 4]
crabsQuant <- crabsQuant[, -4]

crabsFL <- scale(crabsQuant$FL)
crabsRW <- scale(crabsQuant$RW)
crabsSex <- crabs[, 2]
crabsSpecies <- crabs[, 1]

# for the frontal lobe variable
