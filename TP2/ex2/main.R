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

parametersX <- run(x, xClass, 2, 10)

cat('\ndonnees:\n')
cat('piEmp1 =', n1 / n, 'piEmp2 =', (n - n1) / n, '\n')
cat('muEmp1 =', mean(x[1:n1]), 'muEmp2 =', mean(x[n1Plus1:n]), '\n')
cat('sigmaEmp1 =', sd(x[1:n1]) ^ 2, 'sigmaEmp2 =', sd(x[n1Plus1:n]) ^ 2, '\n\n')

# histogram with density
y <- seq(min(x), max(x), length = 100)
fEmp <- dnorm(y, mean(x[1:n1]), sd(x[1:n1])) +
    dnorm(y, mean(x[n1Plus1:n]), sd(x[n1Plus1:n]))
fEm <- dnorm(y, parametersX$em$mu1, sqrt(parametersX$em$sigma1)) +
    dnorm(y, parametersX$em$mu2, sqrt(parametersX$em$sigma2))
fCem <- dnorm(y, parametersX$cem$mu1, sqrt(parametersX$cem$sigma1)) +
    dnorm(y, parametersX$cem$mu2, sqrt(parametersX$cem$sigma2))

pngname <- 'histDensityX.png'
png(pngname)
histX <- hist(x,
              breaks = 40,
              ylim = c(0, 220),
              main = paste('Histogramme des crabes en fonction de\n',
                           'la longueur de leur lobe frontal', sep = ''),
              xlab = 'Longueur du lobe frontal en mm',
              ylab = 'Frequence')
multiplier <- histX$counts / histX$density
lines(y, fEmp * multiplier[1] / 2, col = 'black')
lines(y, fEm * multiplier[1] / 2, col = 'red')
lines(y, fCem * multiplier[1] / 2, col = 'blue')
legend(2, 150,
       c('réelles', 'em', 'cem'),
       col = c('black', 'red', 'blue'),
       lwd = 1)
dev.off()
cat(pngname, 'sauvegardee\n')

# application to the crabs dataset
data(crabs)
crabsQuant <- crabs[, 4:8]
crabsQuant <- crabsQuant / crabsQuant[, 4]
crabsQuant <- crabsQuant[, -4]

crabsFL <- scale(crabsQuant$FL)
crabsRW <- scale(crabsQuant$RW)
crabsSex <- crabs[, 2]
crabsSpecies <- crabs[, 1]

# histograms
pngname <- 'histFL.png'
png(pngname)
hist(crabsFL,
     breaks = 20,
     main = paste('Histogramme des crabes en fonction de\n',
                  'la longueur de leur lobe frontal', sep = ''),
     xlab = 'Longueur du lobe frontal en mm',
     ylab = 'Frequence')
dev.off()
cat(pngname, 'sauvegardee\n')

pngname <- 'histRW.png'
png(pngname)
hist(crabsRW,
     breaks = 20,
     main = paste('Histogramme des crabes en fonction de\n',
                  'la longueur de leur partie arriere', sep = ''),
     xlab = 'Longueur de la partie arriere en mm',
     ylab = 'Frequence')
dev.off()
cat(pngname, 'sauvegardee\n')

# for the frontal lobe variable
cat('\nFrontal lobe / Species:\n\n')
parametersFL <- run(crabsFL, crabsSpecies, nlevels(crabsSpecies), 100)
cat('\ndonnees FL:\n')
cat('piEmp1 =', 0.5, 'piEmp2 =', 0.5, '\n')
cat('muEmp1 =', mean(crabsFL[crabs$sp == 'B']),
    'muEmp2 =', mean(crabsFL[crabs$sp == 'O']), '\n')
cat('sigmaEmp1 =', sd(crabsFL[crabs$sp == 'B']) ^ 2,
    'sigmaEmp2 =', sd(crabsFL[crabs$sp == 'O']) ^ 2, '\n\n')

cat('\n\nRearWidth / Sex:\n\n')
parametersRW <- run(crabsRW, crabsSex, nlevels(crabsSex), 100)
cat('\ndonnees RW:\n')
cat('piEmp1 =', 0.5, 'piEmp2 =', 0.5, '\n')
cat('muEmp1 =', mean(crabsRW[crabs$sex == 'M']),
    'muEmp2 =', mean(crabsRW[crabs$sex == 'F']), '\n')
cat('sigmaEmp1 =', sd(crabsRW[crabs$sex == 'M']) ^ 2,
    'sigmaEmp2 =', sd(crabsRW[crabs$sex == 'F']) ^ 2, '\n')

# histograms with density
x <- seq(-3, 3, length = 100)
fEm1 <- dnorm(x, parametersFL$em$mu1, sqrt(parametersFL$em$sigma1))
fEm2 <- dnorm(x, parametersFL$em$mu2, sqrt(parametersFL$em$sigma2))
fEm <- dnorm(x, parametersFL$em$mu1, sqrt(parametersFL$em$sigma1)) +
    dnorm(x, parametersFL$em$mu2, sqrt(parametersFL$em$sigma2))
fCem1 <- dnorm(x, parametersFL$cem$mu1, sqrt(parametersFL$cem$sigma1))
fCem2 <- dnorm(x, parametersFL$cem$mu2, sqrt(parametersFL$cem$sigma2))
fCem <- dnorm(x, parametersFL$cem$mu1, sqrt(parametersFL$cem$sigma1)) +
    dnorm(x, parametersFL$cem$mu2, sqrt(parametersFL$cem$sigma2))

pngname <- 'histDensityFL.png'
png(pngname)
histFL <- hist(crabsFL,
             breaks = 20,
             main = paste('Histogramme des crabes en fonction de\n',
                          'la longueur de leur lobe frontal', sep = ''),
             xlab = 'Longueur du lobe frontal en mm',
             ylab = 'Frequence')
multiplier <- histFL$counts / histFL$density
lines(x, fEm1 * multiplier[1] / 2, col = 'red', lty = 2)
lines(x, fEm2 * multiplier[1] / 2, col = 'red', lty = 2)
lines(x, fEm * multiplier[1] / 2, col = 'red')
lines(x, fCem1 * multiplier[1] / 2, col = 'blue', lty = 2)
lines(x, fCem2 * multiplier[1] / 2, col = 'blue', lty = 2)
lines(x, fCem * multiplier[1] / 2, col = 'blue')
legend(1.3, 20,
       c('em', 'cem'),
       col = c('red', 'blue'),
       lwd = 1)
dev.off()
cat(pngname, 'sauvegardee\n')

x <- seq(-3, 3, length = 100)
fEm1 <- dnorm(x, parametersRW$em$mu1, sqrt(parametersRW$em$sigma1))
fEm2 <- dnorm(x, parametersRW$em$mu2, sqrt(parametersRW$em$sigma2))
fEm <- dnorm(x, parametersRW$em$mu1, sqrt(parametersRW$em$sigma1)) +
    dnorm(x, parametersRW$em$mu2, sqrt(parametersRW$em$sigma2))
fCem1 <- dnorm(x, parametersRW$cem$mu1, sqrt(parametersRW$cem$sigma1))
fCem2 <- dnorm(x, parametersRW$cem$mu2, sqrt(parametersRW$cem$sigma2))
fCem <- dnorm(x, parametersRW$cem$mu1, sqrt(parametersRW$cem$sigma1)) +
    dnorm(x, parametersRW$cem$mu2, sqrt(parametersRW$cem$sigma2))

pngname <- 'histDensityRW.png'
png(pngname)
histRW <- hist(crabsRW,
               breaks = 20,
               main = paste('Histogramme des crabes en fonction de\n',
                            'la longueur de leur partie arriere', sep = ''),
               xlab = 'Longueur de la partie arriere en mm',
               ylab = 'Frequence')
multiplier <- histRW$counts / histRW$density
lines(x, fEm1 * multiplier[1] / 2, col = 'red', lty = 2)
lines(x, fEm2 * multiplier[1] / 2, col = 'red', lty = 2)
lines(x, fEm * multiplier[1] / 2, col = 'red')
lines(x, fCem1 * multiplier[1] / 2, col = 'blue', lty = 2)
lines(x, fCem2 * multiplier[1] / 2, col = 'blue', lty = 2)
lines(x, fCem * multiplier[1] / 2, col = 'blue')
legend(-2.5, 20,
       c('em', 'cem'),
       col = c('red', 'blue'),
       lwd = 1)
dev.off()
cat(pngname, 'sauvegardee\n')
