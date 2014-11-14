library(MASS)
source('run.R')
source('../randIndex.R')

# artificial data
#n <- 1000
#n1 <- rbinom(1, n, 0.5)
#n1Plus1 <- n1 + 1
#
#x <- c(rnorm(n1), rnorm(n - n1, mean = 6, sd = 5))
#
#x <- x - mean(x)
#x <- x / sd(x)
#
#xClass <- c(rep(1, n1), rep(2, n - n1))
#
#run(x, xClass, 2, 1)
#
#cat('\ndonnees:\n')
#cat('piEmp1 =', n1 / n, 'piEmp2 =', (n - n1) / n, '\n')
#cat('muEmp1 =', mean(x[1:n1]), 'muEmp2 =', mean(x[n1Plus1:n]), '\n')
#cat('sigmaEmp1 =', sd(x[1:n1]) ^ 2, 'sigmaEmp2 =', sd(x[n1Plus1:n]) ^ 2, '\n')

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
run(crabsFL, crabsSpecies, nlevels(crabsSpecies), 10)
cat('\ndonnees FL:\n')
cat('piEmp1 =', 0.5, 'piEmp2 =', 0.5, '\n')
cat('muEmp1 =', mean(crabsFL[crabs$sp == 'B']),
    'muEmp2 =', mean(crabsFL[crabs$sp == 'O']), '\n')
cat('sigmaEmp1 =', sd(crabsFL[crabs$sp == 'B']) ^ 2,
    'sigmaEmp2 =', sd(crabsFL[crabs$sp == 'O']) ^ 2, '\n\n')

cat('\n\nRearWidth / Sex:\n\n')
run(crabsRW, crabsSex, nlevels(crabsSex), 10)
cat('\ndonnees RW:\n')
cat('piEmp1 =', 0.5, 'piEmp2 =', 0.5, '\n')
cat('muEmp1 =', mean(crabsRW[crabs$sex == 'M']),
    'muEmp2 =', mean(crabsRW[crabs$sex == 'F']), '\n')
cat('sigmaEmp1 =', sd(crabsRW[crabs$sex == 'M']) ^ 2,
    'sigmaEmp2 =', sd(crabsRW[crabs$sex == 'F']) ^ 2, '\n')

# histograms with density
x <- seq(-3, 3, length = 100)
fx <- dnorm(x, mean(crabsFL[crabs$sp == 'B']), sd(crabsFL[crabs$sp == 'B']))
fy <- dnorm(x, 
pngname <- 'histDensityFL.png'
png(pngname)
hist(crabsFL,
     breaks = 20,
     main = paste('Histogramme des crabes en fonction de\n',
                  'la longueur de leur lobe frontal', sep = ''),
     xlab = 'Longueur du lobe frontal en mm',
     ylab = 'Frequence')
#lines(
dev.off()
cat(pngname, 'sauvegardee\n')

pngname <- 'histDensityRW.png'
png(pngname)
hist(crabsRW,
     breaks = 20,
     main = paste('Histogramme des crabes en fonction de\n',
                  'la longueur de leur partie arriere', sep = ''),
     xlab = 'Longueur de la partie arriere en mm',
     ylab = 'Frequence')
dev.off()
cat(pngname, 'sauvegardee\n')
