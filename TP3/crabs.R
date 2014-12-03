library(MASS)
library(nnet)

n <- dim(crabs)[1]

# split crabs according to their sex
x <- log(cbind(crabs$FL, crabs$RW))
T <- class.ind(crabs$sex)
color <- rep('red', n)
color[crabs$sex == 'M'] <- 'blue'
plot(x, col = color)

# split according to their color
T <- class.ind(crabs$sp)
color <- rep('red', n)
color[crabs$sp == 'B'] <- 'blue'
plot(x, col = color)
