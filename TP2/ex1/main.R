library(MASS)
library(cluster)
source('randIndex.R')

data(iris)
fleurs <- NULL
fleurs$num <- iris[, c(1:4)]
fleurs$cls <- iris[, 5]

#PCA
# center fleurs$num by column
fleurs$numCentered <- apply(fleurs$num, 2, function(c) { c - mean(c) })
# covariance matrix
fleurs$sigma <- 1 / dim(fleurs$num)[1] *
    t(fleurs$numCentered) %*% fleurs$numCentered
# computes the eigen values and vectors
fleurs$eigen <- eigen(fleurs$sigma)
# principal components
fleurs$principalComponents <- fleurs$numCentered %*% fleurs$eigen$vectors
# computes the percentage of explained inertia for the first factorial plan
fleurs$inertia <- 100 * sum(fleurs$eigen$values[c(1, 2)]) /
                        sum(fleurs$eigen$values[fleurs$eigen$values > 0])

cat('Pourcentage d\'inertie expliquée par le premier plan factoriel',
    fleurs$inertia, '\n')

for (i in 2:4) {
    kmeansi <- kmeans(fleurs$num, i)
    pngName <- paste('kmeans', i, '.png', sep = '')
    png(pngName)
    plot(fleurs$principalComponents,
         pch = c(1:3)[fleurs$cls],
         col = c(1:i)[kmeansi$cluster],
         main = paste('Représentation des clusters produits par\n',
                      'la méthode des centres mobiles avec', i, 'classes'),
         xlab = 'Composante 1',
         ylab = 'Composante2')
    legend(-2, 1.4,
           title = 'Classes réelles',
           c('Classe 1', 'Classe 2', 'Classe 3'),
           pch = c(1, 2, 3))
    clusterNames <- c()
    clusterCol <- c()
    for (k in 1:i) {
        clusterNames[k] <- paste('Cluster', k)
        clusterCol[k] <- k
    }
    legend(-0.1, 1.4,
           title = 'Clusters issus des kmeans',
           clusterNames,
           col = clusterCol,
           lwd = 1)
    dev.off()
    cat(pngName, 'sauvegardée\n')
}

inertia <- c()
for (i in 1:100) {
    kmeans3 <- kmeans(fleurs$num, 3)
    inertia[i] <- kmeans3$tot.withinss
}
print(table(inertia))

inertia <- matrix(0, ncol = 4, nrow = 100)
for (i in 2:5) {
    for (j in 1:100) {
        kmeansij <- kmeans(fleurs$num, i)
        inertia[j, i - 1] <- kmeansij$tot.withinss
    }
}
inertiaMeans <- colMeans(inertia)
pngName <- 'methodeCoude.png'
png(pngName)
plot(c(2, 3, 4, 5), inertiaMeans,
     type = 'l',
     xlab = 'Nombre de classes k',
     ylab = 'Inertie intra-classe',
     main = 'Inerties intra-classes moyennes pour k = {2, 3, 4, 5} classes')
dev.off()
cat(pngName, 'sauvegardée\n')

randIndex <- randindex(kmeans3$cluster, fleurs$cls)
cat('rand index:', randIndex$rate, '\n')
