library(MASS)
source('aftd.R')

mutations <- read.table('mutations2.txt', header = F, row.names = 1)

# homemade aftd
aftdHM <- aftd(mutations)
aftd <- cmdscale(mutations, k = 2, eig = T)

# computes the percentage of explained inertia by the two first axes
aftdQuality <- 100 * sum(aftd$eig[c(1,2)]) / sum(aftd$eig[aftd$eig > 0])

# compares the difference between our explained inertia
stopifnot(aftdQuality == aftdHM$quality)

cat('Pourcentage d inertie expliquee par les 2 premiers axes par notre méthode',
    aftdHM$quality, '\n')
cat('Pourcentage d inertie explique par les 2 premiers axes par cmdscale',
    aftdQuality, '\n')

pngnameHomemade <- 'aftdHomemade.png'
png(pngnameHomemade, width = 500, height = 500)
plot(aftdHM$points[, 1], aftdHM$points[, 2],
     type = 'n',
     main = 'Representation de l AFTD des especes par notre methode',
     xlab = '',
     ylab = '',
     xlim = c(-30, 70),
     ylim = c(-50, 50))
text(aftdHM$points[, 1], aftdHM$points[, 2],
     rownames(mutations),
     cex = 0.8)
dev.off()
cat(pngnameHomemade, 'sauvegardee\n')

pngname <- 'aftd.png'
png(pngname, width = 500, height = 500)
plot(aftd$points[, 1], aftd$points[, 2],
     type = 'n',
     main = 'Representation de l AFTD des especes par cmdscale',
     xlab = '',
     ylab = '',
     xlim = c(-30, 70),
     ylim = c(-50, 50))
text(aftd$points[, 1], aftd$points[, 2],
     rownames(mutations),
     cex = 0.8)
dev.off()
cat(pngname, 'sauvegardee\n\n')

# computes Sammon's non linear mapping
mutationsMatrix = as.matrix(mutations)
sammon <- sammon(mutationsMatrix, aftd$points, k = 2)

pngnameSammon <- 'sammon.png'
png(pngnameSammon, width = 750, height = 500)
plot(sammon$points[, 1], sammon$points[, 2],
     type = 'n',
     main = 'Projection de Sammon, aftd realisee par cmdscale',
     xlab = '',
     ylab = '',
     xlim = c(-50, 100),
     ylim = c(-50, 50))
text(sammon$points[, 1], sammon$points[, 2],
     rownames(mutations),
     cex = 0.8)
dev.off()
cat(pngnameSammon, 'sauvegardee\n\n')

# computes Kruskal's non-metric multidimensional scaling
kruskal <- isoMDS(mutationsMatrix, aftd$points, k = 2)

pngnameKruskal <- 'kruskal.png'
png(pngnameKruskal, width = 750, height = 500)
plot(kruskal$points[, 1], kruskal$points[, 2],
     type = 'n',
     main = 'Projection de Kruskal, aftd realisee par cmdscale',
     xlab = '',
     ylab = '',
     xlim = c(-50, 100),
     ylim = c(-50, 50))
text(kruskal$points[, 1], kruskal$points[, 2],
     rownames(mutations),
     cex = 0.8)
dev.off()
cat(pngnameKruskal, 'sauvegardee\n\n')

# computes Shepard's diagrams
shepardAftd <- Shepard(mutationsMatrix, aftd$points, k = 2)
shepardSammon <- Shepard(mutationsMatrix, sammon$points, k = 2)
shepardKruskal <- Shepard(mutationsMatrix, kruskal$points, k = 2)
