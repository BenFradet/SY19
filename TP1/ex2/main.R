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
     xlim = range(aftdHM$points[, 1]),
     ylim = range(aftdHM$points[, 2]))
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
     xlim = range(aftd$points[, 1]),
     ylim = range(aftd$points[, 2]))
text(aftd$points[, 1], aftd$points[, 2],
     rownames(mutations),
     cex = 0.8)
dev.off()
cat(pngname, 'sauvegardee\n\n')

# computes Sammon's non linear mapping
mutationsMatrix <- as.matrix(mutations)
sammon <- sammon(mutationsMatrix, aftd$points, k = 2)

pngnameSammon <- 'sammon.png'
png(pngnameSammon, width = 750, height = 500)
plot(sammon$points[, 1], sammon$points[, 2],
     type = 'n',
     main = 'Projection de Sammon des donnees mutations',
     xlab = '',
     ylab = '',
     xlim = range(sammon$points[, 1]),
     ylim = range(sammon$points[, 2]))
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
     main = 'Projection de Kruskal des donnees mutations',
     xlab = '',
     ylab = '',
     xlim = range(kruskal$points[, 1]),
     ylim = range(kruskal$points[, 2]))
text(kruskal$points[, 1], kruskal$points[, 2],
     rownames(mutations),
     cex = 0.8)
dev.off()
cat(pngnameKruskal, 'sauvegardee\n\n')

# computes Shepard's diagrams
shepardAftd <- Shepard(dist(mutationsMatrix), aftd$points)
shepardSammon <- Shepard(dist(mutationsMatrix), sammon$points)
shepardKruskal <- Shepard(dist(mutationsMatrix), kruskal$points)

pngnameShepardAftd <- 'shepardAftd.png'
png(pngnameShepardAftd, width = 500, height = 500)
plot(shepardAftd,
     pch = '.',
     xlab = 'Dissimilarite',
     ylab = 'Distance',
     xlim = range(shepardAftd$x),
     ylim = range(shepardAftd$y))
lines(shepardAftd$x, shepardAftd$yf,
      type = 'S')
dev.off()
cat(pngnameShepardAftd, 'sauvegardee\n\n')

pngnameShepardSammon <- 'shepardSammon.png'
png(pngnameShepardSammon, width = 500, height = 500)
plot(shepardSammon,
     pch = '.',
     xlab = 'Dissimilarite',
     ylab = 'Distance',
     xlim = range(shepardSammon$x),
     ylim = range(shepardSammon$y))
lines(shepardSammon$x, shepardSammon$yf,
      type = 'S')
dev.off()
cat(pngnameShepardSammon, 'sauvegardee\n\n')

pngnameShepardKruskal <- 'shepardKruskal.png'
png(pngnameShepardKruskal, width = 500, height = 500)
plot(shepardKruskal,
     pch = '.',
     xlab = 'Dissimilarite',
     ylab = 'Distance',
     xlim = range(shepardKruskal$x),
     ylim = range(shepardKruskal$y))
lines(shepardKruskal$x, shepardKruskal$yf,
      type = 'S')
dev.off()
cat(pngnameShepardKruskal, 'sauvegardee\n\n')
