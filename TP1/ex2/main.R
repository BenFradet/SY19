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
png(pngnameHomemade)
plot(aftdHM$points[, 1], aftdHM$points[, 2],
     type = 'n',
     main = 'Representation de l AFTD des donnees mutations par notre methode',
     xlab = '',
     ylab = '',
     xlim = range(aftdHM$points[, 1]) + c(-7, 5),
     ylim = range(aftdHM$points[, 2]),
     asp = 1)
text(aftdHM$points[, 1], aftdHM$points[, 2],
     rownames(mutations))
dev.off()
cat(pngnameHomemade, 'sauvegardee\n')

pngname <- 'aftd.png'
png(pngname)
plot(aftd$points[, 1], aftd$points[, 2],
     type = 'n',
     main = 'Representation de l AFTD des donnees mutations',
     xlab = '',
     ylab = '',
     xlim = range(aftd$points[, 1] + c(-7, 5)),
     ylim = range(aftd$points[, 2]))
text(aftd$points[, 1], aftd$points[, 2],
     rownames(mutations))
dev.off()
cat(pngname, 'sauvegardee\n\n')

# computes Sammon's non linear mapping
mutationsMatrix <- as.matrix(mutations)
sammon <- sammon(mutationsMatrix, aftd$points, k = 2)

pngnameSammon <- 'sammon.png'
png(pngnameSammon)
plot(sammon$points[, 1], sammon$points[, 2],
     type = 'n',
     main = 'Projection de Sammon des donnees mutations',
     xlab = '',
     ylab = '',
     xlim = range(sammon$points[, 1]) + c(0, 5),
     ylim = range(sammon$points[, 2]),
     asp = 1)
text(sammon$points[, 1], sammon$points[, 2],
     rownames(mutations))
dev.off()
cat(pngnameSammon, 'sauvegardee\n\n')

# computes Kruskal's non-metric multidimensional scaling
kruskal <- isoMDS(mutationsMatrix, aftd$points, k = 2)

pngnameKruskal <- 'kruskal.png'
png(pngnameKruskal)
plot(kruskal$points[, 1], kruskal$points[, 2],
     type = 'n',
     main = 'Projection de Kruskal des donnees mutations',
     xlab = '',
     ylab = '',
     xlim = range(kruskal$points[, 1]) + c(-8, 5),
     ylim = range(kruskal$points[, 2]),
     asp = 1)
text(kruskal$points[, 1], kruskal$points[, 2],
     rownames(mutations))
dev.off()
cat(pngnameKruskal, 'sauvegardee\n\n')

# computes Shepard's diagrams
shepardAftd <- Shepard(dist(mutationsMatrix), aftd$points)
shepardSammon <- Shepard(dist(mutationsMatrix), sammon$points)
shepardKruskal <- Shepard(dist(mutationsMatrix), kruskal$points)

pngnameShepard <- 'shepard.png'
png(pngnameShepard, width = 300)
par(mfrow = c(3, 1))
plot(shepardSammon,
     main = 'Diagramme de Shepard de la projection de Sammon',
     pch = '.',
     xlab = 'Dissimilarite',
     ylab = 'Distance',
     xlim = range(shepardSammon$x),
     ylim = range(shepardSammon$y))
lines(shepardSammon$x, shepardSammon$yf,
      type = 'S')
plot(shepardKruskal,
     main = 'Diagramme de Shepard de la projection de Kruskal',
     pch = '.',
     xlab = 'Dissimilarite',
     ylab = 'Distance',
     xlim = range(shepardKruskal$x),
     ylim = range(shepardKruskal$y))
lines(shepardKruskal$x, shepardKruskal$yf,
      type = 'S')
plot(shepardAftd,
     main = 'Diagramme de Shepard de l AFTD initiale',
     pch = '.',
     xlab = 'Dissimilarite',
     ylab = 'Distance',
     xlim = range(shepardAftd$x),
     ylim = range(shepardAftd$y))
lines(shepardAftd$x, shepardAftd$yf,
      type = 'S')
dev.off()
cat(pngnameShepard, 'sauvegardee\n\n')
