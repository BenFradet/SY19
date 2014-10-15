library(MASS)

graph <- function(data, name) {
    aftd <- cmdscale(data, k = 2, eig = T)

    aftdQuality <- 100 * sum(aftd$eig[c(1, 2)]) / sum(aftd$eig[aftd$eig > 0])
    cat('qualite 2 axes', aftdQuality, '\n\n')

    aftdQuality <- 100 * sum(aftd$eig[c(1, 2, 3)]) / sum(aftd$eig[aftd$eig > 0])
    cat('qualite 3 axes', aftdQuality, '\n\n')

    pngname <- paste(substitute(data), 'Aftd.png', sep = '')
    png(pngname)
    plot(aftd$point[, 1], aftd$points[, 2],
         type = 'n',
         main = paste('Representation de l AFTD des donnees', name),
         xlab = '',
         ylab = '',
         xlim = range(aftd$points[, 1]) + c(-1000, 300))
    text(aftd$points[, 1], aftd$points[, 2],
         rownames(data))
    dev.off()
    cat(pngname, 'sauvegardee\n\n')

    mat <- as.matrix(data)

    sam<- sammon(mat, aftd$points, k = 2)
    pngnameSammon <- paste(substitute(data), 'Sammon.png', sep = '')
    png(pngnameSammon)
    plot(sam$points[, 1], sam$points[, 2],
         type = 'n',
         main = paste('Projection de Sammon des donnees', name),
         xlab = '',
         ylab = '',
         xlim = range(sam$points[, 1]) + c(-1000, 300))
    text(sam$points[, 1], sam$points[, 2],
         rownames(data))
    dev.off()
    cat(pngnameSammon, 'sauvegardee\n\n')

    kruskal <- isoMDS(mat, aftd$points, k = 2)
    pngnameKruskal <- paste(substitute(data), 'Kruskal.png', sep = '')
    png(pngnameKruskal)
    plot(kruskal$points[, 1], kruskal$points[, 2],
         type = 'n',
         main = paste('Projection de Kruskal des donnees', name),
         xlab = '',
         ylab = '',
         xlim = range(kruskal$points[, 1]) + c(-1000, 300))
    text(kruskal$points[, 1], kruskal$points[, 2],
         rownames(data))
    dev.off()
    cat(pngnameKruskal, 'sauvegardee\n\n')
}

airports <- read.table('airports2.txt', header = F, row.names = 1)
airports.name <- 'aeroports'
graph(airports, airports.name)

europeansAirportsNames <- c('Athenes', 'Berlin', 'Copenhague', 'Dublin',
                            'Londres', 'Madrid', 'Paris',
                            'Rome', 'Vienne')
europeansAirports <- airports[europeansAirportsNames,
                              which(rownames(airports) %in%
                                    europeansAirportsNames)]
europeansAirports.name <- 'aeroports europeens'
graph(europeansAirports, europeansAirports.name)
