library(MASS)

graph <- function(data, name) {
    aftd <- cmdscale(data, k = 2)
    pngname <- paste(substitute(data), 'Aftd.png', sep = '')
    png(pngname)
    plot(aftd[, 1], aftd[, 2],
         type = 'n',
         main = paste('Representation de l AFTD des donnees', name),
         xlab = '',
         ylab = '',
         asp = 1)
    text(aftd[, 1], aftd[, 2],
         rownames(data),
         cex = 0.8)
    dev.off()
    cat(pngname, 'sauvegardee\n\n')

    mat <- as.matrix(data)

    sammon <- sammon(mat, aftd, k = 2)
    pngnameSammon <- paste(substitute(data), 'Sammon.png', sep = '')
    png(pngnameSammon, width = 500, height = 500)
    plot(sammon$points[, 1], sammon$points[, 2],
         type = 'n',
         main = paste('Projection de Sammon des donnees', name),
         xlab = '',
         ylab = '',
         xlim = range(sammon$points[, 1]),
         ylim = range(sammon$points[, 2]))
    text(sammon$points[, 1], sammon$points[, 2],
         rownames(data),
         cex = 0.8)
    dev.off()
    cat(pngnameSammon, 'sauvegardee\n\n')

    kruskal <- isoMDS(mat, aftd, k = 2)
    pngnameKruskal <- paste(substitute(data), 'Kruskal.png', sep = '')
    png(pngnameKruskal, width = 500, height = 500)
    plot(kruskal$points[, 1], kruskal$points[, 2],
         type = 'n',
         main = paste('Projection de Kruskal des donnees', name),
         xlab = '',
         ylab = '',
         xlim = range(kruskal$points[, 1]),
         ylim = range(kruskal$points[, 2]))
    text(kruskal$points[, 1], kruskal$points[, 2],
         rownames(data),
         cex = 0.8)
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
