png('ex1.png')
plot(c(0, 1.5), c(1.5, 0),
     type = 'l',
     xlab = 'x1',
     ylab = 'y1',
     main = 'Frontières de décision')
lines(c(0, 0.5), c(0.5, 0))
lines(c(0, 2.5), c(2.5, 0))
points(c(0, 1), c(0, 1), pch = 1)
points(c(0, 1), c(1, 0), pch = 3)
text(1.25, 0.75, 'R1')
text(0.5, 0.5, 'R2')
text(0.15, 0.15, 'R3')
text(1.35, 1.35, 'R4')
dev.off()
