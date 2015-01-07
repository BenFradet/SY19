library(e1071)

# matrice des exemples
x <- c(1, 2, 6, 4, 5)
# matrice des classes associees a ces exemples
y <- factor(c(1, 1, 1, -1, -1))
df <- data.frame(x = x, class = y)
# creation du modele grace a la fonction svm
model <- svm(class ~ x, data = df, scale = F, cost = 100,
             kernel = 'polynomial', gamma = 1, coef0 = 1, degree = 2)

# affichage des resultats
cat('support vectors: ', model$SV, '\n')
cat('alpha(i) * y(i): ', model$coefs, '\n')
cat('w: ', t(model$coefs) %*% as.matrix(df[model$index, 1]), '\n')
cat('w0: ', -model$rho, '\n')

pngName <- 'svmEx3.png'
png(pngName)
color <- rep('black', 5)
color[model$index] <- 'red'
plot(x, rep(0, 5),
     pch = as.numeric(y) + 2,
     col = color,
     xlim = c(0, 7),
     ylim = c(-2, 2),
     xlab = 'x',
     ylab = '',
     main = 'Régions de décision produites par SVM')
curve(0.663 * x^2 - 5.334 * x + 8.99, add = T, col = 'blue')
text(4, 1, 'Région où l\'on\nprédit C2 (y = -1)')
text(1, -1.5, 'Région où l\'on\nprédit C1 (y = 1)')
dev.off()
cat(pngName, 'sauvegardee\n')
