library(e1071)

# matrice des exemples
x <- matrix(c(2, 0, 0, 2, -2, 2, -1, 3), nrow = 4, byrow = T)
# matrices des classes associees a ces exemples
y <- factor(c(-1, 1, 1, 1))
df <- data.frame(x1 = x[, 1], x2 = x[, 2], class = y)
# creation du modele grace a la fonction svm
model <- svm(class ~ x1 + x2, data = df, scale = F, kernel = 'linear')

# affichage des resultats
cat('support vectors: ', model$SV, '\n')
cat('alpha(i) * y(i): ', model$coefs, '\n')
cat('w: ', t(model$coefs) %*% as.matrix(df[model$index, c(1, 2)]), '\n')
cat('w0: ', -model$rho, '\n')

pngName <- 'svmEx1.png'
png(pngName)
plot(model, df,
     formula = x2 ~ x1,
     xlab = 'x',
     ylab = 'y',
     main = 'Régions de décision produites par SVM')
dev.off()
cat(pngName, 'sauvegardee\n')
