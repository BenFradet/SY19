library(e1071)

x <- c(1, 2, 6, 4, 5)
y <- factor(c(1, 1, 1, -1, -1))
df <- data.frame(x = x, class = y)
model <- svm(class ~ x, data = df, scale = F, cost = 100,
             kernel = 'polynomial', gamma = 1, coef0 = 1, degree = 2)

cat('support vectors: ', model$SV, '\n')
cat('alpha(i) * y(i): ', model$coefs, '\n')
cat('w: ', t(model$coefs) %*% as.matrix(df[model$index, 1]), '\n')
cat('w0: ', -model$rho, '\n')

pngName <- 'svmEx4.png'
png(pngName)
plot(x, rep(0, 5),
     xlab = 'x',
     ylab = 'y',
     main = 'Régions de décision produites par SVM')
dev.off()
cat(pngName, 'sauvegardee\n')
