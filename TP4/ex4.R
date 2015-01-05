library(e1071)
library(gmodels)

bcdata <- read.csv('breast-cancer-wisconsin.data', head = T)

# removes useless columns
databc <- subset(bcdata, select = c(-Samplecodenumber, -Class))

# classes
classesbc <- subset(bcdata, select = Class)

# training set
databcTrain <- databc[1:400, ]
classesbcTrain <- classesbc[1:400, ]
nTrain <- length(classesbcTrain)

# testing set
databcTest <- databc[401:699, ]
classesbcTest <- classesbc[401:699, ]
nTest <- length(classesbcTest)

# q2
costs <- 1:10
probas <- c()
coefs <- list()
notNullCoefs <- c()
for (c in costs) {
    model <- svm(databcTrain, classesbcTrain, kernel = 'radial', cost = c)
    pred <- predict(model, databcTest)
    probas[c] <- length(which(pred != classesbcTest)) / nTest
    coefs[[c]] <- model$coefs[, 1]
    notNullCoefs[c] <- length(model$coefs[, 1])
}

pngName <- 'probaErrorEx4.png'
png(pngName)
plot(costs, probas,
     xlab = 'Coût',
     ylab = 'Erreur moyenne',
     main = paste('Erreur moyenne de classification sur l\'ensemble de test',
            'en fonction du paramètre de pénalisation', sep = '\n'))
dev.off()
cat(pngName, 'sauvegardee\n')

pngName <- 'boxplotsCoefsEx4.png'
png(pngName)
boxplot(coefs[[1]], coefs[[2]], coefs[[3]], coefs[[4]], coefs[[5]],
        coefs[[6]], coefs[[7]], coefs[[8]], coefs[[9]], coefs[[10]],
        names = costs,
        xlab = 'Coût',
        ylab = 'Composantes du vecteur alpha',
        main = paste('Boîtes à moustaches des composantes du vecteur alpha',
                     'en fonction du paramètre de pénalisation', sep = '\n'))
dev.off()
cat(pngName, 'sauvegardee\n')

pngName <- 'notNullCoefsEx4.png'
png(pngName)
plot(costs, notNullCoefs,
     xlab = 'Coût',
     ylab = 'Nombre d\'exemples ayant des composantes alpha non nulles',
     main = paste('Nombre d\'exemples ayant un alpha non nul',
                  'en fonction du paramètre de pénalisation', sep = '\n'))
dev.off()
cat(pngName, 'sauvegardee\n')

# q3
gammas <- -4:5
probas <- c()
notNullCoefs <- c()
for (g in gammas) {
    model <- svm(databcTrain, classesbcTrain, kernel = 'radial', cost = 1000,
                gamma = 10^g)
    pred <- predict(model, databcTest)
    probas[g + 5] <- length(which(pred != classesbcTest)) / nTest
    notNullCoefs[g + 5] <- length(model$coefs[, 1])
}

pngName <- 'probaErrorEx43.png'
png(pngName)
plot(gammas, probas,
     xlab = 'Largeur de bande (10^x)',
     ylab = 'Erreur moyenne',
     main = paste('Erreur moyenne de classification sur l\'ensemble de test',
            'en fonction de la largeur de bande', sep = '\n'))
dev.off()
cat(pngName, 'sauvegardee\n')

pngName <- 'notNullCoefsEx43.png'
png(pngName)
plot(gammas, notNullCoefs,
     xlab = 'Largeur de bande (10^x)',
     ylab = 'Nombre d\'exemples ayant des composantes alpha non nulles',
     main = paste('Nombre d\'exemples ayant un alpha non nul',
                  'en fonction de la largeur de bande', sep = '\n'))
dev.off()
cat(pngName, 'sauvegardee\n')

# q4
tuneGaussian <- tune(svm, train.x = databcTrain, train.y = classesbcTrain,
                     validation.x = databcTest, validation.y = classesbcTest,
                     ranges = list(gamma = 10^(-4:5)),
                     control = tune.control(sampling = 'fix'),
                     cost = 1, kernel = 'radial')
model <- svm(databcTrain, classesbcTrain, kernel = 'radial', cost = 1,
             gamma = tuneGaussian$best.parameter$gamma)
pred <- predict(model, databcTest)
CrossTable(x = classesbcTest, y = pred,
           prop.chisq = F, dnn = c('actual', 'predicted'))

tunePoly <- tune(svm, train.x = databcTrain, train.y = classesbcTrain,
                 validation.x = databcTest, validation.y = classesbcTest,
                 ranges = list(gamma = 10^-4:5, degree = 1:10, coef0 = 1:10),
                 control = tune.control(sampling = 'fix'),
                 cost = 1, kernel = 'polynomial')
model <- svm(databcTrain, classesbcTrain, kernel = 'polynomial', cost = 1,
             gamma = tunePoly$best.parameter$gamma,
             degree = tunePoly$best.parameter$degree,
             coef0 = tunePoly$best.parameter$coef0)
pred <- predict(model, databcTest)
CrossTable(x = classesbcTest, y = pred,
           prop.chisq = F, dnn = c('actual', 'predicted'))
