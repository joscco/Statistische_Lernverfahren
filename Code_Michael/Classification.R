library(caret)
library(e1071)
library(nnet)
library(neuralnet)
library(randomForest)

col_count <- ncol(train)
x <- train[, 1:nrow(dictionary2)]
y <- train$type
testData <- test[, 1:nrow(dictionary2)]
# View(testData)

# PCA-Test
pca <- prcomp(x, scale = TRUE)
pca
biplot(pca)

# SVM-Test
dat = data.frame(x, y = as.factor(y))
# colnames(dat)[colnames(dat)=="X0"] <- "0"

svm1 <- svm(x, y,
            method="C-classification", kernal="radial",
            cost = 1)
svm2 <- svm(y~., data = dat,
            method="C-classification", kernal="radial",
            cost = 1)
plot(svm2, dat, die ~ und)
predict_svm <- predict(svm1, newdata = testData)
confusionMatrix(predict_svm, test$type)

# NeuralNet-Test
feats <- names(x)
# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('y ~',f)
# Convert to formula
f <- as.formula(f)
f

model_nnet <- neuralnet(f, data = dat, linear.output = FALSE)

confusionMatrix(predict_nnet, test$type)
predict_nnet <- compute(model_nnet, testData)
predict_nnet

# nb, rpart, rf, nnet
model_nb <- train(x, y, 'nb', trControl = trainControl(method = 'cv', number = 10))
model_rpart <- train(x, y, 'rpart', trControl = trainControl(method = 'cv', number = 10))
model_rf <- train(x, y, 'rf', trControl = trainControl(method = 'cv', number = 10))
model_rf <- randomForest(x, y, ntree = 10000)

model_nnet <- train(x, y, 'nnet', trControl = trainControl(method = 'cv', number = 10))

predict_nb <- predict(model_nb, newdata = testData)
predict_rpart <- predict(model_rpart, newdata = testData)
predict_rf <- predict(model_rf, newdata = testData)
predict_nnet <- predict(model_nnet, newdata = testData)

confusionMatrix(predict_nb, test$type)

modelle <- names(getModelInfo())

n <- length(predict_nb)
y_test <- test$type

calculatePerformance <- function(testData, predictedData) {
  typen <- levels(predictedData)
  l <- length(typen)
  n <- length(testData)
  
  # Erzeuge Dataframe
  performance <- data.frame(Typ = character(l), Rp = integer(l),
                            Rn = integer(l), Fp = integer(l),
                            Fn = integer(l), acc = double(l),
                            prec = double(l), rec = double(l),
                            f1 = double(l), freq = double(l),
                            stringsAsFactors = FALSE)
  
  i = 1
  for(typus in typen) {
    performance$Typ[i] <- typus
    
    # Anzahl der richtig positiven Faelle
    Rp <- length(predictedData[predictedData == typus & testData == typus])
    performance$Rp[i] <- Rp
    
    # Anzahl der richtig negativen Faelle
    Rn <- length(predictedData[predictedData != typus & testData != typus])
    performance$Rn[i] <- Rn
    
    # Anzahl der falsch positiven Faelle
    Fp <- length(predictedData[predictedData == typus & testData != typus])
    performance$Fp[i] <- Fp
    
    # Anzahl der falsch negativen Faelle
    Fn <- length(predictedData[predictedData != typus & testData == typus])
    performance$Fn[i] <- Fn
    
    # Accuracy
    acc = (Rp+Rn)/n
    performance$acc[i] <- acc
    
    # Precision
    if(Rp+Fp == 0) {
      prec <- 0
    }
    else {
      prec <- Rp/(Rp+Fp)
    }
    performance$prec[i] <- prec
    
    # Recall
    if(Rp+Fn == 0) {
      rec <- 0
    }
    else {
      rec <- Rp/(Rp+Fn)
    }
    performance$rec[i] <- rec
    
    # F1
    if(prec+rec == 0) {
      f1 <- 0
    }
    else {
      f1 <- 2*(prec*rec)/(prec+rec)
    }
    performance$f1[i] <- f1
    
    # Haeufigkeit der Beobachtungen im Testdatensatz
    performance$freq[i] <- length(testData[testData == typus])/n
    
    # print(sprintf("%s: Rp = %d, Rn = %d, Accuracy = %f, Precision = %f, Recall = %f, F1 = %f", typus, Rp, Rn, acc, prec, rec, f1))
    
    i <- i + 1
  }
  performance$Typ <- as.factor(performance$Typ)
  
  # Berechne nun Macro-Werte
  weighted_f1 <- weighted.mean(performance$f1, performance$freq)
  macro <- data.frame("acc" = mean(performance$acc), "prec" = mean(performance$prec),
                      "rec" = mean(performance$rec), "f1" = mean(performance$f1),
                      "weighted_f1" = weighted_f1)
  
  return(list("performance" = performance, "macro" = macro))
}
nb_performance <- calculatePerformance(y_test, predict_nb)
nb_performance

rpart_performance <- calculatePerformance(y_test, predict_rpart)
rpart_performance

rf_performance <- calculatePerformance(y_test, predict_rf)
rf_performance

nnet_performance <- calculatePerformance(y_test, predict_nnet)
nnet_performance

svm_performance <- calculatePerformance(y_test, predict_svm)
svm_performance

#for(modell in modelle) {
#  vorhersage <- predict(train(x, y, modell, trControl = trainControl(method = 'cv', number = 10)), newdata = testData)
#  acc <- confusionMatrix(predict_rf, test$type)$overall["Accuracy"]
#  print(sprintf("%s: Modell = %s, Accuracy = %d", modell, acc))
#}
