# Author: Mateusz Kiebala 359758

library(tree)
library(e1071)
library(kknn)
library(ROCR)

data <- read.table(file = "dane.csv", header = TRUE, sep = ",")

compute <- function (n) {
  # Mieszamy dane
  data <- data[sample(nrow(data)),]
  sample.size = nrow(data) / n
  
  # Wektory wynikowe. Miary skutecznosci algorytmow: auc i blad empiryczny
  tree.res = data.frame(auc = vector(length = n), err = vector(length = n))
  knn.res = data.frame(auc = vector(length = n), err = vector(length = n))
  bayes.res = data.frame(auc = vector(length = n), err = vector(length = n))
  
  # Kroswalidacja
  for (i in 1:n) {
    # Wybieramy zbior treningowy i testowy. Takie same dla wszystkich algorytmow.
    sample = (((i-1)*sample.size + 1) : (i*sample.size))
    data.train = data[-sample,]
    data.test = data[sample,]
    
    # Drzewa decyzyjne
    treeClassModel = tree(as.factor(D)~., data.train)
    tree.prediction = predict(treeClassModel, data.test, type = 'class')
    tree.res$err[i] = 1 - sum(diag(table(data.test$D, tree.prediction))) / sample.size
    
    treeFitModel = tree(D~., data.train)
    tree.scores = predict(treeFitModel, data.test)
    tree.preds = prediction(tree.scores, data.test$D)
    tree.res$auc[i] = slot(performance(tree.preds, measure="auc"), "y.values")
    
    # KNN
    knnClassModel = train.kknn(as.factor(D)~., data.train, kmax = 21)
    knn.class.prediction = predict(knnClassModel, data.test)
    knn.res$err[i] = 1 - sum(diag(table(data.test$D, knn.class.prediction))) / sample.size
    
    knnFitModel = train.kknn(D~., data.train, kmax = 21)
    knn.scores = predict(knnFitModel, data.test)
    knn.preds = prediction(knn.scores, data.test$D)
    knn.res$auc[i] = slot(performance(knn.preds, measure="auc"), "y.values")
    
    # Naive Bayes
    bayesClassModel = naiveBayes(as.factor(D)~., data.train)
    bayes.prediction = predict(bayesClassModel, data.test, type = "class")
    bayes.res$err[i] = 1 - sum(diag(table(data.test$D, bayes.prediction))) / sample.size
    
    bayesFitModel = naiveBayes(D~., data.train)
    bayes.scores = predict(bayesFitModel, data.test, type = 'raw')
    bayes.preds = prediction(bayes.scores[,"1"], data.test$D)
    bayes.res$auc[i] = slot(performance(bayes.preds, measure="auc"), "y.values")
  }
  
  data.frame(tree.auc = mean(as.numeric(tree.res$auc)),
             tree.err = mean(as.numeric(tree.res$err)),
             knn.auc = mean(as.numeric(knn.res$auc)),
             knn.err = mean(as.numeric(knn.res$err)),
             bayes.auc = mean(as.numeric(bayes.res$auc)),
             bayes.err = mean(as.numeric(bayes.res$err)))
}

# Liczba powtorzen kroswalidacji dla kazdego n
iters = 5
# Lista n do kroswalidacji
nList = c(10, 20, 50, 100)
nCrossList = rep(nList, each = iters)
# Wyniki kroswalidacji
crossResults = data.frame()
for (n in nCrossList) {
  crossResults = rbind(crossResults, compute(n))
}
row.names(crossResults) = paste(nCrossList, rep(1:iters, length(nList)), sep = "_")

# Liczenie sredniej z wynikow dla danej n-kroswalidacji
results = data.frame()
for (i in 1:length(nList)) {
  results = rbind(results, apply(crossResults[((i-1)*iters+1):(i*iters),], mean, MARGIN = 2))
}
row.names(results) = nList
colnames(results) = colnames(crossResults)
