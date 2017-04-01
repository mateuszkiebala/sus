# Author: Mateusz Kiebala 359758

library(tree)
library(e1071)
library(kknn)
library(ROCR)

set.seed(1234)
data <- read.table(file = "dane.csv", header = TRUE, sep = ",")
data <- data[sample(nrow(data)),]

calculate <- function (n) {
  sample.size = nrow(data) / n
  
  # Results vectors
  tree.res = data.frame(auc = vector(length = n), acc = vector(length = n))
  knn.res = data.frame(auc = vector(length = n), acc = vector(length = n))
  bayes.res = data.frame(auc = vector(length = n), acc = vector(length = n))
  
  # Crossvalidation
  for (i in 1:n) {
    # Select train and test set. Same for all algorithms.
    sample = (((i-1)*sample.size + 1) : (i*sample.size))
    data.train <- data[-sample,]
    data.test <- data[sample,]
    
    # Decision tree
    treeClassModel = tree(as.factor(D)~., data.train)
    tree.prediction = predict(treeClassModel, data.test, type = 'class')
    tree.res$acc[i] = sum(diag(table(data.test$D, tree.prediction))) / sample.size
    
    treeFitModel = tree(D~., data.train)
    tree.scores = predict(treeFitModel, data.test)
    tree.preds = prediction(tree.scores, data.test$D)
    tree.res$auc[i] = slot(performance(tree.preds, measure="auc"), "y.values")
    
    # KNN
    knnClassModel = train.kknn(as.factor(D)~., data.train, kmax = 21)
    knn.class.prediction = predict(knnClassModel, data.test)
    knn.res$acc[i] = sum(diag(table(data.test$D, knn.class.prediction))) / sample.size
    
    knnFitModel = train.kknn(D~., data.train, kmax = 21)
    knn.scores = predict(knnFitModel, data.test)
    knn.preds = prediction(knn.scores, data.test$D)
    knn.res$auc[i] = slot(performance(knn.preds, measure="auc"), "y.values")
    
    # Naive Bayes
    bayesClassModel = naiveBayes(as.factor(D)~., data.train)
    bayes.prediction = predict(bayesClassModel, data.test, type = "class")
    bayes.res$acc[i] = sum(diag(table(data.test$D, bayes.prediction))) / sample.size
    
    bayesFitModel = naiveBayes(D~., data.train)
    bayes.scores = predict(bayesFitModel, data.test, type = 'raw')
    bayes.preds = prediction(bayes.scores[,"1"], data.test$D)
    bayes.res$auc[i] = slot(performance(bayes.preds, measure="auc"), "y.values")
  }
  
  data.frame(tree.auc = mean(as.numeric(tree.res$auc)),
             tree.acc = mean(as.numeric(tree.res$acc)),
             knn.auc = mean(as.numeric(knn.res$auc)),
             knn.acc = mean(as.numeric(knn.res$acc)),
             bayes.auc = mean(as.numeric(bayes.res$auc)),
             bayes.acc = mean(as.numeric(bayes.res$acc)))
}

nList = c(10, 20, 50, 100)
results = data.frame()
for (i in 1:length(nList)) {
  results = rbind(results, calculate(nList[i]))
}
row.names(results) = nList
