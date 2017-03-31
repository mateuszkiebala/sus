# Author: Mateusz Kiebala 359758

#install.packages("tree")
#install.packages("class")
#install.packages("e1071")
#install.packages("ROCR")
library(tree)
library(class)
library(e1071)
library(ROCR)

set.seed(1234)
data <- read.table(file = "dane.csv", header = TRUE, sep = ",")
data <- data[sample(nrow(data)),]

calculate <- function (n) {
  print (n)
  sample.size = nrow(data) / n
  tree.res = data.frame(auc = vector(length = n))
  knn.res = data.frame(auc = vector(length = n))
  bayes.res = data.frame(auc = vector(length = n))
  for (i in 1:n) {
    sample = (((i-1)*sample.size + 1) : (i*sample.size))
    data.train = data[-sample,]
    data.test = data[sample,]
    
    # Decision tree
    treeModel = tree(as.factor(D)~., data.train)
    tree.prediction = predict(treeModel, data.test, type='class')
    preds = prediction(as.numeric(tree.prediction) - 1, data.test$D)
    tree.res$auc[i] = slot(performance(preds, measure="auc"), "y.values")
    
    # KNN
    knn.prediction = knn(data.train[,1:10], data.test[,1:10], data.train$D, k=50)
    preds = prediction(as.numeric(knn.prediction) - 1, data.test$D)
    knn.res$auc[i] = slot(performance(preds, measure="auc"), "y.values")
    
    # Naive Bayes
    naiveBayesModel = naiveBayes(as.factor(D)~., data.train)
    bayes.prediction = predict(naiveBayesModel, data.test, type="class")
    preds = prediction(as.numeric(bayes.prediction) - 1, data.test$D)
    bayes.res$auc[i] = slot(performance(preds, measure="auc"), "y.values")
  }
  
  c(mean(as.numeric(tree.res$auc)), mean(as.numeric(knn.res$auc)), mean(as.numeric(bayes.res$auc)))
}

res <- sapply(c(2,4,8,10,16,20,25,32,40,50,100,200), calculate)
print ("Decision tree auc summary:")
print (summary(res[1,]))
print ("KNN auc summary:")
print (summary(res[2,]))
print ("Naive Bayes auc summary:")
print (summary(res[3,]))