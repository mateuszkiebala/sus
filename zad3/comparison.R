CROSS_VALIDATION = FALSE

library(e1071)
library(class)
library(xgboost)

print("Reading data")

readData = function() {
  dataSet = read.csv("letter-recognition-train.csv", sep=",", header=TRUE)
  clsVec = dataSet[, "letter"]
  dataSet = subset(dataSet, select=-c(letter))
  list(dataSet, clsVec)
}

datas <- readData()

dataSet = datas[[1]]
clsVec = as.vector(datas[[2]])
letters = sort(unique(clsVec))

predictXgboostForCols = function(trainingSet, clsTr, testSet) {
  trainingSet = apply(trainingSet, 2, as.numeric)
  testSet = apply(testSet, 2, as.numeric)
  clsTr = as.numeric(clsTr)
  dtrain = xgb.DMatrix(data = trainingSet, label = clsTr)
  watchlist = list(train = dtrain)
  nc = max(clsTr) + 1
  bst = xgb.train(data=dtrain, max_depth=15, eta=0.8, nthread = 20, nrounds=50, watchlist=watchlist, objective = "multi:softprob", num_class=nc)
  r = predict(bst, testSet)
  predictionMatrix = matrix(r, nrow = nrow(testSet), byrow=TRUE)
  predictions = apply(predictionMatrix, 1, function(x) {which(x == max(x))[1] - 1})
  predictions
}

predictGLM = function(trainingSet, clsTr, testSet) {
  trainingSet = apply(trainingSet, 2, as.numeric)
  testSet = apply(testSet, 2, as.numeric)
  print(max(trainingSet[,1]))
  clsTr = as.numeric(clsTr)
  colsNo = ncol(trainingSet)
  h2oData = as.h2o(cbind(trainingSet, clsTr))
  model = h2o.glm(1:colsNo, colsNo + 1, h2oData, family="gaussian")
  
  h2oDataTest = as.h2o(testSet)
  
  result = h2o.predict(model, h2oDataTest)
  print(result)
  preds = round(as.vector(result))
  print(preds)
  preds
}

predictColumn = function(colId) {
  trainIdx = sort(which(dataSet[, colId] != "?"))
  testIdx = sort(which(dataSet[, colId] == "?"))
  trainingSet = dataSet[trainIdx, 1:5]
  clsTr = dataSet[trainIdx, colId]
  testSet = dataSet[testIdx, 1:5]

  predictions = predictGLM(trainingSet, clsTr, testSet)
  print(predictions)
  ret = character(nrow(dataSet))
  ret[trainIdx] = clsTr
  ret[testIdx] = as.character(predictions)
  
  print(ret)
  ret
}

predictUnknownValues = function() {
  dataCopy = dataSet
  for (i in 6:15) {
    dataCopy[, i] <- predictColumn(i)
  }
  dataCopy
}

h2o.init(nthreads = -1)
dataSet = apply(dataSet, 2, as.character)
dataSet = predictUnknownValues()
print("Read successfully")

predictKNNforK = function(trainingSet, clsTr, testSet, K) {
  predictions = knn(trainingSet, testSet, clsTr, k = K, prob = T, use.all = F)
  as.numeric(levels(predictions))[predictions]
}

predictKNN3 = function(trainingSet, clsTr, testSet) {
  predictKNNforK(trainingSet, clsTr, testSet, 3)
}

predictXgboost = function(trainingSet, clsTr, testSet) {
  trainingSet = apply(trainingSet, 2, as.numeric)
  testSet = apply(testSet, 2, as.numeric)
  clsTr = as.numeric(clsTr)
  dtrain = xgb.DMatrix(data = trainingSet, label = clsTr)
  watchlist = list(train = dtrain)
  bst = xgb.train(data=dtrain, max_depth=50, eta=0.5, nthread = 20, nrounds=50, watchlist=watchlist, objective = "binary:logistic")
  predict(bst, testSet)
}

classifiers = list(predictXgboost)

makePrediction = function(dataSet, clsTr, testSet, classifier) {
  tmpClsTr = clsTr
  bestScores = numeric(nrow(testSet))
  preds = character(nrow(testSet))
  for (letter in letters) {
    print("Now letter")
    print(letter)
    clsTr = as.numeric((tmpClsTr == letter))
    predScores = (classifier)(dataSet, clsTr, testSet)
    for (i in 1:nrow(testSet)) {
      if (bestScores[i] < predScores[i]) {
        bestScores[i] = predScores[i]
        preds[i] = letter
      }
    }
  }
  preds
}

# testSet = read.csv("letter-recognition-test-without-decisions.csv", sep=",")
# testSet = as.data.frame(lapply(testSet, function(y) sapply(y, function(x) if (x == "?") {NA} else {x})))
# testSet = testSet[, -(1:2)]

# trainingSet = dataSet
# clsTr = clsVec

trainingIdx = sample(1:nrow(dataSet), round(3*(nrow(dataSet)/5)))
trainingSet = dataSet[trainingIdx,]
testSet = dataSet[-trainingIdx,]

clsTr = clsVec[trainingIdx]
clsTe = clsVec[-trainingIdx]

prediction = makePrediction(trainingSet, clsTr, testSet, predictXgboost)
print(mean(prediction == clsTe))

fileConn = file("prediction")
writeLines(as.character(prediction), fileConn)
