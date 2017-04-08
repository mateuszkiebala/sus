setwd("/Users/mateuszkiebala/Documents/studia/sus/sus/puczlu")
library(nnet)
library(AMORE)

dataSet = read.table(file = file.choose(getwd()), header = F, sep=',', row.names=1, na.strings="?")
colnames(dataSet) = c("diagnosis",
                      paste(c(rep("mean",10), rep("SE",10), rep("worst",10)),
                            rep(c("radius", "texture", "perimeter", "area",
                                  "smoothness", "compactness", "concavity",
                                  "concave_points", "symmetry", "fractal_dimension"),3),
                            sep="_"))
clsVec = dataSet[[1]]
dataSet = dataSet[-1]
clsVec = sapply(clsVec, function(x) if (x == "M") 1 else -1)

#Weryfikacja krzyzowa (cross-validation):
crossValidation = function(dataSet, clsVec, K) {
  K = 10
  #Tworzymy wektor definiujacy podzial danych na K mniej wiecej rownolicznych zbiorow
  permutationVec = sample(c(rep(1:K, each = nrow(dataSet) %/% K), 
                            (1:K)[min(nrow(dataSet),1):(nrow(dataSet) %% K)]), 
                          nrow(dataSet))
  
  #Dzielimy dane
  dataList = split(dataSet, permutationVec)
  clsList = split(clsVec, permutationVec)
  idxVec = split(1:nrow(dataSet), permutationVec)
  
  #Dokonujemy klasyfikacji wszystkich obiektow z danych
  predsNnet = numeric(nrow(dataSet))
  predsAMORE = numeric(nrow(dataSet))
  
  for(i in 1:K) {
    #Dzielimy na zbior treningowy, testowy.
    trainingSet = unsplit(dataList[-i], permutationVec[!(permutationVec == i)])
    testSet = dataList[[i]]
    clsTr = unsplit(clsList[-i], permutationVec[!(permutationVec == i)])
    
    #Tworzymy modele
    myNnet = nnet(trainingSet, clsTr, size = 5, rang = 0.1,
               decay = 5e-4, maxit = 200)
    
    myAmoreNet = newff(n.neurons = c(2,3,1), learning.rate.global = 0.01, momentum.global = 0.5,
                   error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")
    print(is(trainingSet))
    print(is(clsTr))
    myAmoreNetModel = train(myAmoreNet, trainingSet, clsTr, report=T, n.shows = 100, show.step = 100)
    
    #Liczymy predykcje
    preds = predict(myNnet, testSet)
    preds[preds > 0.5] = 1
    preds[preds <= 0.5] = -1
    predsNnet[idxVec[[i]]] = preds
    
    preds = sim.MLPnet(myNNetModel$net,myRandomData)
    preds[preds > 0.5] = 1
    preds[preds <= 0.5] = -1
    predsAMORE[idxVec[[i]]] = preds
  }
  list(predsNnet, predsAMORE)
}

predictions = crossValidation(dataSet, clsVec, 10)