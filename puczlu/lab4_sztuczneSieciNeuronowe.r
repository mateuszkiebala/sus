#lab9: perceptron, sieci neuronowe (dla problemu predykcji)
#autor: Andrzej Janusz
#email: janusza@mimuw.edu.pl

#1. Konstrukcja perceptronu.
#2. Funkcje aktywacji.
#3. Siec neuronowa.
#4. Sieci neuronowe w R.

#Perceptron:
#W najprostrzej wersji perceptron jest modelem neuronu. Posiada N wejsc, jedno wyjscie i funkcje aktywacji.
#Z kazdym wejsciem stowarzyszony jest parametr określający jego wage (najczesciej przyjmujaca wartosci z 
#przedzialu [-1,1]). Dodatkowo, czesto dodaje sie jedna wage nie zwiazana z zadnym wejsciem ("bias factor").
#Perceptron sumuje wazone sygnaly wejsciowe i na wyjsciu zwraca wartosc funkcji aktywacji od obliczonej sumy.
#Funkcje aktywacji moga byc roznego typu, lecz najczesciej uzywa sie rozniczkowalnych funkcji sigmoidalnych,
#np. funkcji logistycznej: 
#sigmoid(x) = 1/(1 + e^(-x)) lub tangensa hiperbolicznego: 
#tanh(x) = (exp(x) - exp(-x))/(exp(x) + exp(-x))

library(e1071)
?sigmoid
?dsigmoid

plot(sigmoid(seq(-10,10,length.out=100)),type="l",lwd=2)

#Prosta reprezentacja neuronu:
createNeuron = function(nIn, activationF = sigmoid, activationDer = dsigmoid, range = 0.1)
                          return(list(weights = runif(nIn + 1, -range, range), 
                                      fun = activationF, derivative = activationDer))

neuron = createNeuron(2, range = 0.5)

#Uczenie perceptronu - metoda gradientowa (prosze porownac z metoda optymalizacji omawiana na zajeciach numer 6-7)
#sposób uaktualniania wag w kolejnych iteracjach:
#deltaW(i) = alpha * (truth - f(W %*% X) ) * f'(W %*% X) * X_i, gdzie
#deltaW(i) - uaktualnienie i-tej wagi
#W - wektor wag neuronu
#X = <X_1, ..., X_N> - wejscie dla neuronu
#f oraz f' - funkcja aktywacji oraz jej pochodna
#alpha - parametr regulujący szybkość uczenia

myRandomData = matrix(rnorm(200), 100)
myCls = apply(myRandomData, 1, function(x) return(as.integer(c(7,-3)%*%x > 0)))
table(myCls)
plot(x = myRandomData[,1], y = myRandomData[,2], pch = myCls, col = myCls + 2)

abline(-neuron$weights[3]/neuron$weights[2], -neuron$weights[1]/neuron$weights[2], col = "blue")

#prosta metoda uczenia:
weightedSum = function(input, neuron) return(as.numeric(c(input,1) %*% neuron$weights))
# np. weightedSum(c(1,1), neuron)

compErr = function(input, target, neuron) return(abs(target - neuron$fun(weightedSum(input, neuron))))
# np. compErr(c(1,1), 1, neuron)

updateWeights = function(input, target, neuron, alpha = 0.1) {
    wSum = weightedSum(input, neuron)
    deltaW = alpha * (target - neuron$fun(wSum)) * neuron$derivative(wSum) * c(input,1)
    neuron$weights = neuron$weights + deltaW
    return(neuron)
}
#np. 
updateWeights(c(1,1), 1, neuron)

trainNeuron = function(neuron, dataTab, clsVec, 
                       alpha = 0.1, maxIterations = 100, 
                       threshold = 0.1, printInfo = TRUE) {
  
  endFlag = FALSE
  iteration = 1
  prevErrs = Inf
  while(!endFlag) {
      errVec = mapply(compErr, split(dataTab, 1:nrow(dataTab)), as.list(clsVec), MoreArgs = list(neuron = neuron))
      errSum = sum(errVec)
      if(prevErrs - errSum < threshold || iteration > maxIterations) {
        endFlag = TRUE
      } else {
          for(i in 1:nrow(dataTab)) neuron = updateWeights(dataTab[i,], clsVec[i], neuron, alpha)
          if(printInfo) cat("Iteracja: ", iteration, " suma bledow: ", errSum, "\n", sep="")
          prevErrs = errSum
          iteration = iteration + 1
      }
  }
  if(printInfo) {
    print(sum(errVec))
    print(neuron)
  }
  neuron
}

#sprobujmy nauczyc nasz neuron rozpoznawac zadana klase decyzyjna:
neuron = trainNeuron(neuron, myRandomData, myCls)

#mozemy teraz narysowac nasz model
plot(x = myRandomData[,1], y = myRandomData[,2], pch = myCls, col = myCls + 2)
abline(-neuron$weights[3]/neuron$weights[2], -neuron$weights[1]/neuron$weights[2], col = "blue")

#A co gdy decyzja jest bardziej zlozona?
myCls = apply(myRandomData, 1, function(x) return(as.integer(c(7,-3)%*%x > 0 &
                                                             c(-1,-2.5)%*%x + 1 > 0 &
                                                             c(-1,-2.5)%*%x - 2 < 0)))
plot(x = myRandomData[,1], y = myRandomData[,2], pch = myCls, col = myCls + 2)

neuron = createNeuron(2, range = 0.5)

#ponownie sprobujmy wytrenowac nasz neuron:
neuron = trainNeuron(neuron, myRandomData, myCls, printInfo = FALSE)
neuron

plot(x = myRandomData[,1], y = myRandomData[,2], pch = myCls, col = myCls + 2)
abline(-neuron$weights[3]/neuron$weights[2], -neuron$weights[1]/neuron$weights[2], col = "blue")
#wyglada to marnie...

#Zeby nauczyc siec neuronowa bardziej skomplikowanych "ksztaltow" klas decyzyjnych nalezy polaczyc
#pojedyncze neurony w siec. Jednym z najczesciej stosowanych algorytmow trenowania sieci neuronowych
#jest metoda wstecznej propagacji.

#Algorytm:
#1. Zainicjuj siec (losowo).
#2. Powtarzaj dla kolejnych obiektow az do osiagniecia kryterium stopu:
#  - oblicz blad klasyfikacji obiektu,
#  - oblicz uaktualnienia wag deltaW(i) pomiedzy neuronem wyjsciowym a i-tym neuronem ukrytym,
#  - dla wszystkich neuronow w warstwie ukrytej oblicz uaktualnienia wag deltaW(i,j) miedzy i-tym neuronem a j-tym wejsciem,
#  - uaktualnij wszystkie wagi modelu.

#Jaka strukture powinna miec siec dla tak zadanej klasy decyzyjnej?
#Implementacje sici neuronowych w R:
library(nnet)
?nnet

lol = nnet(myRandomData, myCls, size = 2, rang = 0.1,
          decay = 5e-4, maxit = 200)

plot(x = myRandomData[,1], y = myRandomData[,2], pch = myCls, col = myCls + 2)
abline(-lol$wts[3]/lol$wts[2], -lol$wts[1]/lol$wts[2], col = "blue")
preds = predict(lol, myRandomData)
preds[preds > 0.5] = 1
preds[preds <= 0.5] = 0
mean(preds == myCls)

#Zadanie 1:
#Prosze zaprojektowac i wyuczyc siec neuronowa z nnet dla powyzszych danych.

#W celu predykcji etykiet dla nowych obiektow w implementacji z nnet wykorzystuje sie metode predict.

#Konstruowanie wlasnych sieci neuronowych o skomplikowanej strukturze umozliwia biblioteka AMORE.
library(AMORE)
?newff
?train
myNNet = newff(n.neurons = c(2,3,1), learning.rate.global = 0.01, momentum.global = 0.5,
               error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")
myNNetModel = train(myNNet, as.data.frame(myRandomData), as.numeric(myCls), report=T, n.shows = 100, show.step = 100)

preds = predict(myNNetModel, myRandomData)

round(sim.MLPnet(myNNetModel$net,myRandomData), 3)
sum(abs(sim.MLPnet(myNNetModel$net,myRandomData) - myCls))

plot(x = myRandomData[,1], y = myRandomData[,2], pch = myCls, col = myCls + 2)
for(idx in myNNetModel$net$layers[[2]]) {
  neuron = myNNetModel$net$neurons[[idx]]
  abline(-neuron$bias/neuron$weights[2], - neuron$weights[1]/neuron$weights[2], col = "blue")
}

#Dla niektorych problemow konieczne jest skonstruowanie sieci zawierajacych wiecej niz jedna
#warstwe ukryta.... np. uogolniony XOR ograniczony do dwoch wymiarow:

myRandomData = matrix(rnorm(200), 100)
myCls = apply(myRandomData, 1, function(x) return(as.integer(all(x>0) | all(x<0))))
plot(x = myRandomData[,1], y = myRandomData[,2], pch = myCls, col = myCls + 2)

#Jaka strukture bedzie miala najmniejsza siec, ktora jest w stanie rozwiazac ten problem?

myNNet = newff(n.neurons = c(2,2,2,1), learning.rate.global = 0.1, momentum.global = 0.9,
               error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")
tmp = clsVec
tmp[tmp == -1] = 0
myCls
fix(dataSet)
first = apply(dataSet, 2, function(x) (x - min(x)) / (max(x) - min(x)))

myNNetModel = train(myNNet, as.data.frame(myRandomData), as.numeric(myCls), 
                    report=TRUE, n.shows = 50, show.step = 1000)

row.names(dataSet) = 1:dim(dataSet)[1]
myNNet = newff(n.neurons = c(30,3,1), learning.rate.global = 0.01, momentum.global = 0.5,
               error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")

myNNetModel = train(myNNet, as.data.frame(dataSet), as.numeric(clsVec), 
                    report=TRUE, n.shows = 50, show.step = 1000)

print(sum(abs(sim.MLPnet(myNNetModel$net,myRandomData) - myCls)))
mean(round(sim.MLPnet(myNNetModel$net,myRandomData)) == myCls)

for(idx in myNNetModel$net$layers[[2]]) {
  neuron = myNNetModel$net$neurons[[idx]]
  abline(-neuron$bias/neuron$weights[2], - neuron$weights[1]/neuron$weights[2], col = "blue")
}

transformX = function(x, neuron) return(neuron$f0(x%*%neuron$weights + neuron$bias))

transformedData = apply(myRandomData, 1, function(x) return(c(transformX(x, myNNetModel$net$neurons[[1]]),
                                                              transformX(x, myNNetModel$net$neurons[[2]]))))
transformedData = t(transformedData)

plot(x = transformedData[,1], y = transformedData[,2], pch = myCls, col = myCls + 2)
for(idx in myNNetModel$net$layers[[3]]) {
  neuron = myNNetModel$net$neurons[[idx]]
  abline(-neuron$bias/neuron$weights[2], -neuron$weights[1]/neuron$weights[2], col = "blue")
}

transformedData = apply(transformedData, 1, function(x) return(c(transformX(x, myNNetModel$net$neurons[[3]]),
                                                                 transformX(x, myNNetModel$net$neurons[[4]]))))
transformedData = t(transformedData)

plot(x = transformedData[,1], y = transformedData[,2], pch = myCls, col = myCls + 2)
neuron = myNNetModel$net$neurons[[5]]
abline(-neuron$bias/neuron$weights[2], -neuron$weights[1]/neuron$weights[2], col = "blue")

#Zadanie domowe:
#Prosze wytrenowac siec neuronowa z bibliteki nnet oraz AMORE na danych "wdbc". Prosze wybrac dobre parametry oraz
#przetestowac skutecznosc sieci metoda weryfikacji krzyzowej (prosze wykonac kilka powtorzen testu).
#WAZNE: przy uczeniu sieci nalezy pamietac o odpowiednim skalowaniu danych oraz o zamienieniu decyzji 
#na wartosci numeryczne z zbioru {-1, 1}!

