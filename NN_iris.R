library(AMORE)
library(ROCR)
# zad1
data = data.frame(x = c(0,1,2,3))
target = c(1,0,0,1)
acc = vector(length = 1000)

for (i in 1:1000) {
  print (i)
  # Create network
  myNNet = newff(n.neurons = c(1,8,1), learning.rate.global = 0.4, momentum.global = 0.3,
                 error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")
  # Train
  myNNetModel = train(myNNet, data, target, report=F, n.shows = 10, show.step = 100)
  # Test
  scores = round(sim.MLPnet(myNNetModel$net, data))
  # Accuracy in supremum norm
  acc[i] = as.numeric(max(scores) == max(target))
}
print(paste("zad1:", "accuracy", mean(acc)))

# zad2
subset = sample(dim(iris)[1], size = dim(iris)[1] * 0.8, replace = F)
data.iris = iris
iris.species = c('setosa', 'versicolor', 'virginica')
data.iris = cbind(data.iris, setosa = as.numeric(iris$Species == iris.species[1]))
data.iris = cbind(data.iris, versicolor = as.numeric(iris$Species == iris.species[2]))
data.iris = cbind(data.iris, virginica = as.numeric(iris$Species == iris.species[3]))
data.iris.train = data.iris[subset,]
data.iris.test = data.iris[-subset,]

myNNet = newff(n.neurons = c(4,20,3), learning.rate.global = 0.3, momentum.global = 0.2,
               error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")
# Train
myNNetModel = train(myNNet, data.iris.train[,1:4], data.iris.train[,6:8], report=T, n.shows = 100, show.step = 100)

# Test
scores = sim.MLPnet(myNNetModel$net, data.iris.test[,1:4])
res = table(max.col(scores), max.col(data.iris.test[,6:8]))
row.names(res) = iris.species
colnames(res) = iris.species
acc = sum(diag(res)) / dim(data.iris.test)[1]
print (paste("zad2:", "accuracy", acc))
