library(AMORE)
library(xgboost)
# Read data as characters to avoid problems connected with factors
train = read.table("letter-recognition-train.csv", header=T, sep=",", colClasses=rep('character', 16))
test = read.table("letter-recognition-test-without-decisions.csv", header=T, sep=",", colClasses=rep('character', 15))
# Convert letters to numbers
train$letter = apply(train, 1, function(r) {which(LETTERS == r[16])})

predictMissingValues <- function(data, flag) {
  for (i in c(6:15)) {
    # We use 1-5 columns (there are no '?') and letter column (if possible) to predict missing values.
    if (flag) {
      test.attr = data[data[i] == '?',c(1:5, 16)]
      train.attr = data[data[i] != '?',c(1:5, 16)]
    } else {
      test.attr = data[data[i] == '?',c(1:5)]
      train.attr = data[data[i] != '?',c(1:5)]
    }
    train.attr.label = as.numeric(data[data[i] != '?',i])
    train.attr = apply(train.attr, 2, as.numeric)
    test.attr = apply(test.attr, 2, as.numeric)
    dtrain = xgb.DMatrix(data = train.attr, label = train.attr.label)
    watchlist = list(train = dtrain)
    bst = xgb.train(data=dtrain, max_depth=6, eta=0.3, nthread=20,
                    nrounds=30, watchlist=watchlist, num_class=max(train.attr.label)+1, objective="multi:softmax")
    data[data[i] == '?',i] = predict(bst, test.attr)
  }
  return (data)
}

train = predictMissingValues(train, T)
test = predictMissingValues(test, F)
for (i in 1:26) {
  print(i)
  colname.letter = paste("letter_", i, sep = "")
  train[,colname.letter] = numeric(nrow(train))
  train[train$letter %in% i, colname.letter] = 1
}
train = apply(train, 2, as.numeric)
test = apply(test, 2, as.numeric)
myNNet = newff(n.neurons = c(15,40,26), learning.rate.global = 0.01, momentum.global = 0.01,
               error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")

subset = sample(1:nrow(train), nrow(train)*0.7)
data.train = train[subset,]
data.test = train[-subset,]

# Train
myNNetModel = train(myNNet, data.train[,1:15], data.train[,17:42], report=T, n.shows = 100, show.step = 100)
sum(diag(table(max.col(sim.MLPnet(myNNetModel$net, data.test[,1:15])), max.col(data.test[,17:42])))) / nrow(data.test)
write(LETTERS[max.col(sim.MLPnet(myNNetModel$net, data.test[,1:15]))], file="NN_res")