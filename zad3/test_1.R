library(xgboost)
train = read.table("letter-recognition-train.csv", header=T, sep=",", colClasses=rep('character', 16))
# test = read.table("letter-recognition-test-without-decisions.csv", header=T, sep=",", colClasses=rep('character', 15))
train$letter = apply(train, 1, function(r) {which(LETTERS == r[16])})

for (i in c(6:15)) {
  test.attr = train[train[i] == '?',c(1:5, 16)]
  train.attr = train[train[i] != '?',c(1:5, 16)]
  train.attr.label = as.numeric(train[train[i] != '?',i])
  
  train.attr = apply(train.attr, 2, as.numeric)
  test.attr = apply(test.attr, 2, as.numeric)
  dtrain = xgb.DMatrix(data = train.attr, label = train.attr.label)
  watchlist = list(train = dtrain)
  bst = xgb.train(data=dtrain, max_depth=5, eta=0.2, nthread=20,
                  nrounds=30, watchlist=watchlist, num_class=max(train.attr.label)+1, objective="multi:softmax")
  train[train[i] == '?',i] = predict(bst, test.attr)
}

subset = sample(1:nrow(train), size = nrow(train)*0.7)
data.test = train[-subset,]
data.train = train[subset,]
data.test = apply(data.test, 2, as.numeric)
dtrain = xgb.DMatrix(data = apply(data.train[,-16], 2, as.numeric), label = data.train[,16])
watchlist = list(train = dtrain)
bst = xgb.train(data=dtrain, max_depth=5, eta=0.1, nthread=20,
             nrounds=50, watchlist=watchlist, num_class=27, objective="multi:softmax")
classes = predict(bst, data.test[,-16])
sum(diag(table(classes, data.test[,16]))) / nrow(data.test)
