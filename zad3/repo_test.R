library(xgboost)
data = read.table("letter-recognition.data.txt", header = F, sep = ",")
subset = sample(1:nrow(data), nrow(data)*0.7)
data.train = data[subset,]
data.test = data[-subset,]

dtrain = xgb.DMatrix(data = apply(data.train[,-1], 2, as.numeric), label = data.train[,1])
dtest = xgb.DMatrix(data = apply(data.test[,-1], 2, as.numeric), label = data.test[,1])
watchlist = list(train = dtrain, test = dtest)
bst = xgb.train(data=dtrain, max_depth=6, eta=0.3, nthread=20, nfold=5,
             nrounds=50, watchlist=watchlist, num_class=27, objective="multi:softmax")

library(kknn)
knnClassModel = train.kknn(as.factor(V1)~., data.train, kmax = 27)
sum(diag(table(predict(knnClassModel, data.test[,-1]), data.test[,1]))) / nrow(data.test)
