# Mateusz Kiebala 359758
#!/usr/bin/env Rscript
#args = commandArgs(trailingOnly=TRUE)
#if (length(args) != 3) {
#  stop("USAGE: Rscript run_mk359758.R training_set test_set output", call.=FALSE)
#}
#
#set.train = args[1] 
#set.test = args[2]
#output = args[3]

require(xgboost)
library(xgboost)

# Read data as characters to avoid problems connected with factors
train = read.table("letter-recognition-train.csv" , header=T, sep=",", colClasses=rep('character', 16))
test = read.table("letter-recognition-test-without-decisions.csv", header=T, sep=",", colClasses=rep('character', 15))
# Convert letters to numbers
train$letter = apply(train, 1, function(r) {which(LETTERS == r[16])})

# Predict value for '?' in every column
predictMissingValues <- function(data, flag) {
  for (i in c(6:7,10:11,13:14)) {
    print (paste("Predicting missing values for column ", i, sep=""))
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
    bst = xgb.train(data=dtrain, max_depth=6, eta=0.3, nthread=20,
                    nrounds=30, num_class=max(train.attr.label)+1, objective="multi:softmax")
    # XGBoost built-in crossvalidation to check accuracy on training set and select the best parameters.
    #watchlist = list(train = dtrain)
    #xgb.cv(data=dtrain, max_depth=6, eta=0.3, nthread=20, nfold=5,
    #        nrounds=30, watchlist=watchlist, num_class=max(train.attr.label)+1, objective="multi:softmax")
    data[data[i] == '?',i] = predict(bst, test.attr)
  }
  return (data)
}

subset = sample(1:nrow(train), round(nrow(train)*0.7))
data.train = train[subset,]
data.test = train[-subset,]
data.train = predictMissingValues(data.train, T)
data.test = predictMissingValues(data.test, F)
for (i in c(8,9,12,15)) {
  data.train[data.train[i] == '?', i] = 0
  data.test[data.test[i] == '?', i] = 0
}

dtrain = xgb.DMatrix(data = apply(data.train[,-16], 2, as.numeric), label = data.train[,16])
watchlist = list(train = dtrain)
# XGBoost built-in crossvalidation to check accuracy on training set and select the best parameters.
#bst = xgb.cv(data=dtrain, max_depth=50, eta=0.1, nthread=20, nfold=5,
#             nrounds=50, watchlist=watchlist, num_class=27, objective="multi:softmax")
bst = xgb.train(data=dtrain, max_depth=6, eta=0.3, nthread=20,
                nrounds=50, watchlist=watchlist, num_class=27, objective="multi:softmax")
print(sum(diag(table(predict(bst, apply(data.test[,-16], 2, as.numeric)), data.test[,16]))) / nrow(data.test))
#write(LETTERS[predict(bst, apply(test, 2, as.numeric))], file=output)