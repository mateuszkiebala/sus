# Mateusz Kiebala 359758
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
  stop("USAGE: Rscript run_mk359758.R training_set test_set output", call.=FALSE)
}

set.train = args[1] 
set.test = args[2]
output = args[3]

require(xgboost)
library(xgboost)

# Read data as characters to avoid problems connected with factors
train = read.table(set.train, header=T, sep=",", colClasses=rep('character', 16))
test = read.table(set.test, header=T, sep=",", colClasses=rep('character', 15))
# Convert letters to numbers
train$letter = apply(train, 1, function(r) {which(LETTERS == r[16])})

# Predict value for '?' in every column
predictMissingValues <- function(data, flag) {
  for (i in c(6:15)) {
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
    # watchlist = list(train = dtrain)
    # bst = xgb.cv(data=dtrain, max_depth=6, eta=0.3, nthread=20, nfold=5,
    #                nrounds=30, watchlist=watchlist, num_class=max(train.attr.label)+1, objective="multi:softmax")
    data[data[i] == '?',i] = predict(bst, test.attr)
  }
  return (data)
}

train = predictMissingValues(train, T)
test = predictMissingValues(test, F)
dtrain = xgb.DMatrix(data = apply(train[,-16], 2, as.numeric), label = train[,16])
watchlist = list(train = dtrain)
# XGBoost built-in crossvalidation to check accuracy on training set and select the best parameters.
#bst = xgb.cv(data=dtrain, max_depth=6, eta=0.3, nthread=20, nfold=5,
#             nrounds=50, watchlist=watchlist, num_class=27, objective="multi:softmax")
bst = xgb.train(data=dtrain, max_depth=6, eta=0.3, nthread=20,
                nrounds=50, watchlist=watchlist, num_class=27, objective="multi:softmax")
write(LETTERS[predict(bst, apply(test, 2, as.numeric))], file=output)