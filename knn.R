library(class)

data = seq(1, 10000)
f <- function (n) {
  if ((((n %/% 1000) %% 2) == 0 && n %% 4 == 0) ||
      (((n %/% 1000) %% 2) != 0 && n %% 4 != 0)) {
    1
  } else {
    0
  }
}

myKNN <- function(x, data.train, k) {
  stopifnot(k <= length(data.train))
  left = 1
  right = length(data.train)
  mid = 0
  res = right
  while (left <= right) {
    mid = (left + right) %/% 2
    if (data.train[mid] > x) {
      right = mid - 1
      res = mid
    } else {
      left = mid + 1
    }
  }
  
  neighbours = vector(length = k)
  left = res - 1
  right = res
  i = 1
  while (i <= k) {
    takeLeft = FALSE
    takeRight = FALSE
    if (right > length(data.train)) {
      if (data.train[left] != x) {
        takeLeft = TRUE
      }
    } else if (left < 1) {
      if (data.train[right] != x) {
        takeRight = TRUE
      }
    } else {
      if (data.train[left] != x && data.train[right] != x) {
        if (abs(data.train[left] - x) <= abs(data.train[right] - x)) {
          takeLeft = TRUE
        } else {
          takeRight = TRUE
        }
      }
    }
    
    if (takeLeft == TRUE) {
      neighbours[i] = data.train[left]
      left = left - 1
      i = i + 1
    } else if (takeRight == TRUE) {
      neighbours[i] = data.train[right]
      right = right + 1
      i = i + 1
    } else {
      if (data.train[left] == x) {
        left = left - 1
      } else {
        right = right + 1
      }
    }
  }
  as.numeric(names(sort(table(sapply(neighbours, f)), decreasing = T)[1]))
}


zad1 <- function () {
  K = seq(1, 29, 2)
  res = vector()
  U = sort(data[sample(x=data, size=1000, replace = F)], decreasing = F)
  target = sapply(U, f)
  for (i in K) {
    prediction = sapply(U, myKNN, data.train=U, k=i)
    err = 1 - (sum(diag(table(prediction, target)))) / length(target)
    res = c(res, c(err))
    print (paste("k:", i, err))
  }
  return (res)
}

print (summary(zad1()))

# --------------------------------------------------------------------------------------------------

zad2 <- function () {
  subset = sample(x = data, size = 1000, replace = FALSE)
  U = sort(data[subset], decreasing = F)
  data.test =  data[sample(x = data[-subset], size = 1000, replace = FALSE)]
  target.U = sapply(U, f)
  target.test = sapply(data.test, f)
  
  prediction_1 = sapply(U, myKNN, data.train = U, k = 1)
  generalizationError = 1 - sum(diag(table(prediction_1, target.U))) / length(target.U)
  
  prediction_2 = sapply(data.test, myKNN, data.train = U, k = 1)
  empiricError = 1 - sum(diag(table(prediction_2, target.test))) / length(target.test)

  abs(generalizationError - empiricError)
}

res2 = vector(length = 100)
for (i in seq(1,1000)) {
  print (i)
  res2[i] = zad2()
}
testEpsZad2 <- max(sort(res2, decreasing = F)[1:95])
hoeffdingEps <- sqrt(log(0.05)/(-2000))
print (testEpsZad2)
print (hoeffdingEps)

# --------------------------------------------------------------------------------------------------

zad3 <- function (n) {
  U = sort(data[sample(x = data, size = 1000, replace = FALSE)], decreasing = FALSE)
  prediction = sapply(U, myKNN, data.train = U, k = 1)
  target = sapply(U, f)
  generalizationError = 1 - sum(diag(table(prediction, target))) / length(target)
  
  # Crossvalidation
  sample.size = length(U) / n
  res = vector(length = n)
  U = U[sample(length(U))]
  for (i in 1:n) {
    sample = (((i-1)*sample.size + 1) : (i*sample.size))
    U.train = sort(U[-sample], decreasing = FALSE)
    U.test = U[sample]
    prediction.test = sapply(U.test, myKNN, data.train = U.train, k = 1)
    target.test = sapply(U.test, f)
    res[i] = 1 - sum(diag(table(prediction.test, target.test))) / length(target.test)
  }
  empiricError = mean(res)
  abs(generalizationError - empiricError)
}

res3 = vector(length = 100)
for (i in seq(1,100)) {
  res3[i] = zad3(10)
}
testEpsZad3 <- max(sort(res3, decreasing = F)[1:95])
print (testEpsZad3)