x = 0:26
y = c(1,-1,1,-1,-1,-1,1,-1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,1,-1,-1,-1,1,-1,1)
n = length(x)
w = rep(1/n, n)

h1 <- function(x, a) {
  if (x <= a) {
    return (1)
  } else {
    return (-1)
  }
}

h2 <- function(x, a) {
  if (x <= a) {
    return (-1)
  } else {
    return (1)
  }
}
learners = list(h1, h2)

zad2_classificator <- function(learners, w) {
  best.a = 100
  best.h.ind = 3
  best.error = 1
  for (a in 0:26) {
    for (i in 1:length(learners)) {
      h = learners[[i]]
      e = 0
      for (j in 1:length(x)) {
        # 1 * 1 = 1 and -1 * (-1) = 1
        if (h(x[j], a) * y[j] == -1) {
          e = e + w[j]
        }
      }
      if (e < best.error) {
        best.a = a
        best.h.ind = i
        best.error = e
      }
    }
  }
  print (paste("a:", best.a, sep = " "))
  print (paste("e:", best.error, sep = " "))
  return (list(best.error, best.a, best.h.ind))
}

T = 35
H = list()
A = numeric(length = T)
ALPHAS = numeric(length = T)
for (t in 1:T) {
    res = zad2_classificator(learners, w)
    e = res[[1]]
    a = res[[2]]
    h.ind = res[[3]]

    alpha = 1/2 * log((1 - e) / e)
    # Update weights
    for (i in 1:n) {
      w[i] = w[i] * exp(-y[i] * alpha * learners[[h.ind]](x[i], a))
    }
    
    # Normalize weights that they sum up to 1
    sum = sum(w)
    for (i in 1:n) {
      w[i] = w[i] / sum
    }
    
    H[[t]] = learners[[h.ind]]
    ALPHAS[t] = alpha
    A[t] = a
}

classify <- function (x) {
  res = 0
  for (i in 1:T) {
    h = H[[i]]
    res = res + h(x, A[i]) * ALPHAS[i]
  }
  sign(res)
}

print (w)
res = sapply(x, classify)
accuracy = sum(diag(table(res, y))) / (length(x))
print (paste("accuracy:", accuracy, sep = " "))