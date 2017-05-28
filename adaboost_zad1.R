#Zad1
x = c(0, 1, 1)
y = c(-1, 1, 1)
n = length(x)
w = rep(1/n, n)

h1 <- function(x) {1}
h2 <- function(x) {-1}
learners = list(h1, h2)

T = 2
H = list()
ALPHAS = numeric(T)
for (t in 1:T) {
  h = learners[[t]]
  e = 0
  for (i in 1:n) {
    if (h(x[i]) != y[i]) {
      e = e + w[i]
    }
  }
  
  alpha = 1/2 * log((1 - e) / e)
  # Update weights
  for (i in 1:n) {
    w[i] = w[i] * exp(-y[i] * alpha * h(x[i]))
  }
  
  # Normalize weights that they sum up to 1
  sum = sum(w)
  for (i in 1:n) {
    w[i] = w[i] / sum
  }
  
  print (e)
  print (w)
  print (alpha)
  H[[t]] = h
  ALPHAS[t] = alpha
}

classify <- function (x) {
  res = 0
  for (i in 1:T) {
    h = H[[i]]
    res = res + h(x) * ALPHAS[i]
  }
  sign(res)
}