library(tree)

# Zadanie 1
filteredIris <- iris[iris$Petal.Width > 1.5, ]
filteredIris$Species

# Zadanie 2
countClassificationError <- function (n) {
  iris <- iris[sample(nrow(iris)),]
  sample_size <- dim(iris)[1] / n
  # generate n groups from iris table
  groups <- split(iris, rep(1:n, each = sample_size))
  message(sprintf("Iteration error: %f", classification_error <- (sum(sapply(groups, getGroupError)) / length(groups))))
  classification_error
}

getGroupError <- function (g) {
  m <- dim(g)[1]
  val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m))
  g.learn <- g[-val,]
  g.valid <- g[val,]
  g.tr <- tree(Species ~., g.learn)
  fit <- predict(g.tr, g.valid, type="class")
  error <- (dim(g.valid)[1] - sum(diag(table(g.valid$Species, fit)))) / dim(g.valid)[1]
}

n = 1
iterations = 10
print (sum(sapply(rep(n, each=iterations), countClassificationError)) / iterations)
