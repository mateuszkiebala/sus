library(tree)

# Zadanie 1
iris[iris$Petal.Width > 1.5, ]$Species


# Zadanie 2
countClassificationError <- function (n) {
  iris <- iris[sample(nrow(iris)),]
  sample_size <- dim(iris)[1] / n
  sum_error <- 0
  
  for (i in (1:n)){
    sam <- (((i-1)*sample_size + 1) : (i*sample_size))
    train <- iris[-sam,]
    test  <- iris[sam,]
    iris.tr <- tree(Species ~., train)
    fit <- predict(iris.tr, test, type="class")
    sum_error <- sum_error + (length(sam) - sum(diag(table(test$Species, fit)))) / (length(sam))
  }
  print (sum_error / n)
}

n = 10
countClassificationError(n)
