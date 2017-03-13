estimate_error <- function (sample_size, salience) {
  sqrt(log(1-salience) / (-2*sample_size))
}

empiric_error <- 22 / 232
print (epsilon <- estimate_error(232, 0.95))
print (empiric_error + epsilon)

# ------------------------------------------

min_sample_size <- function (eps, salience) {
  ceiling(log(1-salience) / (-2*(eps^2)))
}

saliences = c(0.9, 0.99, 0.999, 0.9999)
print (sapply(saliences, min_sample_size, eps=0.03))

eps = c(0.1, 0.07, 0.05, 0.04, 0.03, 0.02, 0.01)
print (sapply(eps, min_sample_size, salience=0.95))
