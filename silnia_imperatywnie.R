iterative_factorial <- function(n) {
  silnia <- 1
  for (i in 1:n) silnia <- silnia * i
  silnia
}

print(iterative_factorial(5))
