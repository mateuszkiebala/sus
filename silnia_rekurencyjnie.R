recursive_factorial <- function(n) {
  if (n == 0)
    return (1)
  else
    return (n * recursive_factorial(n-1))
}

print(recursive_factorial(5))
