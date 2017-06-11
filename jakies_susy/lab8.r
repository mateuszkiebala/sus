wczytaj_przyklad <- function()
{ 
  a <- readline(prompt="Enter an argument of example: ")
  v <- readline(prompt="Enter value of example: ")
  return(c(as.integer(a), as.integer(v)))
}

sigma <- function(x, beta=1) {
  return(1/(1+exp((-beta)*x)))
}

my_random <- function(x, min, max) {
	i <- runif(x, -1, 1)
	sig <- ifelse(i < 0, -1, 1)
	return(sig*runif(x, min, max))
}

siec_neuronowa <- function(eta_pocz=1, beta=1) {

	N <- 8 # liczba neuronów ukrytych
  
  eta <- eta_pocz
  krok <- 0.00001 * eta_pocz
  # oczekiwany wynik (i jednocześnie przykłady treningowe)
  res <- data.frame(x=c(0,1,2,3), t=c(1,0,0,1))
  
  w <- my_random(N, 0., 1)
  u <- my_random(N, 0., 1)
  v <- my_random(N, 0., 1)
  v0 <- my_random(1, 0., 1)
  print(res)
  
  count <- 0
  err_max <- 1
  while (err_max > .001) {
  	count <- count + 1
  	for (i in 1:dim(res)[1]) {
		  # wczytujemy przyklad
		  x <- res[i,1]
		  t <- res[i,2]
		  # obliczamy aktywacje
		  h <- sigma(w*x + u, beta)
		  o <- sigma(sum(v*h) + v0, beta)
		  # poprawiamy wagi
		  delta0 <- beta*(t-o)*o*(1-o)
		  delta_h <- beta*delta0*v*h*(1-h)
		  v0 <- v0 + eta*delta0
		  v <- v + eta*delta0*h
		  w <- w + eta*delta_h*x
		  u <- u + eta*delta_h
		  # obliczamy nowe wartosci
		  o <- c()
		  for (xi in res$x) {
		    hi <- sigma(w*xi + u, beta)
		    o <- c(o, sigma(sum(v*hi) + v0, beta))
		  }
		  res$o <- o
		}
		plot(res$x, res$o)
		if (count %% 500 == 0) {
  		cat("err: ", abs(res$o-res$t), "\n")
  		cat("count: ", count, "\n")
  		cat("eta: ", eta, "\n")
			print(res)
		}
		err_max <- max(abs(res$o-res$t))
		eta <- eta - krok
    #readline(prompt="Press Enter to continue...")
  }
  cat("final count: ", count, "\n")
}
