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

siec_kohonena_otoczenie <- function(N=1000, IT=1000, k=5) {
	
	Wx <- runif(N, 0, 1)
	Wy <- runif(N, 0, 1)
	
	t <- 1
	while (t <= IT) {
		ex_x <- runif(1, 0, 1)
		ex_y <- runif(1, 0, 1)
		
		#ex_x <- rnorm(1, .5, .1)
		#ex_y <- rnorm(1, .5, .1)
	
		d <- sqrt((ex_x-Wx)^2 + (ex_y-Wy)^2)
		argmin_d <- which.min(d)
		
		h_0 <- .9 * (1 - t/IT)
		sigma_c <- N/4 * (1 - t/IT)^3
		new_Wx <- Wx
		new_Wy <- Wy
		new_Wx[argmin_d] <- Wx[argmin_d] + h_0 * (ex_x-Wx[argmin_d])
		new_Wy[argmin_d] <- Wy[argmin_d] + h_0 * (ex_y-Wy[argmin_d])
		
		# wstecz
		it <- argmin_d - 1
		neighbor_dist <- 0
		for (i in 1:k) {
			if (it > i) {
				neighbor_dist <- neighbor_dist + sqrt(i^2 + i^2)
				h_ri_rc <- h_0 * exp(-(neighbor_dist^2)/(sigma_c^2))
				new_Wx[it-i] <- Wx[it-i] + h_ri_rc * (ex_x-Wx[it-i])
				new_Wy[it-i] <- Wy[it-i] + h_ri_rc * (ex_y-Wy[it-i])
			}
		}
		
		# wprzód
		it <- argmin_d + 1
		neighbor_dist <- 0
		for (i in 1:k) {
			if (it <= N-i) {
				neighbor_dist <- neighbor_dist + sqrt(i^2 + i^2)
				h_ri_rc <- h_0 * exp(-(neighbor_dist^2)/(sigma_c^2))
				new_Wx[it+i] <- Wx[it+i] + h_ri_rc * (ex_x-Wx[it+i])
				new_Wy[it+i] <- Wy[it+i] + h_ri_rc * (ex_y-Wy[it+i])
			}
		}
		Wx <- new_Wx
		Wy <- new_Wy
		
		t <- t+1
		
		plot(Wx, Wy, type="l")
	}
}

siec_kohonena <- function(N=1000, IT=1000, k=5) {
	
	Wx <- runif(N, 0, 1)
	Wy <- runif(N, 0, 1)
	
	t <- 1
	while (t <= IT) {
		ex_x <- runif(1, 0, 1)
		ex_y <- runif(1, 0, 1)
		
		#ex_x <- rnorm(1, .5, .1)
		#ex_y <- rnorm(1, .5, .1)
	
		d <- sqrt((ex_x-Wx)^2 + (ex_y-Wy)^2)
		argmin_d <- which.min(d)
		
		h_0 <- .9 * (1 - t/IT)
		sigma_c <- N/4 * (1 - t/IT)
		new_Wx <- Wx
		new_Wy <- Wy
		new_Wx[argmin_d] <- Wx[argmin_d] + h_0 * (ex_x-Wx[argmin_d])
		new_Wy[argmin_d] <- Wy[argmin_d] + h_0 * (ex_y-Wy[argmin_d])
		
		# wstecz
		it <- argmin_d - 1
		for (i in 1:k) {
			if (it > i) {
				neighbor_dist <- i
				h_ri_rc <- h_0 * exp(-(neighbor_dist^2)/(sigma_c^2))
				new_Wx[it-i] <- Wx[it-i] + h_ri_rc * (ex_x-Wx[it-i])
				new_Wy[it-i] <- Wy[it-i] + h_ri_rc * (ex_y-Wy[it-i])
			}
		}
		
		# wprzód
		it <- argmin_d + 1
		for (i in 1:k) {
			if (it <= N-i) {
				neighbor_dist <- i
				h_ri_rc <- h_0 * exp(-(neighbor_dist^2)/(sigma_c^2))
				new_Wx[it+i] <- Wx[it+i] + h_ri_rc * (ex_x-Wx[it+i])
				new_Wy[it+i] <- Wy[it+i] + h_ri_rc * (ex_y-Wy[it+i])
			}
		}
		Wx <- new_Wx
		Wy <- new_Wy
		
		t <- t+1
		
		plot(Wx, Wy, type="l")
	}
}

s_k <- function(N=1000, IT=1000) {
  
  Wx <- runif(N, 0, 1)
  Wy <- runif(N, 0, 1)
  t <- 1
  while (t <= IT) {
    
    #ex <- runif(2, 0, 1)    
    ex <- rnorm(2, mean=c(0.5, 0.5), sd=c(0.1,0.1))
    
    d <- sqrt((ex[1]-Wx)^2 + (ex[2]-Wy)^2)
    argmin_d <- which.min(d)
    
    h_0 <- 0.9 * (1 - t/IT)
    sigma_c <- N/4 * (1 - t/IT)
    
    odl <- abs(c(1:N) - argmin_d)
    h_ri_rc <- h_0 * exp(-(odl^2)/(sigma_c^2))
    Wx <- Wx + h_ri_rc * (ex[1] - Wx)
    Wy <- Wy + h_ri_rc * (ex[2] - Wy)
    
    t <- t+1
    
    plot(Wx, Wy, type="l")
  }
}

kohonen_iris <- function () {
  library(som.nn)
  data(iris)
  species <- iris$Species
  som <- som.nn.train(iris, class.col = "Species", kernel = "internal",
                      xdim = 15, ydim = 9, alpha = 0.2, len = 10000,
                      norm = TRUE, toroidal = FALSE)
  
  unk <- iris[,!(names(iris) %in% "Species")]
  p <- predict(som, unk)
  print (table(species, p$pred.class)) #) / dim(iris)[1]
}
