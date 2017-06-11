klastruj_2_centr <- function(vec) {
  s1 <- vec[1]
  s2 <- vec[2]
  
  kl1 <- s1
  kl2 <- s2
  
  for (i in c(3:length(vec))) {
    if (abs(vec[i]-s1) <= abs(vec[i]-s2)) {
      kl1 <- c(kl1, vec[i])
    }
    else {
      kl2 <- c(kl2, vec[i])
    }
  }
  
  s1 <- sum(kl1)/length(kl1)
  s2 <- sum(kl2)/length(kl2)
  
  while (TRUE){
    kl1 <- vector()
    kl2 <- vector()
    for (i in vec){
      if (abs(i-s1) <= abs(i-s2)) {
        kl1 <- c(kl1, i)
      }
      else {
        kl2 <- c(kl2, i)
      }
    }
    
    s1_new <- sum(kl1)/length(kl1)
    s2_new <- sum(kl2)/length(kl2)
    
    if (s1_new == s1 && s2_new == s2) {
      cat("\nkl1 = ")
      print(kl1)
      cat("kl2 = ")
      print(kl2)
      cat("\n")
      break
    }
    else {
      s1 <- s1_new
      s2 <- s2_new
    }
  }
  
}

aux <- function(mi, sig, tau, x) {
	return (1/sig * exp(-((x-mi)^2/(2*(sig)^2))) * tau)
}

P <- function(mi, sig, tau, x) {
	return (1/(sig * sqrt(2 * pi)) * exp(-((x-mi)^2/(2*(sig)^2))) * tau)
}

EM <- function(vec, stop = 1/100) {

	miA <- 0
	sigA <- 1
	tauA <- .5 
	miB <- 0
	sigB <- 10
	tauB <- .5
	
	N <- length(vec)
	
	PA_prev <- 2
	PB_prev <- 2
	PA_act <- prod(P(miA, sigA, tauA, vec))
	PB_act <- prod(P(miB, sigB, tauB, vec))
	#abs(log(prod((P_act)) - log(prod(P_prev))) > stop
	while (T) {
		PA_prev <- PA_act
		PB_prev <- PB_act
		TAi <- aux(miA, sigA, tauA, vec)/(aux(miA, sigA, tauA, vec) + aux(miB, sigB, tauB, vec))
		TBi <- aux(miB, sigB, tauB, vec)/(aux(miA, sigA, tauA, vec) + aux(miB, sigB, tauB, vec))
		
		sumTAi = sum(TAi)
		sumTBi = sum(TBi)
		
		tauA <- 1/N * sumTAi
		tauB <- 1/N * sumTBi
		print(tauA)
		print(tauB)
		print(tauA + tauB)
		miA <- sum(TAi * vec)/sumTAi
		miB <- sum(TBi * vec)/sumTBi
		sigA <- sum(TAi * (vec - miA)^2)/sumTAi
		sigB <- sum(TBi * (vec - miB)^2)/sumTBi
		
		kl1 <- vector()
		kl2 <- vector()
		for (x in vec) {
			if (P(miA, sigA, tauA, x) > P(miB, sigB, tauB, x)) {
				kl1 = c(kl1, x)
			}
			else {
				kl2 = c(kl2, x)
			}
		}
      cat("\nkl1 = ")
      print(kl1)
      cat("kl2 = ")
      print(kl2)
      cat("\n")
		
		PA_act <- prod(P(miA, sigA, tauA, vec))
		PB_act <- prod(P(miB, sigB, tauB, vec))
		#xA <- seq(miA-10, miA+10, length=1000)
		#yA <- dnorm(xA, mean=miA, sd=sigA)
		#plot(xA, yA, type="l", lwd=1)
		xB <- seq(max(c(miB-10, 0)), min(c(10, miB+10)), length=1000)
		yB <- dnorm(xB, mean=miB, sd=sigB)
		plot(xB, yB, type="l", lwd=1)
		print(sigA)
		print(sigB)
		print(log(PA_prev))
		print(log(PA_act))
		#print(abs(log(PA_act) - log(PA_prev)))
    readline(prompt="Press Enter to continue...")
	}
	
}
