# 3 stany (numerujemy od 1)
# dla każdego po 2 akcje (od 1)
# dla każdej 3 możliwości przejścia
# - z każdą związane prawdopodobieństwo i nagroda

STATES <- array(0, dim=c(3,2,3,2)) # 3 stany x 2 akcje x 3 stany x {p-stwo, nagroda}

# wypełniamy dane
STATES[1,1,,] <- c(0.5, 0, 0.5, 0, 0, 0)
STATES[1,2,,] <- c(0, 0, 1, 0, 0, 0)
STATES[2,1,,] <- c(0.7, 0.1, 0.2, 5, 0, 0)
STATES[2,2,,] <- c(0, 0.95, 0.05, 0, 0, 0)
STATES[3,1,,] <- c(0.4, 0, 0.6, 0, 0, 0)
STATES[3,2,,] <- c(0.3, 0.3, 0.4, -1, 0, 0)

algorithm16 <- function(states, strategy, gam, the) {
	number_of_states <- dim(states)[1]
	V <- rep(0, number_of_states)
	it <- 0
	repeat {
		del <- 0
		it <- it + 1
		for (s in 1:number_of_states) {
			v <- V[s]
			a <- strategy[s]
			V[s] <- sum(states[s,a,,1] * (states[s,a,,2] + gam * V))
			#cat ("V[", s, "] = ", V[s], "\n", sep="")
			del <- max(del, abs(v-V[s]))
		}
		#cat ("del = ", del, "\n", sep="")
    #readline(prompt="Press Enter to continue...")
		if (del < the) break
	}
	cat ("iteracji: ", it, "\n", sep="")
	V
}

algorithm18 <- function(states, strategy, gam, the) {
	number_of_states <- dim(states)[1]
	PI <- strategy
	V <- rep(0, number_of_states)
	finish <- FALSE
	repeat {
		policy.stable <- TRUE
		it <- 0
		
		# policy evaluation
		repeat {
			del <- 0
			it <- it + 1
			for (s in 1:number_of_states) {
				v <- V[s]
				a <- PI[s]
				V[s] <- sum(states[s,a,,1] * (states[s,a,,2] + gam * V))
				#cat ("V[", s, "] = ", V[s], "\n", sep="")
				del <- max(del, abs(v-V[s]))
			}
			#cat ("del = ", del, "\n", sep="")
			if (del < the) break
		}
	  cat ("\niteracji: ", it, "\n", sep="")
	
		if (finish) {
			for (s in 1:number_of_states)
				cat ("V*[", s, "] = ", V[s], "\n", sep="")
			break
		}
	
		# policy improvement
		for (s in 1:number_of_states) {
			b <- PI[s]
			actions_outcome <- vector()
			for (a in 1:dim(states)[2]) {
				actions_outcome[a] <- sum(states[s,a,,1]*(states[s,a,,2]+gam*V))
			}
			PI[s] <- which.max(actions_outcome)
			if (b != PI[s]) policy.stable <- FALSE
		}
		if (!policy.stable) {
			cat ("--- NOT STABLE --- : updated strategy:\n", sep="")
			for (s in 1:number_of_states) {
				cat ("PI(", s, ") = ", PI[s], "\n", sep="")
			}
			cat ("\n")
		}
		else {
			cat ("*** STABLE *** : strategy:\n", sep="")
			for (s in 1:number_of_states) {
				cat ("PI(", s, ") = ", PI[s], "\n", sep="")
			}
			cat ("\n")
			finish <- TRUE
		}
		readline(prompt="Press Enter to continue...")
	}
	V
}

algorithm30 <- function(states, strategy, gam, the, alpha) {
	
	res16 <- algorithm16(states, strategy, gam, the)
	
	number_of_states <- dim(states)[1]
	PI <- strategy
	V <- rep(0, number_of_states)
	it <- 0
	repeat {
		s <- sample(number_of_states, 1)
		it <- it + 1
		repeat {
			a <- PI[s]
			s_prime <- sample(number_of_states, 1, prob = states[s,a,,1])
			r <- states[s,a,s_prime,2]
			V[s] <- V[s] + alpha * (r + gam * V[s_prime] - V[s])
			s <- s_prime
			#cat ("s = ", s, "\n", sep="")
			#for (s in 1:number_of_states)
			#	cat ("V[", s, "] = ", V[s], "\n", sep="")
   		#readline(prompt="Press Enter to continue...")
   		break
		}
		if (all(V <= res16 + 0.01 & V >= res16 - 0.01))
			break
	}
	cat ("iteracji - algorithm30: ", it, sep="")

}
