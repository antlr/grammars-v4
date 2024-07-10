# Taken from the JPlag repository (https://github.com/jplag/JPlag/blob/570efab64f89ad368bf262d1c1e57710557c0fed/languages/rlang/src/test/resources/de/jplag/rlang/Game.R)

# Sample R program
# Author: Robin Maisch

readBool <- function()
{ 
  n <- readline(prompt="Yes or no? (y/n): ")
  while (!grepl("[YyNn]",n))
  {
	 n <- readline()
  }
  return(ifelse(n=="y"||n=="Y", TRUE, FALSE))
}


getFilterBalance <- function(f, elements) {
	return(abs(length(Filter(f, elements)) - length(elements)/2))
}

divisibleByFilter <- function(i) {
	res <- c(function(x) x %% i == 0, paste("a multiple of", i), NA);
	return(res)
}

greaterThanFilter <- function(i) {
	res <- c(function(x) x > i, paste("greater than", i), NA);
	return(res)
}

smallerThanFilter <- function(i) {
	res <- c(function(x) x < i, paste("smaller than", i), NA);
	return(res)
}

endsWithFilter <- function(i) {
	res <- c(function(x) x %% 10 == i, paste("'s last digit a'", i), NA);
	return(res)
}

# real program start here
  
main <- function() {
	filters <- c(lapply(2:20, divisibleByFilter),
				lapply(0:100, greaterThanFilter),
				lapply(0:100, smallerThanFilter),
				lapply(0:10, endsWithFilter))
	filters <- aperm(simplify2array(filters, higher=FALSE), c(2,1))
	activeFilters = list()

	cat("Think of a number between 0 and 100.\n")
	count <- 1
	candidates <- c()
	repeat {
		cat("\n +++ Round",count,"+++\n\n")
		candidates = 0:100
		for (f in activeFilters) {
			filterFunc = ifelse(f[[3]], function(x) f[[1]](x), function(x) !f[[1]](x))
			candidates = Filter(filterFunc, candidates)
		}
		if (length(candidates) == 1) {
			break
		}

		balance <- sapply(1:(length(filters)/3), function(i) {return(getFilterBalance(filters[i,][[1]], candidates))})
		rank <- rank(balance, ties="first")
		risk = max(20-2*count, 1)
		lowerEnd = 1
		topFiveIdx = order(rank)[lowerEnd:risk]
		# print(filters[topFiveIdx[1:3],2])
		winnerIdx = topFiveIdx[sample.int(risk-lowerEnd + 1,1)]
		winnerFilter = filters[winnerIdx,]

		cat("Is your number", winnerFilter[[2]], "?\n")
		winnerFilter[[3]] <- readBool()

		cat("Okay, now, let's see...\n")
		activeFilters[[length(activeFilters)+1]] = winnerFilter
		# cat("Now we have", length(activeFilters), "active filters.\n")

		filters[- winnerIdx,] # remove element
		count <- count + 1
	}

	cat("Your number must be", candidates[1], ", right???\n")
	answer <- readBool()
	if (answer) {
		cat("Hahaha, I knew it. I'm a genius.\n")
	} else {
		cat("You must be joking!\n")
	}

}
main()
