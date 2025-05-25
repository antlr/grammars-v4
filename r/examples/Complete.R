# Taken from the JPlag repository (https://github.com/jplag/JPlag/blob/570efab64f89ad368bf262d1c1e57710557c0fed/languages/rlang/src/test/resources/de/jplag/rlang/Complete.R)

# This R code sample is supposed to contain the corresponding AST structure for each type of RToken.
# It is also working code.
# Author: Robin Maisch

main <- function() {
	sixteen <- square(4);
	squareOkay <- ifelse(sixteen==16, TRUE, FALSE);
	cat("Should be 16: ", sixteen, squareOkay);
	?cat;

	if (squareOkay) {
		perfectSquare <- square
	} else {
		perfectSquare <- function (x) x*x
	}

	for (i in 1:10) {
		print(apply(arg=i, func=perfectSquare))
	}

	repeat {
		if (FALSE) next
		else break
	}

	idx <- 0
	while (FALSE) {
		idx <- idx + 1
		print((0:40)[idx=])
	}
}
oneMore <- function(var) base::'+'(var, 1)  		# weird way to access + operator
square <- function(x) x+x   						# works for x=2, so that's not bad
identity <- function(x) x
apply <- function(arg=0, func=identity) func(arg)

main()
