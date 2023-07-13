#' Round a P-Value
#'
#' @param p a numeric vector of p-values to round
#' @return a numeric vector of rounded p-values
#' @export
#'
#' @examples
#' library(tableOne)
#' p <- c(runif(10), pmin(rexp(10,20),0.5))
#' cbind(p, roundP(p))

roundP <- function(p){
	ifelse(
		p <= 0.001, 0.001,
		ifelse(
			p<=0.1, as.character(round(p, digits = 3)),
			ifelse(p<=0.2, round(p, digits = 2),
				   	   round(p,  digits = 1)
				   )
			)
	)
}

roundN <- function(n){
	ifelse(
		n <= 1, round(n, 3),
		ifelse(
			n<=10, round(n, 2),
			ifelse(
				n<=100, round(n,  1),
				round(n,  digits = 0)
			)
		)
	)
}
