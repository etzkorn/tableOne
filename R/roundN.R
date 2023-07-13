#' Round a Number
#'
#' @param p a numeric vector of p-values to round
#' @return a numeric vector of rounded p-values
#' @export
#'
#' @examples
#' library(tableOne)
#' p <- c(runif(10), pmin(rexp(10,20),0.5))
#' cbind(p, roundP(p))

roundN <- function(n){
	formatC(
		signif(n,digits=3),
		digits=3,format="fg", flag="#")
}
