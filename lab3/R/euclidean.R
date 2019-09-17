#' @title Euclidean Algorithm
#' 
#' @description \code{euclidean} finds the greatest common divisor of two integers.
#' 
#' @param a A number.
#' @param b A number.
#' @return Greatest common divisor between \code{a} and \code{b}
#' @seealso \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Wikipedia}
#' @aliases eucl euclidean_algorithm
#' @family \code{\link{dijkstra}}
#' @examples
#' euclidean(123612,13892347912)
#' euclidean(100,1000)

euclidean <- function(a,b){
	stopifnot( !is.na(as.numeric(a)) , !is.na(as.numeric(a)) )
	while(b!=0){
		aux <- b
		b <- a %% b
		a <- aux
	}
	return(a)
}