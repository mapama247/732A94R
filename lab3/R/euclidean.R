# 3.1.1 EUCLIDEAN ALGORITHM

# function that finds thes greatest common divisor of two given integers
# pseudocode: https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean <- function(a,b){
	# Assert that the arguments are numeric scalars or integers
	stopifnot( !is.na(as.numeric(a)) , !is.na(as.numeric(a)) )
	
	while(b!=0){
		aux <- b
		b <- a %% b
		a <- aux
	}
	return(a)
}

euclidean(123612, 13892347912) # 4
euclidean(100, 1000) # 100
