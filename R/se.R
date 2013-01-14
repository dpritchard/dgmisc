se <- function(x, na.rm=FALSE){
	if (!is.vector(x)){
		stop("'x' must be a vector. See ?se for further information.")
	}
	if(na.rm){
		se <- sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))
	} else {
		se <- sd(x)/sqrt(length(x))
	}
	return(se)
}
