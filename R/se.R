se <- function(x, na.rm=FALSE){
	if(na.rm){
	lenx <- length(na.omit(x))
	} else {
	lenx <- length(x)
	}
	se <- sd(x)/sqrt(lenx)
	return(se)
}
