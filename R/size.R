size <- function(x, dim=NA) {
	if(is.vector(x)) {
		out <- c(1,length(x))
	} else {
		out <- dim(x)
	}
	if(is.null(out)){
		warning('Size is NULL.  This might be becuase you supplied a factor. See help(size) for more info.')
		return(out)
	} else if(is.na(dim)) {
		return(out)
	} else {
		return(out[dim])
	}
}