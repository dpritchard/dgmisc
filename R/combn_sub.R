combn_sub <- function(x, m, sub=NA){
	if(is.na(sub)){
		sub_undeclared = TRUE
		sub <- choose(x,m)
		if(sub==0){
			stop('Number of possible combinations is zero.')
		}
	} else {
		sub_undeclared = FALSE
	}
	# Check if sub > number of unique combinations...
	if(sub > choose(x,m)) {
		stop('sub is greater than the number of unique combinations. sub must be <= choose(x,m).')
	}
	# OK, so the input should be OK at this point...
	all_possible <- try(combn(x,m), silent=TRUE)
	if(inherits(all_possible, 'try-error')) {
		if(grep('too large or NA', all_possible[1])) {
			if(sub_undeclared) {
				stop("Too many items to iterate all possible combinations. You must supply 'sub'.")
			}
			warning('Too many items to iterate all possible combinations. Manually selecting a subset.')
			outmat <- matrix(nrow=m, ncol=sub)
			wsub = 1 
			while(wsub <= sub) {
				samp <- sample(c(1:x), m)
				if(!any(colSums(outmat==samp, na.rm=TRUE)==nrow(outmat))) {
					# The new column is unique. Add it.
					outmat[,wsub] <- samp
					wsub = wsub+1
				}
			}
		} else {
			stop(cat('Unhandeled exception:\n', all_possible[1]))
		}
	} else {
		all_subset <- sort(sample(c(1:dim(all_possible)[2]), sub))
		outmat <- all_possible[,all_subset]
	}
	return(outmat)
}