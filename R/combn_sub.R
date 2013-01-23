combn_sub <- function(n, m, sub=NA){
    if(length(n)>1) {
        stop("'n' must be a single integer. See help(combn_sub) for more info.")
    }
    
	no_possible <- choose(n,m)
	
	if(no_possible==0){
		stop('Number of possible combinations is zero.')
	}
	
	# If sub is undeclared, set it to the number of posisble combinations
	sub_undeclared = FALSE
	if(is.na(sub)){
		sub <- no_possible
		sub_undeclared = TRUE
	}
	
	# If sub supplied, check if sub > number of unique combinations...
	if(sub > choose(n,m)) {
		stop('sub is greater than the number of unique combinations. sub must be <= choose(x,m).')
	}
	
	# For performace reasons, we limit the number of subsets to 5000
	# NB: An arbirary limit.  Unlikely in ecology to need more than this?
	if(sub > 5000) {
		if(sub_undeclared) {
			stop("You did not provide 'sub' and when trying to return all possible outcomes the (arbitary) 5000-item limit was exceeded. See help(combn_sub) for details.")
		} else {
			stop('You requested more than 5000 unique combinations.  For performance reasons, the maximum number of posisble combinations is limited to 5000. See help(combn_sub) for details.')
		}
	}
	
	# If there are more than 500 thousand possible combinations then we will use the the manual (old school) approach.
	# This also means that there will always be 1000-times more possible combinations available than the maximum requested.
	# NB: This is also an arbitary limit base don performance on DP's MacBook.  
	old_school <- FALSE
	if(no_possible > 500000) {
		old_school <- TRUE
	}
	
	# OK, so the input should be OK at this point...
	if(old_school) {
		outmat <- matrix(nrow=m, ncol=sub)
		wsub = 1 
		while(wsub <= sub) {
			samp <- sort(sample(c(1:n), m))
			if(!any(colSums(outmat==samp, na.rm=TRUE)==nrow(outmat))) {
				# The new column is unique. Add it.
				outmat[,wsub] <- samp
				wsub = wsub+1
			}
		}
	} else {
		all_possible <- combn(n,m)
		all_subset <- sort(sample(c(1:dim(all_possible)[2]), sub))
		outmat <- all_possible[,all_subset]
	}	
	return(outmat)
}