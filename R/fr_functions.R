## Functional Response Functions

# Each function needs to have a specification (e.g. rogersII), a fit (e.g. rogersII_fit) and, where appropriate, a maximum likelihood NLL function.  
# Each function specification needs to be listed in 'resp_known' with a description.
# N0 replaced with 'X' for simplicity and consistency.
# X = Number of 'prey' (prey density / concentration)
# Y = Number of prey eaten / consumed / killed / absorbed

## resp_known is the master list of usable functions.
## each named entry here must have a corresponding function entry below (and vice versa!)
fr_responses <- function(show=FALSE){
	resp_known <- list('rogersII'="Roger's Type II decreasing prey function.")
	if(show){
		cat('\n')
		cat('Response', '\t', 'Description', '\n', sep='')
		cat('--------', '\t', '-----------', '\n', sep='')
		for (a in 1:length(resp_known)) { 
			cat(names(resp_known)[a], '\t', resp_known[[a]], '\n', sep='')
		}
		cat('\n')
	} else {
		return(resp_known)
	}
}

## Rogers Type II decreasing prey function ##
# Same as ?lambertW, with the addition of 'P'
# Everything except 'X' should be provided.
rogersII <- function(X, a, h, P, T) {
	X - lambertW(a * h * X * exp(-a * (P * T - h * X)))/(a * h)
}
# rogersII_fit: Does the heavy lifting
# data = The data from which to subsample. X and Y are drawn from here.
# samp = Provided by boot() or manually, as required
# start = List of starting values for items to be optimised.  Usually 'a' and 'h'.
# fixed = List of 'Fixed data' (not optimised).   Usually 'T' and 'P'
# Note required packages are reloaded here so Windows can do parallel computing!
rogersII_fit <- function(data, samp, start, fixed, windows=FALSE) {
	if(windows){
		dgmisc_load <- require(dgmisc, warn.conflicts=FALSE, quietly=TRUE)
		emdbook_load <- require(emdbook, warn.conflicts=FALSE, quietly=TRUE)
		bbmle_load <- require(bbmle, warn.conflicts=FALSE, quietly=TRUE)
		if(any(c(dgmisc_load, emdbook_load, bbmle_load)==FALSE)){
			stop('Error establishing workspace for parallel computing in Windows.')
		}
	}
	samp <- sort(samp)
	data <- data[samp,]
	X <- data$X
	Y <- data$Y
	fixed[['X']] <- X
	fixed[['Y']] <- Y
	try_rogersII <- try(mle2(rogersII_nll, start=start, data=fixed), silent=T) 
	## Remove 'silent=T' for more verbose output
	if (inherits(try_rogersII, "try-error")){
 		# The fit failed...
 		out = c(NA, NA, samp)
 		return(out)
 	} else {
 		out = c(coef(try_rogersII)['a'], coef(try_rogersII)['h'], samp)
 		return(out)
 	}
}	
# rogersII_nll
# Provides negative log-likelihoos for estimations via mle2()
# See Bowkers book for more info
rogersII_nll <- function(a, h, T, P, X, Y) {
	if (a < 0 || h < 0) {
		return(NA)
		}
		prop.exp = rogersII(X, a, h, P, T)/X
		return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
	}


### NOT IMPLEMENTED BELOW HERE! ###

## Holling's Type II function ##
# From ?lambertW
# N0 replaced with 'X' for simplicity and consistency
hollingsII <- function(X,a,h) {
	a*X/(1+a*h*X)
}

