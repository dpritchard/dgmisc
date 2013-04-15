## Roger's Type II decreasing prey function.
# Each function needs to have a specification (e.g. rogersII), a fit (e.g. rogersII_fit) and, where appropriate, a maximum likelihood NLL function.  
# Each function specification needs to be listed in 'resp_known' (in fr_functions.R) with a description.
# N0 replaced with 'X' for simplicity and consistency.
# X = Number of 'prey' (prey density / concentration)
# Y = Number of prey eaten / consumed / killed / absorbed

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
# Not also that the statistic now (2013-04-13) now returns the variance 
rogersII_fit <- function(data, samp, start, fixed, boot=FALSE, windows=FALSE) {
	if(windows && boot){
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
	# Hard coded upper limits: TODO: Fix this, allow variable data
	if (inherits(try_rogersII, "try-error") || as.numeric(coef(try_rogersII)['a']) > 10 || as.numeric(coef(try_rogersII)['h']) > 1){
 		# The fit failed...
 		out = c(NA, NA, NA, NA, samp)
 		names(out) <- c('a', 'avar', 'h', 'hvar', rep('', times=length(samp)))
 		if(boot){
 			return(out)
        } else {
 			stop(try_rogersII[1])
 		}
 	} else {
 		out = c(coef(try_rogersII)['a'], vcov(try_rogersII)['a', 'a'], coef(try_rogersII)['h'], vcov(try_rogersII)['h', 'h'], samp)
         names(out) <- c('a', 'avar', 'h', 'hvar', rep('', times=length(samp)))
 		if(boot){
 			return(out)
 		} else {
 			return(try_rogersII)
 		}
 	}
}	
# rogersII_nll
# Provides negative log-likelihood for estimations via mle2()
# See Bowkers book for more info
rogersII_nll <- function(a, h, T, P, X, Y) {
	if (a < 0 || h < 0) {
		return(NA)
		}
		prop.exp = rogersII(X, a, h, P, T)/X
		return(-sum(dbinom(Y, prob = prop.exp, size = X, log = TRUE)))
	}