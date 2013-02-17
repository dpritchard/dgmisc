## fr_fit
# Wrapper function to fit functional response curves
require(dgmisc)
require(emdbook)
require(bbmle)
require(boot)

fr_fit <- function(formula, data, response, start=list(), fixed=list(), boot=FALSE, nboot=999, para=TRUE){
	# Parse call, can check formula...
	call <- match.call()
	mf <- match.call(expand.dots = FALSE)
	mf_list <- as.list(mf)
	expandmod <- terms(formula(mf_list$formula), data=dat)
	expandform <- formula(expandmod)
	leftside <- all.vars(expandform[[2]])
	rightside <- all.vars(expandform[[3]])
	if(length(leftside)!=1 || length(rightside)!=1) {
		stop('Currently only formulae with one dependent and one independent variable (e.g. y ~ x) are supported.')
	}
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    # moddata is the data we need for the fitting...
    moddata <- eval.parent(mf)
    names(moddata) <- c('Y', 'X')
    
    # Plausibly, 'response' might be the function itself, which isn't helpful at this point.
    if(is.function(response)){
    	response <- as.character(mf_list$response)
    }
    
    # Check we can deal with the requested response
    resp_known =names(fr_responses())
    resp_check <- match(response, resp_known, 0L)
    if(resp_check==0){
    	stop("'response' not recognised. Use fr_responses(show=T) to see what has been implemented.")
    }
    
    # Check start
    if(length(start)==0){
    	stop("You didn't provide starting values. It's to fit anything without knowing what to optimise!")
    }
    if(any(lapply(start, length)!=1)){
    	stop("The items in start must be named numeric value of length one.")
    }
    if(!(all(is.numeric(unlist(start))))){
    	stop("The items in start must be named numeric values.")
    }
    
    # Check fixed
    if(length(fixed)>0 && any(lapply(fixed, length)!=1)){
    	stop("The items in fixed must be named numeric value of length one.")
    }
    if(length(fixed)>0 && !(all(is.numeric(unlist(fixed))))){
    	stop("The items in fixed must be named numeric values.")
    }
   
    optimised_vals <- names(start)
    fixed_vals <- names(fixed)
    
    # Check we have everything we need
    req_input <- names(formals(response))
    req_input  <- req_input[req_input!='X']
    input_matches <- match(req_input, c(names(start), names(fixed)), NA)
    if(any(is.na(input_matches))){
    	missing_input <- req_input[is.na(input_matches)]
    	if(length(missing_input)>1){
    		stop(paste("Your requested response function requires input: ", paste(req_input, collapse=', '), ".\n  The following items are missing: ", paste(missing_input, collapse=', '), ".\n  Please provide them via 'start' or 'fixed', as appropriate.", sep=''))
    	} else {
    		stop(paste("Your requested response function requires input: ", paste(req_input, collapse=', '), ".\n  The following item is missing: ", paste(missing_input, collapse=', '), ".\n  Please provide it via 'start' or 'fixed', as appropriate.", sep=''))
    	}
    }
    
    ## Go time!
    # Common output
    out <- list('call' = call, 'data' = data, 'xvar' = rightside, 'yvar' = leftside)
    iswindows <- FALSE
    # Bootstrapping
	if(boot){
    	# Setup output
    	class(out) <- c(class(out), 'fr_boot')
  		
    	# Figure out what to do about parallel processing
    	if(para){
    		os <- as.character(Sys.info()['sysname'])
    		if(is.na(match(tolower(os), 'windows'))) {
    			paramode <- 'multicore'
    		} else {
    			paramode <- 'snow'
    			iswindows <- TRUE
    		}
    	} else {
    		paramode = 'no'
    	}
		
		# TODO: Make this a little more dynamic, allow people to specify ncores
    	ncores <- parallel:::detectCores()
    	
    	# Print some output to calm people's nerves!
    	cat('\nNow bootstrapping.  Please be patient...\n')
    	flush.console()
    	
    	## Case specific fitting...
    	# rogersII
    	if(response=='rogersII'){
    		frout <- boot(data=moddata, statistic= rogersII_fit, R=nboot, parallel=paramode, ncpus=ncores, start=start, fixed=fixed, windows=iswindows)
    		if(size(frout$t,1)!=nboot){stop("Bootstrap function didn't return nboot rows. This should be impossible!")}
    		out[['a0']] <- as.numeric(frout$t0['a'])
    		out[['h0']] <- as.numeric(frout$t0['h'])
    		out[['sample']] <- frout$t[,3:size(frout$t,2)]
    		out[['a']] <- frout$t[,which(names(frout$t0)=='a')]
			out[['h']] <- frout$t[,which(names(frout$t0)=='h')]
			out[['n_failed']] <- sum(is.na(out[['a']]))
			out[['n_duplicated']] <- sum(duplicated(out[['sample']]))
			out[['n_boot']] <- nboot
    	# No function
    	} else {
    		stop('Unknown function.  This should be impossible!')
    	}
	
    # Not bootstrapping
    } else {
    	# Setup output
    	class(out) <- c(class(out), 'fr_fit')
    	
    	# In this instance, the sample is just the data itself...
    	samp=c(1:nrow(moddata))
    	
    	## Case specific fitting...
    	# rogersII
    	if(response=='rogersII'){
    		frout <- rogersII_fit(data=moddata, samp=c(1:nrow(moddata)), start=start, fixed=fixed)
    		if(size(frout,1)>1){stop('Fit function returned more than one row. This should be impossible!')}
    		out[['a0']] <- as.numeric(frout['a'])
    		out[['h0']] <- as.numeric(frout['h'])
    		out[['sample']] <- samp
    		if(is.na(out$a0) || is.na(out$h0)){stop('The fit failed.  Try some different starting values?')}
    	# No function
    	} else {
    		stop('Unknown function.  This should be impossible!')
    	}
    }
    
    # For bootstarpped data, we need to check the number of failures
    if(inherits(out, 'fr_boot')){
    	prop_fail <- out[['n_failed']]/out[['n_boot']]
    	if(prop_fail>0.5){
    		out <- NULL
    		stop('More than 50% of the fits failed. This is an error.  Nothing will be returned.')
    	} else if(prop_fail>0.1){
    		warning('More than 10% of the fits failed. Suggest careful consideration of the output.')
    	}
    }
    return(out)
}