# Tools for so-called functional response analysis

## Rogers Type II decreasing prey frunction ##
# Same as ?lambertW, with the addition of 'P'
rogers.pred = function(N0, a, h, P, T) {
 N0 - lambertW(a * h * N0 * exp(-a * (P * T - h * N0)))/(a * h)
 }

## negative log-likelihood function ##
NLL.rogers = function(a, h, T, P, initial, killed) {
 		if (a < 0 || h < 0) {
 			return(NA)
 		}
 		prop.exp = rogers.pred(initial, a, h, P, T)/initial
 		return(-sum(dbinom(killed, prob = prop.exp, size = initial,log = TRUE)))
 	}

## Statistic function. For Bootstraping using the boot library. ##
statistic<-function(data,samp) {
	samp <- sort(samp)
	data <- data[samp,]
	initial <- data$density
	killed <- data$eaten
	try.FFR.rogers <- try(mle2(NLL.rogers, start = list(a = 1.2, h = 0.015), data = list(T = 1, P = 1, initial = initial, killed = killed)), silent=T) ## Remove 'silent=T' for more verbose output
	if (inherits(try.FFR.rogers, "try-error")){
 		# The fit failed...
 		out = c(NA, NA, samp)
 		return(out)
 	} else {
 		out = c(coef(try.FFR.rogers)['a'], coef(try.FFR.rogers)['h'], samp)
 		return(out)
 	}
}	
## -- ##

## Statistic function. For Bootstraping using the boot library when using snow-based parallisation. ##
statistic_snow<-function(data,samp) {
  samp <- sort(samp)
  library(emdbook)
  library(bbmle)
  library(boot)
  source("bootstrapping_funcs.R")
  data <- data[samp,]
  initial <- data$density
  killed <- data$eaten
  try.FFR.rogers <- try(mle2(NLL.rogers, start = list(a = 1.2, h = 0.015), data = list(T = 1, P = 1, initial = initial, killed = killed)), silent=T) ## Remove 'silent=T' for more verbose output
  if (inherits(try.FFR.rogers, "try-error")){
    # The fit failed...
    out = c(NA, NA, samp)
    return(out)
  } else {
    out = c(coef(try.FFR.rogers)['a'], coef(try.FFR.rogers)['h'], samp)
    return(out)
  }
}	
## -- ##

## parse_boot to parse the output from the boot function and extract desirable stats. ##
# Can also be used to preallocate memory for big runs e.g.: out <- parse_boot(NA, prealloc=T, num_rows=dim(run_list)[1])
parse_boot <-function(boot_out, prealloc=FALSE, num_rows=1) {
	col_names = c('n_data', 'n_boot', 'n_failed', 'n_duplicated', 'a_min', 'a_25', 'a_median', 'a_75', 'a_max', 'a_mean', 'a_sd', 'h_min', 'h_25', 'h_median', 'h_75', 'h_max', 'h_mean', 'h_sd')
	parse_out <- matrix(NA, nrow=num_rows, ncol=length(col_names), dimnames=list(NULL, col_names))
	if (prealloc) {
		return(parse_out)
	}

	ayes <- boot_out$t[,1]
	hatches <- boot_out$t[,2]
	samples <- boot_out$t[,3:dim(boot_out$t)[2]]
	
	parse_out[1,'n_data'] <- nrow(boot_out$data)
	parse_out[1,'n_boot'] <- boot_out$R
	if (sum(is.na(ayes)) != sum(is.na(hatches))) {
		stop("Somehow we got inequal numbers of a's and h's")
	}
	parse_out[1,'n_failed'] <- sum(is.na(ayes))
	parse_out[1,'n_duplicated'] <- sum(duplicated(samples))
	
	a_quan <- quantile(ayes, na.rm=T)
	parse_out[1,'a_min'] <- as.numeric(a_quan[1])
	parse_out[1,'a_25'] <- as.numeric(a_quan[2])
	parse_out[1,'a_median'] <- as.numeric(a_quan[3])
	parse_out[1,'a_75'] <- as.numeric(a_quan[4])
	parse_out[1,'a_max'] <- as.numeric(a_quan[5])
	
	parse_out[1,'a_mean'] <- mean(ayes, na.rm=T)
	parse_out[1,'a_sd'] <- sd(ayes, na.rm=T)
	
	h_quan <- quantile(hatches, na.rm=T)
	parse_out[1,'h_min'] <- as.numeric(h_quan[1])
	parse_out[1,'h_25'] <- as.numeric(h_quan[2])
	parse_out[1,'h_median'] <- as.numeric(h_quan[3])
	parse_out[1,'h_75'] <- as.numeric(h_quan[4])
	parse_out[1,'h_max'] <- as.numeric(h_quan[5])
	
	parse_out[1,'h_mean'] <- mean(hatches, na.rm=T)
	parse_out[1,'h_sd'] <- sd(hatches, na.rm=T)
	
	return(parse_out)
	}	
## -- ##