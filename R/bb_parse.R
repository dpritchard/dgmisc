# v 1.1
bb_parse <- function(raw_bb, verbose=TRUE, log=FALSE) {
	## Check all required columns are present
	# NB: If you change this, remeber to change the name reassignment below!
	req_cols<-c('Year', 'Season', 'Date', 'Sampling.Unit', 'Scientific.Name', 'Modified.Braun.Blanquet.Cover.Abundance.Score..1.7.', 'Condition.Score..1.5.')
  	#Check all required columns are present
  	for (a in 1:length(req_cols)) {
    	if (is.na(match(req_cols[a], names(raw_bb)))) {
      		stop(paste("The supplied data does not have a column named '", req_cols[a], "'.", sep=''))
    	}
  	}
  	
  	# Subset to only keep required columns
  	raw_bb_sub <- raw_bb[,req_cols]
  	
  	# Sorting out column names
  	names(raw_bb_sub)[c(4,5,6,7)]<-c('SampUnit', 'Species','Abund', 'Cond')
  	
  	# Sorintg out dates
  	raw_bb_sub$RDate<-as.POSIXct(strptime(raw_bb_sub$Date, '%m/%d/%Y'))
  	if (verbose) {
  		cat('\n')
  		message("The parsing function assumes that 'Date' column is in Month/Day/Year format (this is pretty American though, don't ya think?)")
  	}

  	# The following line is a manual re-code of season order, check order using 'levels'
  	if (length(levels(raw_bb_sub$Season))>4) {
    	stop("You have more than four seasons? I don't think so. Check your data and try again.") 
  	}
  	raw_bb_sub$Season<-factor(raw_bb_sub$Season, levels(raw_bb_sub$Season)[c(3,1,4,2)])
  	
  	# Creating the 'season year' column
  	## Needed for the 'species richness' plots
  	raw_bb_sub$SeasonYear <- paste(raw_bb_sub$Season, raw_bb_sub$Year, sep=' ')
  	
  	## Data checks ##
  	# TODO:
  	# Check for blank species names
  	# List more specifics for completly missing condition scores (currently we return the numbers that are missing only)
  	# Print info about WHICH cond and abundances are to high 
  	# Logging to file 	
  	
  	# Find entries with nonzero condition and abundance scores
  	raw_bb_sub$HasNonzeroCond <- raw_bb_sub$Cond>0
  	raw_bb_sub$HasNonzeroCond[is.na(raw_bb_sub$HasNonzeroCond)] <- FALSE
  	
  	raw_bb_sub$HasNonzeroAbund <- raw_bb_sub$Abund>0
  	raw_bb_sub$HasNonzeroAbund[is.na(raw_bb_sub$HasNonzeroAbund)] <- FALSE
  	
  	# Get some info about the condition and abundance scores (formally 'drop unused species scores').
  	len_all_Cond <- length(raw_bb_sub$Cond)
  	len_na_Cond <- sum(is.na(raw_bb_sub$Cond))
  	len_has_Cond <- sum(raw_bb_sub$HasNonzeroCond)
  	len_Cond_too_high <- sum(raw_bb_sub$Cond>5, na.rm=T)
  	which_Cond_too_high <- which(raw_bb_sub$Cond>5) # Not used, yet!
  	len_all_Abund <- length(raw_bb_sub$Abund)
  	len_na_Abund <- sum(is.na(raw_bb_sub$Abund))
  	len_has_Abund <- sum(raw_bb_sub$HasNonzeroAbund)
  	len_Abund_too_high <- sum(raw_bb_sub$Abund>7, na.rm=T)
  	which_Abund_too_high <- which(raw_bb_sub$Abund>7) # Not used, yet!
  	cond_abund_info <- matrix(data=c(
  		len_all_Cond, len_na_Cond, len_all_Cond-len_na_Cond, len_has_Cond, len_Cond_too_high, NA,
  		len_all_Abund, len_na_Abund, len_all_Abund-len_na_Abund, len_has_Abund, NA, len_Abund_too_high), 
  		nrow=2, ncol=6, byrow=TRUE,
  		dimnames=list(c('Condition', 'Abundance'), c('Total', 'NAs', 'Values', 'NonZeroValues', 'NumberCond>5', 'NumberAbunds>7')))
  	  	
  	# Check for multiple condition scores per species / date / samp.unit combo.
  	raw_bb_cond_sub <- raw_bb_sub[raw_bb_sub$HasNonzeroCond,]
  	cond_xtab <- xtabs(~Date+Species+SampUnit, data=raw_bb_cond_sub)
  	cond_errors <- as.data.frame(which(cond_xtab>1, arr.ind=T), row.names=NA)
  	if (dim(cond_errors)[1]>0) {
  		names(cond_errors) <- names(dimnames(cond_xtab))
  		error_dates <- dimnames(cond_xtab)$Date[cond_errors$Date]
  		error_species <- dimnames(cond_xtab)$Species[cond_errors$Species]
		error_sampunit <- dimnames(cond_xtab)$SampUnit[cond_errors$SampUnit]
		multiple_condition <- data.frame('Date'=error_dates, 'Species'=error_species, 'SampUnit'=error_sampunit)
		has_multiple_condition <- TRUE
  	} else {
  		multiple_condition <- NA
  		has_multiple_condition <- FALSE
  	}
  	
  	# Check that all species that have a condition score have an abundance score.
  	has_cond_but_no_abund <- which(!raw_bb_cond_sub$HasNonzeroAbund)
  	if (length(has_cond_but_no_abund)>0) {
  		missing_abund <- with(raw_bb_cond_sub[has_cond_but_no_abund,], data.frame('Date'=Date, 'Species'=Species, 'SampUnit'=SampUnit))
  		has_missing_abund <- TRUE
  	} else {
  		missing_abund <- NA
  		has_missing_abund <- FALSE
  	}
  	
  	#For the species richeness data, check if a species exisits multiple times per sampling unit (within each year / season).
  	# 'Species richness' == Species with non-zero abundances
  	raw_bb_abund_sub <- raw_bb_sub[raw_bb_sub$HasNonzeroAbund,]
  	abund_xtab <- xtabs(~Date+Species+SampUnit, data=raw_bb_abund_sub)
  	abund_errors <- as.data.frame(which(abund_xtab>1, arr.ind=T), row.names=NA)
  	if (dim(abund_errors)[1]>0) {
  		names(abund_errors) <- names(dimnames(abund_xtab))
  		error_dates <- dimnames(abund_xtab)$Date[abund_errors$Date]
  		error_species <- dimnames(abund_xtab)$Species[abund_errors$Species]
		error_sampunit <- dimnames(abund_xtab)$SampUnit[abund_errors$SampUnit]
		multiple_abundance <- data.frame('Date'=error_dates, 'Species'=error_species, 'SampUnit'=error_sampunit)
		has_multiple_abundance <- TRUE
  	} else {
  		multiple_abundance <- NA
  		has_multiple_abundance <- FALSE
  	}
  	
  	## Collect output
  	class(raw_bb_sub) <- c(class(raw_bb_sub), 'parsed.bb')
  	out <- list('parsed_bb'=raw_bb_sub, 'cond_abund_info'=cond_abund_info, 'has_multiple_condition'=has_multiple_condition, 'multiple_condition'=multiple_condition, 'has_missing_abund'=has_missing_abund, 'missing_abund'=missing_abund, 'has_multiple_abundance'=has_multiple_abundance, 'multiple_abundance'=multiple_abundance)
  	
  	if (log) {
  		dt <- strptime(date(), '%c')
  		today_date <- format(dt, '%Y-%m-%d-%H-%M')
  		n_samp_units <- length_unique(out$parsed_bb$SampUnit, verbose=FALSE)$lenuni
  		samp_units <- paste(as.character(unique(out$parsed_bb$SampUnit)), collapse='-')
  		if(n_samp_units > 6) {
  			su_names <- paste(n_samp_units, 'Sampling', 'Units', sep='-')
  			many_names = TRUE
  		} else {
  			su_names <- samp_units
  			many_names = FALSE
  		}
  		fname <- paste(today_date, '_', su_names, '.txt', sep='')
  		if(many_names) {
  			cat('\n')
  			message(paste("NB: There were more than 6 sampling units. The logfile name has been concatendated to ", fname, ".", sep=''))
  		}
  		sink(file = fname, append=FALSE)
  		cat(paste(rep('-', times=70), collapse=''))
  		cat('\n')
  		cat(paste('Log file created by bb_parse.R on ', format(dt, '%d/%m/%Y'), ' at ', format(dt, '%H:%M'),'\n\n', sep=''))
  		cat('Sampling units processed: ')
  		cat(paste(as.character(unique(out$parsed_bb$SampUnit)), collapse=', '))
  		cat('\n')
  		cat(paste(rep('-', times=70), collapse=''))
  		cat('\n')
  		sink()
  		sink(file = fname, append=TRUE)
  		cat('\nGeneral Info\n')
  		print(out$cond_abund_info)
  		
  		if (out$has_multiple_condition) {
  			cat('\n')
  			cat('Data entires with multiple condition scores:\n')
  			print(out$multiple_condition)
  		} else {
  			cat('\n')
  			cat('There were no entires with multiple condition scores!\n')
  		}
  		
  		if (out$has_missing_abund) {
  			cat('\n')
  			cat('Data entries with condition scores, but no abundance scores:\n')
  			print(out$missing_abund)
  		} else {
  			cat('\n')
  			cat('There were no entries with condition scores that are missing abundance scores!\n')
  		}
  		
  		if (out$has_multiple_abundance) {
  			cat('\n')
  			cat('Data entires with multiple abundance scores:\n')
  			print(out$multiple_abundance)
  		} else {
  			cat('\n')
  			cat('There were no entires with multiple abundance scores!\n')
  		}  		
  		sink()
  	}
  	
  	if (verbose) {
  		cat('\n')
	  	message("Here is some info about your data:")
	  	print(out$cond_abund_info)
	  	
	  	if (out$has_multiple_condition) {
  			cat('\n')
  			message("Some species have more than 1 condition score per sampling date / sampling unit. This will likely give you ugly / meaningless plots. The problem entries are:")
  			print(out$multiple_condition)
  		}
  		
  		if (out$has_missing_abund) {
  			cat('\n')
  			message("Some species which have condition scores do not have an abundance score. This is probably an error. The problem entries are:")
  			print(out$missing_abund)
  		}
  		
  		if (out$has_multiple_abundance) {
  			cat('\n')
  			message("Some species have more than 1 abundance score per sampling date / sampling unit. This will likely give you ugly / meaningless plots (especially in 'species richness' plots). The problem entries are:")
  			print(out$multiple_abundance)
  		}
  	 }

  	 return(out) 	
}