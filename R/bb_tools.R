#bb_read v 1.0
bb_read<-function(file, survey_year=NA, survey_season=NA){
    require(stringr)
    #get a field data
    datacore <- read.csv(file=file, skip=6, stringsAsFactors=F)
    #get field metadata
    datameta <- read.csv(file=file, nrows=6,stringsAsFactors=F)
    
    # First column needs to be strings (should be species names!)
    if(!is.character(datacore[,1])) {
        stop(cat(file,': First column of data are not strings. They really need to be species names.', sep=''))
    }
    #checking that presence column has data
    bp<- length(datacore[,2])
    
    if(bp==0){
        stop(cat(file,": The presence column is blank, you are missing some key data.", sep=''))
    }
    
    #and checking that if presence=1, then there is abundance data
    abund<-which(datacore[,3]!='')
    pres<-which(datacore[,2]!='')
    
    if(!length(abund)==length(pres)){
        stop(cat(file,": Blank rows. May be missing presence or abundance data?", sep=''))
    }
    
    #dropping rowing with no species names 
    if(any(datacore[,1]=='')) {
        stop(cat(file,': There are blanks in the first column. May be missing species names?', sep=''))
    }
    
    nbr<-which(datacore[,1]!='')
    
    if(!length(nbr)==max(nbr)){
        stop(cat(file,": Blank rows. May be missing species names?", sep=''))
    }
    
    br<-which(datacore[,1]==''| datacore[,2]=='' )
    
    #checking that dropped rows didn't contain data
    if(all(is.na(datacore[br,3:4]))){
        stop(cat(file,": Data exist without a species name or presence tick", sep=''))
    }
    
    data_nb <- datacore[pres,]
    
    #finding the columns with no data
    data_spp <- data_nb[,3:4]
    nbc<-as.integer(which(colSums(!is.na(data_spp))>0))
    
    if(!length(nbc)==max(nbc)){
        stop(cat(file,": Either (or both) abundance or condition column is blank, you are missing some key data.", sep=''))
    }
    
    #index out only non-blank rows and columns
    dat <- cbind(data_nb[,1], data_spp[,nbc], data_nb[,7])
    
    #cleaning metadata
    if(!is.character(datameta[,1]) & !is.character(datameta[,2])) {
        stop(cat(file,': First two columns of metadata are not strings. They really should be.', sep=''))
    }
    
    # Testing for presences of valid 'Sampling Unit' info
    if(!str_detect(tolower(str_trim(datameta[1,1])), '^samp')){
        stop(cat(file,": Metadata missing header 'Sampling unit' "))
    }
    
    if(str_trim(datameta[1,2])==''){
        stop(cat(file,": Metadata missing sampling unit name."))
    }
    
    # Testing for presences of valid 'Date' info
    if(!str_detect(tolower(str_trim(datameta[2,1])), '^date')){
        stop(cat(file,": Metadata missing header 'Date' "))
    }
    
    if(str_trim(datameta[2,2])==''){
        stop(cat(file,": Metadata missing date."))
    }
    
    # Testing for presences of valid 'Assessor' info
    if(!str_detect(tolower(str_trim(datameta[3,1])), '^ass')){
        stop(cat(file,": Metadata missing header 'Assessor'."))
    }
    
    if(str_trim(datameta[3,2])==''){
        stop(cat(file,": Metadata missing sampling assessor name."))
    }
    
    
    
    ## All datachecks done. Now construct output
    out = NULL
    datout = NULL
    metadatout = NULL
    
    outsamp_year <- survey_year
    outsamp_season <- survey_season
    outmeth <- 'BB'
    outmsu <- str_trim(datameta[1,2])
    outmdate <- str_trim(datameta[2,2])
    outmass <- str_trim(datameta[3,2])
    
    metadataout <- data.frame('Year' = outsamp_year, 'Season' = outsamp_season,'Method Code' = outmeth, 'Sampling Unit ID' = outmsu, 'Start Date (MM/DD/YYYY)' = outmdate, 'Assessor'= outmass, stringsAsFactors=F)
    
    outnrow <- nrow(dat)
    outsamp_year <- rep(survey_year, times=outnrow)
    outsamp_season <-rep(survey_season, times=outnrow)
    outmeth <- rep('BB', times=outnrow)
    outsu <- rep(outmsu, times=outnrow)
    outdate <- rep(outmdate, times=outnrow)
    outass <- rep(outmass, times=outnrow)
    
    outoldspp <- dat[,1]
    outabund <- dat[,2]
    outcond <- dat[,3]
    outnote <- dat[,4]
    
    datout <- data.frame('Year' = outsamp_year,'Season' = outsamp_season,  'Method Code' = outmeth, 'Start Date (MM/DD/YYYY)' = outdate, 'Sampling Unit ID' = outsu, 'Assessors' = outass,'Field Name' = outoldspp, 'Modified Braun-Blanquet Cover/Abundance Score (1-7)' = outabund,  'Condition Score (1-5)' = outcond,'Comments'= outnote, stringsAsFactors=F)
    
    out <- list('metadata' = metadataout, 'data' = datout)
    
    return(out)
}

# bb_parse v 1.3
bb_parse <- function(raw_bb, verbose=TRUE, log=FALSE) {
    ## Check all required columns are present
    # NB: If you change this, remeber to change the name reassignment below!
    req_cols<-c('Year', 'Season', 'Start.Date..YYYY.MM.DD.', 'Sampling.Unit.ID', 'Scientific.Name', 'Modified.Braun.Blanquet.Cover.Abundance.Score..1.7.', 'Condition.Score..1.5.')
    #Check all required columns are present
    for (a in 1:length(req_cols)) {
        if (is.na(match(req_cols[a], names(raw_bb)))) {
            stop(paste("The supplied data does not have a column named '", req_cols[a], "'.", sep=''))
        }
    }
    
    # Subset to only keep required columns
    raw_bb_sub <- raw_bb[,req_cols]
    
    # Sorting out column names
    names(raw_bb_sub)[c(3,4,5,6,7)]<-c('Date', 'SampUnit', 'Species','Abund', 'Cond')
    
    # Sorintg out dates
    raw_bb_sub$RDate<-as.POSIXct(strptime(raw_bb_sub$Date, '%Y/%m/%d'))
    if (verbose) {
        cat('\n')
        message("The parsing function assumes that 'Date' column is in Year/Month/Day format (good to see we are using the correct format now)")
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

# bb_plot_ca v. 1.4
bb_plot_ca <- function(parsed_bb, samp_unit=NA, plot_to='screen', verbose=TRUE, disturbance=FALSE){
  
  require(reshape) 
  
  ## Check to see if data are parsed or not.
  # Parse if needed.
  if (!inherits(parsed_bb,"parsed.bb")) {
  	if (verbose) {
  		cat('\n')
  		message("You provided raw data. It will be automatically parsed by bb_parse().  It is HIGHLY recomended that you run bb_parse() first with either the 'verbose' or 'log' functions enabled.")
  	}
  	parsed_bb_out <- bb_parse(parsed_bb, verbose=FALSE, log=FALSE)
  	parsed_bb <- parsed_bb_out$parsed_bb
  }
  ##
  
  ## Check sampling unit
  if (any(is.na(samp_unit))) {
    samp_unit <- as.character(unique(parsed_bb$SampUnit))
    if (length(samp_unit) != 1) {
      stop('Sampling unit (samp_unit) not supplied and a single unique sampling unit could not be derived from the data.')      
    }	
  } else {
  	if (!is.character(samp_unit)) {
      stop('Sampling unit (samp_unit) must be a charater string')
    }
    if (length(samp_unit) != 1) {
  	  stop('Sampling unit (samp_unit) for bb_plot_ca() must be a single character string.  See ?bb_plot_ca for details.')
  	}
    if (is.na(match(samp_unit, unique(parsed_bb$SampUnit)))) { 
      stop('Sampling unit (samp_unit) was supplied but is not actually in the data!')
    }
  } 
  # Assuming that is all OK, subset by sampling unit
  parsed_bb_sampunit <- parsed_bb[parsed_bb$SampUnit==samp_unit,]
  ## -- ##

  ## Subset data for plotting ##
  ## Get data that are in the required sampling unit and have non-NA condition scores:
  parsed_bb_sub<-parsed_bb_sampunit[parsed_bb_sampunit$HasNonzeroCond,]
  ## -- ##

  ## NB: Data should be OK to plot (though it might still be bullshit, but that's OK)
  
  ## Get summary stats:
  # Melt data (using reshape package)
  parsed_bb_subM<-melt(parsed_bb_sub[,c('Year','Season','RDate','Abund','Cond')], id=c('Year','Season','RDate'))
  # Cast data with median
  parsed_bb_subC<-cast(parsed_bb_subM, RDate~variable, fun.aggregate=median, na.rm=T)
  ## -- ##
  
  ## Labels for both plots
  dat_lab_loc <- paste(rep('00:00 01-07-', length(unique(parsed_bb_sub$Year))),unique(parsed_bb_sub$Year),sep='')
  dat_lab_loc <- as.POSIXct(strptime(dat_lab_loc, '%H:%M %d-%m-%Y'))
  dat_lab_ticks <- paste(rep('00:00 01-01-', length(unique(parsed_bb_sub$Year))),unique(parsed_bb_sub$Year),sep='')
  dat_lab_ticks <- as.POSIXct(strptime(dat_lab_ticks, '%H:%M %d-%m-%Y'))
  seas_labs <- unique(parsed_bb_subM[,c('Season', 'RDate')])
  seas_labs$RDate <- as.POSIXct(seas_labs$RDate, '%d-%m-%Y') 
  ## -- ##
  
  ## xlim for both plots.
  xminimum <- as.POSIXct(strptime(paste('01-01-', min(parsed_bb_sub$Year), sep=''), '%d-%m-%Y'))
  xmaximum <- as.POSIXct(strptime(paste('31-12-', max(parsed_bb_sub$Year), sep=''), '%d-%m-%Y'))
  xlimits <- c(xminimum,xmaximum)

  ## Plot Setup  
  if (plot_to=='png'){
    fname = paste(format(Sys.Date(), '%Y-%m-%d'), '_', samp_unit, '_ca.png', sep='')
    png(filename=fname, units='cm', height=(29.7/2)*1.3, width=21*1.5, res=72*2)
    layout(matrix(c(1,2), ncol=1))
    par(mai=c(1.2, 0.6, 0.1, 0.1)) 
  } else if (plot_to=='screen') {
    layout(matrix(c(1,2), ncol=1))
    par(mai=c(1.5, 0.8, 0.1, 0.1)) 
  } else {
    stop("The plot destination (plot_to) must be either 'png' or 'screen'")
  }
  ## -- ##
  
  ## Plot Condition
  #Setup
  with(parsed_bb_sub, plot(RDate, Cond, type='n', ylim=c(0,5.05), xlim=xlimits, yaxs='i', xaxs='i', axes=F, ann=F))
  with(seas_labs, axis(1, labels=Season, at=RDate, las=2, lwd=0, lwd.ticks=1))
  mtext(side=1, text=format(dat_lab_loc, '%Y'), at=dat_lab_loc, line=4)
  axis(1, at=dat_lab_ticks, labels=NA, lwd=0, lwd.ticks=1, tcl=5, line=5)
  axis(2, lwd=0, lwd.ticks=1)
  box(bty='l')
  mtext(text='Condition', side=2, line=2)
  
  #Add disturbance line/box i.e. mine de-water or undermining dates
  if(disturbance){
  	cat("\nSampling unit: ", samp_unit, "\n", sep='')
    cat("Enter total number of disturbances (both mine and water-discharge).  Enter 0 if none:\n")
    DisNumber<-scan(n=1, what=integer())
    # Check if DisNumber exisits
    if(DisNumber==0) {
    	disturbance = FALSE
    }
   }
  
  if(disturbance){
    if(DisNumber==1){
		cat("\n Enter a disturbance start date (yyyy-mm-dd):\n")
		Sdate<-scan(n=DisNumber, what=character())
		cat("\n Enter a disturbance end date (yyyy-mm-dd):\n")
		Edate<-scan(n=DisNumber, what=character())
    } 
    
    if(DisNumber>1) {
		cat("\n Enter ",DisNumber," disturbance start dates (yyyy-mm-dd):\n", sep='')
		Sdate<-scan(n=DisNumber, what=character())
		cat("\n Enter ",DisNumber," disturbance end dates (yyyy-mm-dd):\n", sep='')
		Edate<-scan(n=DisNumber, what=character())
    }
    
    dateS<-as.POSIXct(strptime(Sdate, '%Y-%m-%d'))
    dateE<-as.POSIXct(strptime(Edate, '%Y-%m-%d'))
    
    date=NULL
    for(a in c(1:DisNumber)) {
      date <- c(date, dateS[a], dateS[a], dateE[a], dateE[a], NA)
    }
    
    y<-rep(c(0,5,5,0,NA), times=DisNumber)
    polygon(date,y, col=rgb(0,0,0,0.2),border=NA, xpd=FALSE)
  }

  # Add data
  plt_spp <- as.character(unique(parsed_bb_sub$Species))
  for(a in 1:length(plt_spp)){
    #cat('a is equal to ',a,' and the species is ', plt_spp[a], '\n', sep='')
    plot_dat<-parsed_bb_sub[parsed_bb_sub$Species==plt_spp[a],]
    plot_dat_ord <- plot_dat[with(plot_dat, order(RDate)),]
    with(plot_dat_ord, lines(RDate, Cond,col=rgb(0,0,0,0.3)))
    #dfg <- scan() 
  }
  parsed_bb_subC_ord <- parsed_bb_subC[with(parsed_bb_subC, order(RDate)),]
  with(parsed_bb_subC_ord, lines(RDate, Cond, col='red', lwd=2))
  
  ## Plot Abundance
  # Setup
  with(parsed_bb_sub, plot(RDate, Abund, type='n', ylim=c(0,7.07), xlim=xlimits, yaxs='i', xaxs='i', axes=F, ann=F))
  with(seas_labs, axis(1, labels=Season, at=RDate, las=2, lwd=0, lwd.ticks=1))
  mtext(side=1, text=format(dat_lab_loc, '%Y'), at=dat_lab_loc, line=4)
  axis(1, at=dat_lab_ticks, labels=NA, lwd=0, lwd.ticks=1, tcl=5, line=5)
  axis(2, lwd=0, lwd.ticks=1)
  box(bty='l')
  mtext(text='Abundance', side=2, line=2)
  
  #for adding a disturbance to abundance plot
  if(disturbance){
    y<-rep(c(0,7,7,0,NA), times=DisNumber)
    polygon(date,y, col=rgb(0,0,0,0.2), border=NA, xpd=FALSE)
  }
    
  # Add data
  plt_spp <- as.character(unique(parsed_bb_sub$Species))
  for(a in 1:length(plt_spp)){
    #cat('a is equal to ',a,' and the species is ', plt_spp[a], '\n', sep='')
    plot_dat<-parsed_bb_sub[parsed_bb_sub$Species==plt_spp[a],]
    plot_dat_ord <- plot_dat[with(plot_dat, order(RDate)),]
    with(plot_dat_ord, lines(RDate, Abund,col=rgb(0,0,0,0.5))) 
  }
  parsed_bb_subC_ord <- parsed_bb_subC[with(parsed_bb_subC, order(RDate)),]
  with(parsed_bb_subC_ord, lines(RDate, Abund,col='red', lwd=2))
  if (plot_to=='png'){
    items_off <- dev.off() 
  }
}

# bb_plot_spp_rich v. 1.1
bb_plot_spp_rich <- function(parsed_bb, samp_unit=NA, samp_year=NA, plot_to='screen', verbose=TRUE){
  
  ## Check to see if data are parsed or not.
  # Parse if needed.
  if (!inherits(parsed_bb,"parsed.bb")) {
    if (verbose) {
      cat('\n')
      message("You provided raw data. It will be automatically parsed by bb_parse().  It is HIGHLY recomended that you run bb_parse() first with either the 'verbose' or 'log' functions enabled.")
    }
    parsed_bb_data <- bb_parse(parsed_bb, verbose=FALSE, log=FALSE)
    parsed_bb <- parsed_bb_data$parsed_bb
  }
  ##
  
  ## Check sampling unit
  if (any(is.na(samp_unit))) {
    samp_unit <- as.character(unique(parsed_bb$SampUnit))
  } else{
    if (!is.character(samp_unit)) {
      stop('Sampling unit (samp_unit) must be a charater string or a charater vector')
    }
    if (any((is.na(match(samp_unit, unique(parsed_bb$SampUnit)))))) { 
      if(length(samp_unit)>1){
        stop('Sampling units (samp_unit) were supplied but at least one is not actually in the data!')       
      } else {
        stop('Sampling unit (samp_unit) was supplied but is not actually in the data!')        
      }      
    }
  } 
  
  ## Check sampling year
  if (is.na(samp_year)) {
    samp_year <- as.character(unique(parsed_bb$SeasonYear))
    if (length(samp_year) != 1) {
      stop('Sampling year (samp_year) not supplied and a single unique sampling year could not be derived from the data.')      
    }  
  } else {
    if (!is.character(samp_year)) {
      stop('Sampling year (samp_year) must be a charater string of format "Month Year"')
    }
    if (length(samp_year) != 1) {
  	  stop('Sampling year (samp_year) for bb_plot_spp_rich() must be a single character string of format "Month Year".  See ?bb_plot_spp_rich for details.')
  	}
    if (is.na(match(samp_year, unique(parsed_bb$SeasonYear)))) { 
      stop('Sampling year (samp_year) was supplied but is not actually in the data!')
    }
  } 
  
  bb_spp_rich_year <- parsed_bb[parsed_bb$SeasonYear==samp_year,]
  which_bb_units<- which(bb_spp_rich_year$SampUnit %in% samp_unit)
  bb_spp_rich <- bb_spp_rich_year[which_bb_units,]
  
  site_rich_info<-tapply(bb_spp_rich$Species,as.character(bb_spp_rich$SampUnit), length_unique, verbose=FALSE)
  
  pos_doubles <- NULL
  site_riches <- NULL
  site_names <- NULL
  for(a in c(1:length(site_rich_info))) {
    pos_doubles <- c(pos_doubles, site_rich_info[[a]]$pos_double)
    site_riches <- c(site_riches, site_rich_info[[a]]$lenuni)
    site_names <- c(site_names, names(site_rich_info[a]))
  }
  
  site_rich <- matrix(site_riches, nrow=1)
  colnames(site_rich) <- site_names
  
  if (any(pos_doubles)) {
  	cat('\n')
    message('There were double-ups on species at the selected timepoint for one or more sites.  This makes it impossible to calculate species richness accurately.  The problem entries are:')
    which_doubles <- which(pos_doubles==TRUE)
    for (a in c(1:length(which_doubles))) {
      cat(names(site_rich_info[a]), ': ', paste(site_rich_info[[a]]$pos_double_names, collapse=', '), '.\n', sep='') 
    }
    cat('\n')
    stop('Impossible to calculate species richness accurately!\n')
  }
  
  ## Plot Setup  
  if (plot_to=='png'){
    fname = paste(format(Sys.Date(), '%Y-%m-%d'), '_', paste(samp_unit, collapse='_'), '_spp_rich.png', sep='')
    png(filename=fname, units='cm', height=(29.7/2)*1.3, width=5+(3.5*length(samp_unit)), res=72*2)
    layout(matrix(c(1), ncol=1))
    par(mai=c(1.0, 0.8, 0.1, 0.1)) 
  } else if (plot_to=='screen') {
    layout(matrix(c(1), ncol=1))
    par(mai=c(1.5, 0.8, 0.1, 0.1)) 
  } else {
    stop("The plot destination (plot_to) must be either 'png' or 'screen'")
  }
  ## -- ##
  
  barplot(site_rich, axes=F, ann=F, xpd=FALSE, ylim= c(0,max(site_rich)*1.5))
  axis(2, lwd=1, lwd.ticks=1, las=1)
  mtext(text='Species Richness', side=2, line=2.5)
  
  if (plot_to=='png'){
    items_off <- dev.off() 
  }
  
}