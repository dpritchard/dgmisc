# v. 1.3
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
  if (is.na(samp_unit)) {
    samp_unit <- as.character(unique(parsed_bb$SampUnit))
    if (length(samp_unit) != 1) {
      stop('Sampling unit (samp_unit) not supplied and a single unique sampling unit could not be derived from the data.')      
    }	
  } else {
    if (!is.character(samp_unit)) {
      stop('Sampling unit (samp_unit) must be a charater string')
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
  seas_labs$RDate <- as.POSIXct(seas_labs$RDate, '%Y-%m-%d') 
  ## -- ##

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
  with(parsed_bb_sub, plot(RDate, Cond, type='n', ylim=c(0,5.05), yaxs='i', axes=F, ann=F))
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
  with(parsed_bb_sub, plot(RDate, Abund, type='n', ylim=c(0,7.07), yaxs='i', axes=F, ann=F))
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