# v. 1.1
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