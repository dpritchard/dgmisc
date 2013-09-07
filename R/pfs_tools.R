require(stringr)
require(XML)

pfs_read <- function(filename=NULL){
	if(is.null(filename)){
		stop('Can be empty')
	}
	fid <- file(filename,'r');
	# Check fid here
	pfs <- pfs_readsec(fid)
	if(sum(str_count(names(pfs$MAIN), 'ECO_LAB_SETUP'))>0){
		class(pfs) <- c('ECOLAB', class(pfs))
	} else {
		class(pfs) <- c('pfs', class(pfs))
	}
	close(fid)
	return(pfs)
}

pfs_readsec <- function(fid, nline=0){
	pfsinfo <- list('MAIN' = list('PFS_DATA'=list()))
	while(length(oneline <- readLines(fid, n = 1, warn = FALSE))>0) {
		nline <- nline+1
		oneline <- str_trim(oneline)
		# If it's an empty line move on
		if(oneline==''){
			next
		}
		# According to the MATLAB function on which this is based, each non-empty line should have at least 3 characters.
		if(str_length(oneline)<3){
			stop(past('Bugger, error in format on line', nline))
		}
		# If it is the end of a section, if so retrun (we are done here)...
		if(!is.na(str_extract(oneline, '^EndSect'))){
			if(length(pfsinfo$MAIN$PFS_DATA)==0){
				pfsinfo$MAIN$PFS_DATA <- NULL
			}
			return(pfsinfo)
		}
		# If it is a comment, capture it in the current level...
		if(!is.na(str_extract(oneline, '^//'))){
			comment <- pfs_dequote(str_replace(oneline, '^//', ''))
			pfsinfo$MAIN$PFS_DATA$PFS_COMMENT <- c(pfsinfo$MAIN$PFS_DATA$PFS_COMMENT, comment)
			next
		}
		# Check if this is a new section...
		if(!is.na(str_extract(oneline, '^\\['))){
			sectionName <- str_match(oneline, '^\\[([A-Za-z1-9_]+)\\]')[1,2]
			pfsinfosec <- pfs_readsec(fid, nline)
			if(!is.null(pfsinfosec$MAIN$PFS_DATA$SYMBOL)){
				sectionName <- pfs_dequote(pfsinfosec$MAIN$PFS_DATA$SYMBOL)
			}
			names(pfsinfosec)[1] <- sectionName
			pfsinfo$MAIN <- c(pfsinfo$MAIN, pfsinfosec)
			next
		}
		
		# Try and read key / value pair
		keyval <- str_split_fixed(oneline, '=', n=2)
		key <- pfs_dequote(keyval[,1])
		val <- pfs_dequote(keyval[,2])
		keyval <- list()
		# Try to coerce to a numeric
		numval <- suppressWarnings(as.numeric(val))
		if(is.na(numval)){
			keyval[key] <- val
		} else {
			keyval[key] <- numval
		}
		#pfsinfo$MAIN <- c(pfsinfo$MAIN, keyval)
		pfsinfo$MAIN$PFS_DATA <- c(pfsinfo$MAIN$PFS_DATA, keyval)
	}
	return(pfsinfo)
}

pfs_dequote <- function(string){
	string <- str_replace_all(string, "['\"]", '')
	string <- str_trim(string)
	return(string)
}

print.ECOLAB <- function(pfsobj, misc=T, sveq=F, ...){
	sec <- names(pfsobj[[c('MAIN', 'ECO_LAB_SETUP')]])
	if(any(sec=='PFS_DATA')){
		sec <- sec[-which(sec=='PFS_DATA')]
	}
	if(!misc){
		sec <- sec[-which(sec=='MISC')]
	}
	if(!sveq){
		sec <- sec[-which(sec=='STATE_VARIABLE_EQUATIONS')]
	}
	prout <- matrix(nrow=length(sec), dimnames=list(sec, 'NumItems'))
	for(a in 1:length(sec)){
		onesec <- names(pfsobj[[c('MAIN', 'ECO_LAB_SETUP', sec[a])]])
		if(any(onesec=='PFS_DATA')){
			onesec <- onesec[-which(onesec=='PFS_DATA')]
		}
		prout[a,1] <- length(onesec)
	}
	print(prout, ...)
	
}

pfs_get <- function(pfsobj, type=c('sv', 'c', 'f', 'av', 'p', 'do')){
	out <- NULL
	if(any(type %in% c('all', 'sv'))){
		svs <- pfsobj[[c('MAIN', 'ECO_LAB_SETUP', 'STATE_VARIABLES')]]
		svs <- pfs_tryget(svs)
		sveq <- pfsobj[[c('MAIN', 'ECO_LAB_SETUP', 'STATE_VARIABLE_EQUATIONS')]]
		sveq <- pfs_tryget(sveq, issveq=T)
		svs$Expression <- sveq$Expression[match(svs$Symbol, sveq$Symbol)]
		if(!is.null(svs)){svs$Type <- 'State Variable'}
		out <- rbind(out,svs)
	}
	if(any(type %in% c('all', 'c'))){
		cons <- pfsobj[[c('MAIN', 'ECO_LAB_SETUP', 'CONSTANTS')]]
		cons <- pfs_tryget(cons)
		if(!is.null(cons)){cons$Type <- 'Constant'}
		out <- rbind(out,cons)
	}
	if(any(type %in% c('all', 'f'))){
		forc <- pfsobj[[c('MAIN', 'ECO_LAB_SETUP', 'FORCINGS')]]
		forc <- pfs_tryget(forc)
		if(!is.null(forc)){forc$Type <- 'Forcing'}
		out <- rbind(out,forc)
	}
	if(any(type %in% c('all', 'av'))){
		avs <- pfsobj[[c('MAIN', 'ECO_LAB_SETUP', 'AUXILIARY_VARIABLES')]]
		avs <- pfs_tryget(avs)
		if(!is.null(avs)){avs$Type <- 'Auxilary Variable'}
		out <- rbind(out,avs)
	}	
	if(any(type %in% c('all', 'p'))){
		procs <- pfsobj[[c('MAIN', 'ECO_LAB_SETUP', 'PROCESSES')]]
		procs <- pfs_tryget(procs)
		if(!is.null(procs)){procs$Type <- 'Process'}
		out <- rbind(out,procs)
	}	
	if(any(type %in% c('all', 'do'))){
		dos <- pfsobj[[c('MAIN', 'ECO_LAB_SETUP', 'DERIVED_OUTPUTS')]]
		dos <- pfs_tryget(dos)
		if(!is.null(dos)){dos$Type <- 'Derived Output'}
		out <- rbind(out,dos)
	}	
	
	
	return(out)
}

pfs_tryget <- function(pfsobj, issveq=F){
	objnms <- names(pfsobj)
	if(any(objnms=='PFS_DATA')){objnms <- objnms[-which(objnms=='PFS_DATA')]}
	if(length(objnms)==0){return(NULL)}
	pfssum <- NULL
	for(a in 1:length(objnms)){
		onepfsobj <- pfsobj[[c(objnms[a], 'PFS_DATA')]]
		pfssumtemp <- data.frame('Type'='-', stringsAsFactors=F)
		if(issveq){
			pfssumtemp$Symbol <- names(onepfsobj)
			pfssumtemp$Expression <- as.character(onepfsobj)
		} else {
			pfssumtemp$Symbol <- ifelse(is.null(onepfsobj$SYMBOL), '-', onepfsobj$SYMBOL)
			pfssumtemp$Default <- ifelse(is.null(onepfsobj$DEFAULT_VALUE), NA, onepfsobj$DEFAULT_VALUE)
			pfssumtemp$Min <- ifelse(is.null(onepfsobj$MIN_VALUE), NA, onepfsobj$MIN_VALUE)
			pfssumtemp$Max <- ifelse(is.null(onepfsobj$MAX_VALUE), NA, onepfsobj$MAX_VALUE)
			pfssumtemp$Expression <- ifelse(is.null(onepfsobj$EXPRESSION), '-', onepfsobj$EXPRESSION)
			pfssumtemp$Description <- ifelse(is.null(onepfsobj$DESCRIPTION), '-', onepfsobj$DESCRIPTION)
		}
		pfssum <- rbind(pfssum, pfssumtemp)
	}
	return(pfssum)
}

