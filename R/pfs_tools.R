# Tools for PFS files and ecolab files
# TODO:
# renumber IDs in ecolab files?
pfs_read <- function(filename=NULL){
	if(is.null(filename)){
		stop('Can be empty')
	}
	fid <- file(filename,'r');
	# Check fid here
	pfs <- pfs_readsec(fid)
	if(sum(str_count(names(pfs), 'ECO_LAB_SETUP'))>0){
		class(pfs) <- c('ecolab', class(pfs))
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
	return(pfsinfo[[1]])
}

pfs_dequote <- function(string){
	string <- str_replace_all(string, "['\"]", '')
	string <- str_trim(string)
	return(string)
}

print.pfs <- function(pfsobj, ...){
    cat('MIKE by DHI PFS Object\n')
    cat('Be aware that although this function works with many PFS-files,\n')
    cat('only ECOLab files are currently supported\n')
}

pfs_write_section <- function(pfs, fid, indent=0, hlevel=NULL) {
    indent_str <- pfs_indent_str(indent)
    if(is.null(hlevel)){hlevel <- ''}
    sec_names_all <- names(pfs)
    for(a in 1:length(sec_names_all)){
        sec_name <- sec_names_all[a]
        if(sec_name=='PFS_DATA'){
            # If there is a PFS data section, we need to do something slightly different.
            pfs_write_comment(pfs[[sec_name]], fid, indent, hlevel)
            pfs_write_keyvals(pfs[[sec_name]], fid, indent, hlevel) 
        } else {
            cat('\n', indent_str, '[', sec_name, ']\n', sep='', file=fid)
            pfs_write_section(pfs[[sec_name]], fid=fid, indent=indent+1, hlevel=sec_name)
            cat(indent_str, 'EndSect  // ', sec_name, '\n', sep='', file=fid)
        }
    }
}

pfs_write_comment <- function(pfs, fid, indent, hlevel){
    indent_str <- pfs_indent_str(indent)
    if(!is.null(pfs$PFS_COMMENT)){
        for(a in 1:length(pfs$PFS_COMMENT)){
            cat(indent_str, '// ', pfs$PFS_COMMENT[a], '\n', sep='', file=fid)
        }    
    }
}

pfs_write_keyvals <- function(pfs, fid, indent, hlevel){
    indent_str <- pfs_indent_str(indent)
    dat_names <- names(pfs)
    if(!is.null(pfs$PFS_COMMENT)){
        which_comm <- which(dat_names=='PFS_COMMENT')
        dat_names <- dat_names[-which_comm]
    }
    if(length(dat_names)>0){
        for(a in 1:length(dat_names)){
            single_dat <- pfs[[dat_names[a]]]
            format_val <- pfs_format_val(dat_names[a], single_dat, hlevel)
            cat(indent_str, dat_names[a], ' = ', format_val, '\n', sep='', file=fid)
        }	
    }
}

pfs_indent_str <- function(indent){
    if(indent>0){
        indent_str <- rep('   ', times=indent)
    } else {
        indent_str = ''
    }
    return(indent_str)
}

pfs_format_val <- function(key, val, hlevel){
# PFS FORMAT... 
# At the moment this too specific to ECOLAB files
# Also, need to deal with comma separted lists in PFS files (read as vectors? write in correct format)
# Also pipe "|" delimited file names in PFS files need help
    if(toupper(key)=='EXPRESSION'){
        return(str_join("\'\"", as.character(val), "\"\'"))
    } else if(toupper(key)=='CHECKSUM' && hlevel=='MISC'){
        return(str_join("\'", as.character(val), "\'")) # Quote the 'CHECKSUM'
    } else if(str_detect(string=hlevel, pattern='STATE_VARIABLE_EQUATION_')){
        return(str_join("\'\"", as.character(val), "\"\'"))
    } else if(is.character(val)){
        return(str_join("\'", val, "\'"))
    } else {
        return(val)
    }
}