# ECOLAB specific functions
print.ecolab <- function(pfs, misc=TRUE, sveq=FALSE, ...){
    sec <- names(pfs[['ECO_LAB_SETUP']])
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
        onesec <- names(pfs[[c('ECO_LAB_SETUP', sec[a])]])
        if(any(onesec=='PFS_DATA')){
            onesec <- onesec[-which(onesec=='PFS_DATA')]
        }
        prout[a,1] <- length(onesec)
    }
    print(prout, ...)
    
}

ecolab_get <- function(pfs, type=c('sv', 'c', 'f', 'av', 'p', 'do')){
    if(!inherits(pfs, 'ecolab')){
        stop('Only ECOLab objects are supported')
    }
    out <- NULL
    if(any(type %in% c('all', 'sv'))){
        svs <- pfs[[c('ECO_LAB_SETUP', 'STATE_VARIABLES')]]
        svs <- pfs_tryget(svs)
        sveq <- pfs[[c('ECO_LAB_SETUP', 'STATE_VARIABLE_EQUATIONS')]]
        sveq <- pfs_tryget(sveq, issveq=T)
        svs$Expression <- sveq$Expression[match(svs$Symbol, sveq$Symbol)]
        if(!is.null(svs)){svs$Type <- 'State Variable'}
        out <- rbind(out,svs)
    }
    if(any(type %in% c('all', 'c'))){
        cons <- pfs[[c('ECO_LAB_SETUP', 'CONSTANTS')]]
        cons <- pfs_tryget(cons)
        if(!is.null(cons)){cons$Type <- 'Constant'}
        out <- rbind(out,cons)
    }
    if(any(type %in% c('all', 'f'))){
        forc <- pfs[[c('ECO_LAB_SETUP', 'FORCINGS')]]
        forc <- pfs_tryget(forc)
        if(!is.null(forc)){forc$Type <- 'Forcing'}
        out <- rbind(out,forc)
    }
    if(any(type %in% c('all', 'av'))){
        avs <- pfs[[c('ECO_LAB_SETUP', 'AUXILIARY_VARIABLES')]]
        avs <- pfs_tryget(avs)
        if(!is.null(avs)){avs$Type <- 'Auxilary Variable'}
        out <- rbind(out,avs)
    }	
    if(any(type %in% c('all', 'p'))){
        procs <- pfs[[c('ECO_LAB_SETUP', 'PROCESSES')]]
        procs <- pfs_tryget(procs)
        if(!is.null(procs)){procs$Type <- 'Process'}
        out <- rbind(out,procs)
    }	
    if(any(type %in% c('all', 'do'))){
        dos <- pfs[[c('ECO_LAB_SETUP', 'DERIVED_OUTPUTS')]]
        dos <- pfs_tryget(dos)
        if(!is.null(dos)){dos$Type <- 'Derived Output'}
        out <- rbind(out,dos)
    }	
    return(out)
}

ecolab_tryget <- function(pfs, issveq=FALSE){
    if(!inherits(pfs, 'ecolab')){
        stop('Only ECOLab objects are supported')
    }
    objnms <- names(pfs)
    if(any(objnms=='PFS_DATA')){objnms <- objnms[-which(objnms=='PFS_DATA')]}
    if(length(objnms)==0){return(NULL)}
    pfssum <- NULL
    for(a in 1:length(objnms)){
        onepfsobj <- pfs[[c(objnms[a], 'PFS_DATA')]]
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

ecolab_write <- function(pfs, filename, elevate=TRUE){
    if(!inherits(pfs, 'ecolab')){
        stop('Only ECOLab objects are supported')
    }
    pfs <- ecolab_rename(pfs)
    ecolab_validate(pfs, elevate=elevate)
    
    fid <- file(filename, "w")
    pfs_write_section(pfs, fid)
    close(fid)
}

ecolab_sections <- function(){
    ss <- c('STATE_VARIABLES', 'CONSTANTS', 'FORCINGS',
            'AUXILIARY_VARIABLES', 'PROCESSES', 'DERIVED_OUTPUTS',
            'STATE_VARIABLE_EQUATIONS')
    ssvar <- c('STATE_VARIABLE', 'CONSTANT', 'FORCING',
               'AUXILIARY_VARIABLE', 'PROCESS', 'DERIVED_OUTPUT',
               'STATE_VARIABLE_EQUATION')
    out <- data.frame(ss, ssvar, stringsAsFactors=FALSE)
}

ecolab_rename <- function(pfs){
    if(!inherits(pfs, 'ecolab')){
        stop('Only ECOLab objects are supported')
    }
    snames <- names(pfs$ECO_LAB_SETUP)
    ss <- ecolab_sections()$ss
    ssvar <- ecolab_sections()$ssvar
    srename <- snames[which(snames %in% ss)]
    for(a in 1:length(srename)){
        sec <- pfs$ECO_LAB_SETUP[[srename[a]]]
        subname <- names(sec)
        wsec <- which(ss==srename[a])
        wnotpfsdat <- which(names(sec)!='PFS_DATA')
        if(length(wnotpfsdat)>0){
            subrename <- subname
            subrename[wnotpfsdat] <- paste0(ssvar[wsec],'_',c(1:length(wnotpfsdat)))
            names(sec) <- subrename
        }
        pfs$ECO_LAB_SETUP[[srename[a]]] <- sec
    }
    return(pfs)	
}

ecolab_join <- function(pfs1, pfs2, drop=FALSE, validate=FALSE){
    # Consider making use of modifyList() in the future?
    if(!inherits(pfs1, 'ecolab') | !inherits(pfs2, 'ecolab')){
        stop('Only ECOLab objects are supported')
    }
    # Merge key sections from pfs2 into pfs1
    ss <- ecolab_sections()$ss
    for(a in 1:length(ss)){
        base <- pfs1$ECO_LAB_SETUP[[ss[a]]]
        tomerge <- pfs2$ECO_LAB_SETUP[[ss[a]]]
        wnotpfsdat <- which(names(tomerge)!='PFS_DATA')
        if(length(wnotpfsdat)>0){
            tomerge <- tomerge[wnotpfsdat]
            # Optionally drop things from pfs2 that have an eqivilently named object in pfs1
            if(drop){
                basesyms <- ecolab_symbols(base)
                tomergesyms <- ecolab_symbols(tomerge)
                dups <- which(unlist(tomergesyms)%in%unlist(basesyms))
                if(length(dups>0)){
                    tomerge <- tomerge[-dups]
                }
            }
            # Do it!
            pfs1$ECO_LAB_SETUP[[ss[a]]] <- c(base, tomerge)
        }
    }
    # Validate and return
    if(validate){ecolab_validate(pfs1, elevate=FALSE)}
    return(pfs1)
}

ecolab_symbols <- function(pfs, hlevel=NULL){
    out <- NULL
    snames <- names(pfs)
    if(length(snames)==0){return(NULL)}
    for(a in 1:length(snames)){
        sname <- snames[a]
        #cat(sname, ':', is.na(sname), '\n')
        if(sname=='PFS_DATA'){
            ssnames <- names(pfs[[sname]])
            if('SYMBOL'%in% ssnames){
                out[hlevel] <- pfs[[sname]]$SYMBOL
                return(out)
            }
        } else {
            out <- c(out,ecolab_symbols(pfs[[sname]], hlevel=sname))
        }
    }
    return(out)
}

ecolab_ids <- function(pfs, hlevel=NULL){
    out <- NULL
    snames <- names(pfs)
    if(length(snames)==0){return(NULL)}
    for(a in 1:length(snames)){
        sname <- snames[a]
        if(sname=='PFS_DATA'){
            ssnames <- names(pfs[[sname]])
            if('ID'%in% ssnames){
                out <- pfs[[sname]]$ID
                return(out)
            }
        } else {
            out <- c(out,ecolab_ids(pfs[[sname]], hlevel=sname))
        }
    }
    return(out)
}

ecolab_validate <- function(pfs, elevate=FALSE){
    if(!inherits(pfs, 'ecolab')){
        stop('Only ECOLab objects are supported')
    }
    # Test symbol uniqueness
    syms <- ecolab_symbols(pfs)
    testsyms <- suppressWarnings(length_unique(unlist(syms)))
    if(testsyms$pos_double){
        message <- paste0('\nYou have double-ups in some of your symbol names.\nThis will cause problems for ECOLab.\nThe double-ups are:\n\n', paste0(testsyms$pos_double_names, collapse=', '), '\n')
        if(elevate){stop(message)} else {warning(message)}       
    }
    # Test sequential IDs
    #ids <- ecolab_ids(pfs)
    #testids <- !all(ids==seq(from=1, to=length(ids)))
    #if(testids){
    #    message <- paste0('\nItem IDs are not seqential.\nThis will cause problems for ECOLab.\n')
    #    if(elevate){stop(message)} else {warning(message)}       
    #}
    
    # Other tests?
}