# pim_read v. 1.2

pim_read<-function(file, samp_year=NA, samp_season=NA){
    require(stringr)
    #get a field data
    datacore <- read.csv(file=file, skip=6, stringsAsFactors=F)
    #get field metadata
    datameta <- read.csv(file=file, nrows=6,stringsAsFactors=F)
    
    # First two columns need to be strings (should be species names!)
    if(!is.character(datacore[,1]) & !is.character(datacore[,2])) {
        stop(cat(file,': First two columns of data are not strings. They really need to be species names.', sep=''))
    }
    
    #dropping rowing with no species names
    if(any(datacore[,c(1,2)]=='')) {
        stop(cat(file,': There are blanks in the first two columns. May be missing species names?', sep=''))
    }
    
    nbr<-which(datacore[,1]!=''&datacore[,2]!='')
    br<-which(datacore[,1]==''&datacore[,2]=='')
    
    if(!length(nbr)==max(nbr)){
        stop(cat(file,": Blank rows. May be missing species names?", sep=''))
    }
    
    #checking that dropped rows didn't contain data
    if(!all(is.na(datacore[br,3:ncol(datacore)]))){
        stop(cat(file,": Data exist without a species name. Possibly in last row(s) of file.", sep=''))
    }
    
    data_nb <- datacore[nbr,]
    
    #finding the columns with no data
    data_spp <- data_nb[,3:ncol(data_nb)]
    nbc<-as.integer(which(colSums(!is.na(data_spp))>0))
    bc<- as.integer(which(colSums(!is.na(data_spp))==0))
    
    if(!length(nbc)==max(nbc)){
        stop(cat(file,": Blank columns in the middle of dataset, may be missing data.", sep=''))
    }
    
    #index out only non-blank rows and columns
    dat <- cbind(data_nb[,1:2], data_spp[,nbc])
    
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
    
    # Testing for presences of valid 'Transect' info
    if(!str_detect(tolower(str_trim(datameta[4,1])), '^trans')){
        stop(cat(file,": Metadata missing header 'Transect'."))
    }
    
    if(str_trim(datameta[4,2])==''){
        stop(cat(file,": Metadata missing transect number."))
    }
    
    if(is.na(as.integer(str_trim(datameta[4,2])))){
        stop(cat(file,": Metadata transect number can not be coerced to an integer."))
    }
    
    #Are there equal number of S and C columns and if so what are the number of steps
    steps<-length(dat[,3:ncol(dat)])/2
    if(check_integer(steps)!=TRUE){
        stop(cat(file,": The data contain an unequal number of stratum and condition columns."))
    }
    
    ideal_steps <- seq(from=1,to=steps, by=1)
    real_steps <- na.omit(as.integer(str_trim(as.character(datameta[5,3:ncol(dat)]))))
    
    if(length(ideal_steps)!=length(real_steps)) {
        stop(cat(file,": Steps are not correctly labelled (possibly missing labels?)."))
    }
    
    if(!all(ideal_steps==real_steps)) {
        stop(cat(file,": Steps are not correctly labelled (possibly not numbered sequentially?)."))
    }
    
    ideal_cs <- rep(c('s','c'), times=steps)
    real_cs <- str_trim(tolower(as.character(datameta[6,3:ncol(datameta)])))
    
    if(!all(real_cs[1:(steps*2)]==ideal_cs)) {
        stop(cat(file,": Some 'c' or 's' columns are labelled incorrectly, or missing labels entierly."))
    }
    
    ## All datachecks done. Now construct output
    out = NULL
    datout = NULL
    metadatout = NULL
    
    outsamp_year <- samp_year
    outsamp_season <- samp_season
    outmeth <- 'PIM'
    outmsu <- str_trim(datameta[1,2])
    outmdate <- str_trim(datameta[2,2])
    outmass <- str_trim(datameta[3,2])
    outmtrans <- as.integer(str_trim(datameta[4,2]))
    
    metadataout <- data.frame('Year' = outsamp_year, 'Season' = outsamp_season,'Method Code' = outmeth, 'Sampling Unit ID' = outmsu, 'Start Date (YYYY/MM/DD)' = outmdate, 'Assessor'= outmass, 'Assessors Transect Number' = outmtrans, stringsAsFactors=F)
    
    outnrow <- steps*nrow(dat)
    outsamp_year <- rep(samp_year, times=outnrow)
    outsamp_season <-rep(samp_season, times=outnrow)
    outmeth <- rep('PIM', times=outnrow)
    outsu <- rep(outmsu, times=outnrow)
    outdate <- rep(outmdate, times=outnrow)
    outass <- rep(outmass, times=outnrow)
    outtrans <- rep(outmtrans, times=outnrow)
    
    outsteps <- rep(real_steps, each=nrow(dat))
    outoldspp <- rep(dat[,1], times=steps)
    outnewspp <- rep(dat[,2], times=steps)
    scols <- seq(from=3, to=steps*2+2, by=2)
    ccols <- seq(from=4, to=steps*2+2, by=2)
    
    outs <- unlist(dat[,scols])
    outc <- unlist(dat[,ccols])
    
    datout <- data.frame('Year' = outsamp_year, 'Season' = outsamp_season, 'Method Code' = outmeth, 'Start Date (YYYY/MM/DD)' = outdate, 'Sampling Unit ID' = outsu, 'Plot Transect Number' = outtrans, 'Assessors' = outass,  'Assessors Transect Number' = outtrans, 'Step' = outsteps, 'Stratum' = outs, 'Field Name' = outoldspp, 'Scientific Name' = outnewspp,  'Condition Score (1-5)' = outc, stringsAsFactors=F)
    
    out <- list('metadata' = metadataout, 'data' = datout)
    
    return(out)
}

#pim_parse
pim_parse<- function(raw_pim){
    
    req_cols<-c('Year', 'Season', 'Start.Date..YYYY.MM.DD.', 'Sampling.Unit.ID', 'Plot.Transect.Number', 'Step', 'Stratum','Scientific.Name','Condition.Score..1.5.')
    
    #Check all required columns are present
    for (a in 1:length(req_cols)) {
        if (is.na(match(req_cols[a], names(raw_pim)))) {
            stop(paste("The supplied data does not have a column named '", req_cols[a], "'.", sep=''))
        }
    }
    
    # Subset to only keep required columns
    raw_pim_sub <- raw_pim[,req_cols]
    
    # Sorting out column names
    names(raw_pim_sub)[c(3,4,5,6,7,8,9)]<-c('Date','SampUnit', 'Transect','Step','Strata','ScientificName','Condition')
    
    # The following line is a manual re-code of season order, check order using 'levels'
    if (length(levels(raw_pim_sub$Season))>4) {
        stop("You have more than four seasons? I don't think so. Check your data and try again.") 
    }
    raw_pim_sub$Season<-factor(raw_pim_sub$Season, levels(raw_pim_sub$Season)[c(3,1,4,2)])
    
    
    #making new id column to subset data by:
    raw_pim_sub$seasonyearsu<-paste(raw_pim_sub$Season, raw_pim_sub$Year, raw_pim_sub$SampUnit)
    
    #checking that each strata score has a condition score
    raw_pim_sub$HasNonzeroCond <- raw_pim_sub$Condition>=0
    raw_pim_sub$HasNonzeroCond[is.na(raw_pim_sub$HasNonzeroCond)] <- FALSE
    raw_pim_sub$HasNonzeroStrat <- raw_pim_sub$Strata>0
    raw_pim_sub$HasNonzeroStrat[is.na(raw_pim_sub$HasNonzeroStrat)] <- FALSE
    
    
    if(all(raw_pim_sub$HasNonzeroStrat==raw_pim_sub$HasNonzeroCond)!=TRUE){
        stop ("You have missing strata or condition scores. Check your data and try again.") 
    }
    
    lencon<-sum(raw_pim_sub$HasNonzeroCond)
    lenstrat<-sum(raw_pim_sub$HasNonzeroStrat)
    
    if(lencon!=lenstrat){
        stop ("You have missing strata or condition scores. Check your data and try again.") 
    }
    
    len_Cond_too_high <- sum(raw_pim_sub$Condition>5, na.rm=T)
    if(len_Cond_too_high!=0){
        stop ("You have condition scores greater than 5, this cannot be. Check your data and try again.") 
    }
    
    num_strat_dodgy<-sum(raw_pim_sub$Strata>12, na.rm=T)
    if(num_strat_dodgy!=0){
        cat('\n')
        cat('You have one or more strata greater than 12, this seems odd... maybe you should check the data?\n')
    }
    class(raw_pim_sub) <- c('parsed.pim', class(raw_pim_sub))
    parsed_pim <- raw_pim_sub 
    
    return(parsed_pim)
}

#pim_sum
pim_sum<-function(subunit){
    # Check for the right class
    if (!inherits(subunit,"parsed.pim")) {
        stop('The input must be parsed data returned by pim_parse()')
        }
    
    # Check for one site. Issue snippy if needed.
    if (length_unique(subunit$SampUnit, verbose=FALSE) > 1) {
        warning('There is more than one site (Sampling Unit) in the data... Are you sure about this?')
    }
    
    ## Get summary stats for frequency using the strata:
    # Melt data (using reshape package)
    subM<-melt(subunit[,c('Transect','Step','Strata','ScientificName')], id=c('ScientificName','Step','Transect', 'Strata'))
    
    #replace na's and numbers with 0 and 1
    NAs <- which(is.na(subM$Strata))
    subM$Strata[NAs] <- 0
    nonzero <- which(subM$Strata>0)
    subM$Strata[nonzero] <- 1
    
    #summarise the number of hits per species:
    pres_abs <- cast(subM, formula= ScientificName ~., value='Strata', fun.aggregate=sum)
    freq_out <- pres_abs[c(2:length(pres_abs))]
    
    #to get the total number of steps per site:old way
    #steps <- cast(subM, formula=Transect + Step ~ ScientificName, value= 'Strata', fun=sum)
    #total_steps<-length(steps$Transect)
    #the new way:
    steps=NULL
    for(a in unique(subunit$Transect)){
        transsub<-subunit[subunit$Transect==a,]
        #transsub<-subset(subunit, Transect==a) # This is bad practice, see ?subset
        y<-max(transsub$Step)
        steps<-rbind(steps, y)
    }  
    total_steps<-colSums(steps)
    
    #now we calculate frequency and output it:
    frequency <-data.frame('Frequency'=(freq_out/total_steps)*100)
    names(frequency) <- 'Frequency'
    
    #to get the mean condition for each species 
    subC<-melt(subunit[,c('Transect','Step','Condition','ScientificName')], id=c('ScientificName','Step','Transect', 'Condition'), na.rm=FALSE)
    median_cond<- cast(subC, formula= ScientificName ~., value='Condition', fun.aggregate=median, na.rm=TRUE)
    condition <-as.data.frame(median_cond[[2]])
    names(condition) <- 'Median_Cond'
    
    if(length(condition)!=length(frequency)){
        stop('you have issues with the summary stats, check data')
    }
    
    if(all((as.character(pres_abs[[1]]))!=(as.character(median_cond[[1]])))){
        stop('you have issues with the summary stats, check data')
    }
    
    
    outdata=NULL
    
    outfreq <- frequency #this is probably not needed
    outcond <- condition #this is probably not needed
    outspp <- (as.character(pres_abs[[1]]))
    
    outseason <- str_trim(subunit[1,'Season'])
    outyear <- str_trim(subunit[1,"Year"])
    outmsamp <- str_trim(subunit[1,"SampUnit"])
    outmtran <- str_trim(subunit[1,"Transect"])
    outmdate <- str_trim(subunit[1,"Date"])
    
    outnrow <- nrow(frequency)
    outsea<- rep(outseason, times=outnrow)
    outyr<-rep(outyear, times=outnrow)
    outsu <- rep(outmsamp, times=outnrow)
    outtran <- rep(outmtran, times=outnrow)
    outdate <- rep(outmdate, times=outnrow)
    
    outdata<-data.frame('Season'=outsea, 'Year'=outyr,'SamplingUnit'=outsu, 'Transect' = outtran, 'Date'= outdate, 'ScientificName'=outspp, 'Frequency'=outfreq, 'MedianCondition'=outcond, stringsAsFactors=F)
    
    return(outdata)   
}
