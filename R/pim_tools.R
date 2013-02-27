# pim_read v. 1.1

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
  
  metadataout <- data.frame('Year' = outsamp_year, 'Season' = outsamp_season, 'MethodCode' = outmeth, 'SamplingUnit' = outmsu, 'Date' = outmdate, 'Assessor'= outmass, 'TransectNo' = outmtrans, stringsAsFactors=F)
  
  outnrow <- steps*nrow(dat)
  outsamp_year <- rep(samp_year, times=outnrow)
  outsamp_season <-rep(samp_season, times=outnrow)
  outmeth <- rep('PIM', times=outnrow)
  outsu <- rep(outmsu, times=outnrow)
  outdate <- rep(outmdate, times=outnrow)
  outass <- rep(outmass, times=outnrow)
  outtrans <- rep(outmtrans, times=outnrow)
  
  outsteps <- rep(real_steps, times=nrow(dat))
  outoldspp <- rep(dat[,1], each=steps)
  outnewspp <- rep(dat[,2], each=steps)
  scols <- seq(from=3, to=steps*2+2, by=2)
  ccols <- seq(from=4, to=steps*2+2, by=2)
  
  outs <- unlist(dat[,scols])
  outc <- unlist(dat[,ccols])
  
  datout <- data.frame('Year' = outsamp_year, 'Season' = outsamp_season, 'MethodCode' = outmeth, 'Date' = outdate, 'SamplingUnit' = outsu, 'PlotTransect' = outtrans, 'Assessor' = outass,  'Transect' = outtrans, 'Step' = outsteps, 'Strata' = outs, 'FieldName' = outoldspp, 'ScientificName' = outnewspp,  'Condition' = outc, stringsAsFactors=F)
  
  out <- list('metadata' = metadataout, 'data' = datout)
  
  return(out)
}