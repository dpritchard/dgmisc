length_unique <-
function(x){
    uni <- unique(x)
    len <- length(x)
    pos.double <- FALSE
    pos.double.names <- NA
    if(length(uni) != len){
      warning('Possible double up')
      pos.double.xtab <- xtabs(~x)
      pos.double.names <- names(which(pos.double.xtab>1))
      pos.double <-TRUE
    }  
    lenuni <- length(uni)
    out<-list('lenuni'= lenuni, 'pos.double'= pos.double, 'pos.double.names'= pos.double.names)
    return(out)
  }
