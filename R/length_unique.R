length_unique <- function(x, verbose=TRUE){
    uni <- unique(x)
    len <- length(x)
    pos_double <- FALSE
    pos_double_names <- NA
    if(length(uni) != len){
      if (verbose) {
         warning('Possible double up.')
      }
      pos_double_xtab <- xtabs(~x)
      pos_double_names <- names(which(pos_double_xtab>1))
      pos_double <-TRUE
    }  
    lenuni <- length(uni)
    out<-list('lenuni'= lenuni, 'pos_double'= pos_double, 'pos_double_names'= pos_double_names)
    return(out)
  }
