ecofind <- function(veg, dist, window=5, axis_no=1){
    # Check that window is odd numbered
    if(check_integer(window/2)){
        stop('Windows needs to be odd-numbered (for now)')
    }
    # Check that nrow(veg) and length(dist are equal)
    if(nrow(veg)!=length(dist)){
        stop('dist must be the same length as nrow(veg)')
    }
    
    dca <- decorana(veg)
    # This gets sites scores, with origin=T
    # To access unscaled site scores we can use origin=F
    # It isn't entierly clear what 'origin' does, but it scales by some constant for each axis.  
    dca_scores <- scores(dca, display='sites', origin=TRUE)
    
    # Here I am trying to select 5 rows (starting at row 1-5, then 2-6, then 3-7 etc), then do a linear regression then collect up all the slopes.
    endcut <- floor(window/2)
    
    # Get 1st Derivs
    first_derivs=matrix(nrow=length(dist))
    
    for(a in 1:(nrow(dca_scores)-(window-1))){
        b <- a+(window-1)
        axis_score <- dca_scores[a:b,axis_no]
        sub_dist <- dist[a:b]
        test_lm <- lm(axis_score ~ sub_dist)
        first_derivs[endcut+a] <- coefficients(test_lm)[2]
    }
    
    # Get 2nd Derivs
    second_derivs=matrix(nrow=length(dist))
    notna <- which(!is.na(first_derivs))
    
    for(a in min(notna):(max(notna)-(window-1))){
        b <- a+(window-1)
        sub_fd <- first_derivs[a:b]
        sub_dist <- dist[a:b]
        test_lm <- lm(sub_fd ~ sub_dist)
        second_derivs[endcut+a] <- coefficients(test_lm)[2]
    }
    
    out_mat <- cbind(dist,dca_scores[,axis_no],first_derivs,second_derivs)
    dcalab <- paste0('dca',axis_no)
    dimnames(out_mat) <- list(NULL, c('distance', dcalab, 'first_derivative', 'second_derivative'))
    out_meta <- c(window,axis_no)
    names(out_meta) <- c('window', 'axis_no')
    out <- list('result'=out_mat, 'func_meta'=out_meta)
    class(out) <- c('ecofind', class(out))
    return(out)
}


plot.ecofind <- function(x, scaling=0.5, plot_info=TRUE, plot_se=TRUE, ...){
    dcalab <- paste0('dca',x$func_meta['axis_no'])
    plotrd <- x$result[,dcalab]
    plotfd <- x$result[,'first_derivative']
    plotsd <- x$result[,'second_derivative']
    # Scale the first dir and the second dir
    
    centerscalefd <- as.numeric(scale(plotfd))
    centerscalesd <- as.numeric(scale(plotsd))
    
    scalefd <- centerscalefd/sd(plotrd, na.rm=T)*scaling
    scalesd <- centerscalesd/sd(plotrd, na.rm=T)*scaling
    
    plot(x$result[,'distance'], plotrd, type='l', ...)
    points(x$result[,'distance'], plotrd, pch=20)
    lines(x$result[,'distance'], scalefd, col=2)
    lines(x$result[,'distance'], scalesd, col=3)
    if(plot_info){
        info_text <- paste0('Scaling = sd(',dcalab, ')*', scaling, '\nWindow = ', x$func_meta['window'])
        text(par('usr')[1], par('usr')[4]*1.2, info_text, cex=par('cex')*0.8, xpd=T, pos=4)
        legend(mean(par('usr')[2]), par('usr')[4]*1.2, c(dcalab, '1st Deriv.', '2nd Deriv.'), lty=1, col=c(1,2,3), xjust=1, yjust=0.5, xpd=T, horiz=T, cex=par('cex')*0.8)
    }
    if(plot_se){
        abline(h=1.96*se(scalefd, na.rm=T),lty=2,col=2)
        abline(h=-1.96*se(scalefd, na.rm=T),lty=2,col=2)
        abline(h=1.96*se(scalesd, na.rm=T),lty=2,col=3)
        abline(h=-1.96*se(scalesd, na.rm=T),lty=2,col=3)
    }
}