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
    first_derivs=rep(NA,times=length(dist))
    
    for(a in 1:(nrow(dca_scores)-(window-1))){
        b <- a+(window-1)
        axis_score <- dca_scores[a:b,axis_no]
        sub_dist <- dist[a:b]
        test_lm <- lm(axis_score ~ sub_dist)
        first_derivs[endcut+a] <- coefficients(test_lm)[2]
    }
    
    # Get 2nd Derivs
    second_derivs=rep(NA,times=length(dist))
    notna <- which(!is.na(first_derivs))
    
    for(a in min(notna):(max(notna)-(window-1))){
        b <- a+(window-1)
        sub_fd <- first_derivs[a:b]
        sub_dist <- dist[a:b]
        test_lm <- lm(sub_fd ~ sub_dist)
        second_derivs[endcut+a] <- coefficients(test_lm)[2]
    }
    
    # Find 2nd Deriv Change Points
    sddif <- diff(second_derivs)
    sddiflogi <- rep(NA,times=length(dist))
    # NB: Signed (+ or -) to flat (0) is still a change!
    for(a in which(!is.na(sddif))[-1]){
        if(sign(sddif[a])==sign(sddif[a-1])){
            sddiflogi[a] <- 0
        } else {
            sddiflogi[a] <- 1
        }
    }
    sddifzone <- rep(0,times=length(dist))
    sdmaxzone <- mean(second_derivs, na.rm=T)+se(second_derivs, na.rm=T)*1.96
    sdminzone <- mean(second_derivs, na.rm=T)-se(second_derivs, na.rm=T)*1.96
    sddifzone[which(second_derivs>sdmaxzone|second_derivs<sdminzone)] <- 1
    
    sd_chngpt <- sddiflogi*sddifzone
        
    # Get output sorted
    out_mat <- cbind(dist, dca_scores[,axis_no], first_derivs, second_derivs, sd_chngpt)
    dcalab <- paste0('dca', axis_no)
    dimnames(out_mat) <- list(NULL, c('distance', dcalab, 'first_derivative', 'second_derivative', 'sd_change_pt'))
    out_meta <- c(window,axis_no)
    names(out_meta) <- c('window', 'axis_no')
    out <- list('result' = out_mat, 'func_meta' = out_meta, 'veg' = veg, 'dist' = dist)
    class(out) <- c('ecofind', class(out))
    return(out)
}

plot.ecofind <- function(x, scaling=0.5, plot_info=TRUE, plot_se=TRUE, plot_cngpts=TRUE, spp_no=2, plot_lab=NA, ...){
    # Setup plotting data
    dcalab <- paste0('dca',x$func_meta['axis_no'])
    plotrd <- x$result[,dcalab]
    plotfd <- x$result[,'first_derivative']
    plotsd <- x$result[,'second_derivative']
    # Center and cale the first dir and the second dir
    centerscalefd <- as.numeric(scale(plotfd))
    centerscalesd <- as.numeric(scale(plotsd))
    scalefd <- centerscalefd/sd(plotrd, na.rm=T)*scaling
    scalesd <- centerscalesd/sd(plotrd, na.rm=T)*scaling
    
    ## Plot the species data
    layout(matrix(c(1,2), ncol=1), heights=c(1.75, 1))
    par(mar=c(0.3,4.1*3,1,0.3), oma=c(0,0,1,0))
    # Setup data
    veg01 <- x$veg
    veg01[veg01>0] <- 1
    usefulspp <- which(colSums(veg01)>spp_no)
    plotveg <- x$veg[,usefulspp]
    plotveg01 <- veg01[,usefulspp]
    plotveg[which(plotveg==0, arr.ind=T)] <- NA
    plotveg01[which(plotveg01==0, arr.ind=T)] <- NA
    ## TODO: Colour gradients
        
    # Order the data
    ## TODO, make this seperate function
    xx <- plotveg01
    for(a in nrow(xx):1) {
        xx <- xx[,order(xx[a,])]
    }
        
    # Plot the first plot
    plot(c(min(x$dist), max(x$dist)), c(-0.5,-ncol(plotveg)-0.5), ann=F, axes=F, type='n', yaxs='i')
        
    if(plot_cngpts){
        # Plot changepoints
        dists <- x$result[,'distance']
        abline(v=dists[which(x$result[,'sd_change_pt']==1)], lty=3)
    }
        
    for(a in 1:ncol(plotveg)) {
        ypos <- a*-1
        points(x$dist, xx[,a]*ypos, pch=20)
    }
        
    # Plot the main title, if set
    if(!is.na(plot_lab)){
        mtext(side=3, text=plot_lab, outer=T, line=0)
    }
    axis(2,at=seq(-1,-ncol(xx)), labels=colnames(xx), las=2, cex.axis=0.8)

    
    ## Plot the Derivitive data
    par(mar=c(3,4.1*3,0.3,0.3))

    plot(x$result[,'distance'], plotrd, type='l', axes=F, ann=F)
    points(x$result[,'distance'], plotrd, pch=20)
    lines(x$result[,'distance'], scalefd, col=2)
    lines(x$result[,'distance'], scalesd, col=3)
    box(bty='l'); axis(1, lwd=0, lwd.ticks=1); axis(2, lwd=0, lwd.ticks=1); mtext(text='Distance', side=1, line=2)
    if(plot_info){
        info_text <- paste0('Scaling = sd(',dcalab, ')*', scaling, '\nWindow = ', x$func_meta['window'])
        text(par('usr')[2]*-0.3, par('usr')[4]*0.75, info_text, cex=par('cex')*0.8, xpd=T, pos=1)
        legend(par('usr')[2]*-0.3, par('usr')[3], c(dcalab, '1st Deriv.', '2nd Deriv.'), lty=1, col=c(1,2,3), xjust=0.5, yjust=0, xpd=T, cex=par('cex')*0.8)
    }
    
    if(plot_se){
        abline(h=1.96*se(scalefd, na.rm=T),lty=2,col=2)
        abline(h=-1.96*se(scalefd, na.rm=T),lty=2,col=2)
        abline(h=1.96*se(scalesd, na.rm=T),lty=2,col=3)
        abline(h=-1.96*se(scalesd, na.rm=T),lty=2,col=3)
    }
    
    if(plot_cngpts){
        # Plot changepoints
        dists <- x$result[,'distance']
        abline(v=dists[which(x$result[,'sd_change_pt']==1)], lty=3)
    }
}