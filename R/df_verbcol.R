# Return dataframe but with all columns (in col) verbatum quoted
df_verbcol <- function(df, cols, verb='`') {
    dfnames <- names(df)
    if(is.numeric(cols)){
        loopnames <- dfnames[cols]
    } else {
        loopnames <- dfnames[which(dfnames%in%cols)]
    }
    for(a in 1:length(loopnames)){
        df[,loopnames[a]] <- paste0(verb,df[,loopnames[a]],verb)
    }
    outdf <- data.frame(df[,loopnames], stringsAsFactors=F)
    names(outdf) <- loopnames
    return(outdf)
}