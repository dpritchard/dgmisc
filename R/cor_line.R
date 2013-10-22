cor_line <- function(x,y,outlim=FALSE,...){
    r <- prcomp(cbind(x,y), retx = TRUE)
    # Gets slope of loading/eigenvector PC1
    b <- r$rotation[2,1] / r$rotation[1,1]
    # It is posisble that the denominator was zero
    # This happens if the slope is vertical. 
    # If so, we replace the Inf with a very large number (the largest one available!)
    if(b==Inf){b <- .Machine$double.eps} 
    a <- as.numeric(r$center[2] - b * r$center[1])
    newx <- seq(from=min(x), to=max(x), length.out=length(unique(x))*100)
    newy <- a + newx*b
    cuty <- which(newy<min(y) | newy>max(y))
    if(length(cuty>0)){
        newx <- newx[-cuty]
        newy <- newy[-cuty]
    }
    #print(a)
    #print(b)
    if(outlim){
        abline(a,b,lty=3)
        lines(newx,newy,...)
    } else {
        lines(newx,newy,...)
    }
}