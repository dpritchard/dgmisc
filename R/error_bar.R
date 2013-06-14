error_bar <- function(x, y, upper, lower=upper, length=0.05, dir='y',...){
    if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("Vectors must be same length")
    if(dir=='y'){
        arrows(x,y+upper,x,y-lower, angle=90, code=3, length=length, ...)
    } else if(dir=='x') {
        arrows(x-lower,y,x+upper,y, angle=90, code=3, length=length, ...)
    } else {
        stop('Impossible')
    }
}