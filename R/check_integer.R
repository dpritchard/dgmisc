# From http://stackoverflow.com/a/3937820
check_integer <- function(n){
  !length(grep("[^[:digit:]]", format(n, scientific = FALSE)))
}