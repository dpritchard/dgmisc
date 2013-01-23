\name{size}
\alias{size}

\title{Size}
\description{Replicates some of the functionality of MATLAB's \code{size} function.}

\usage{size(x, dim=NA)}

\arguments{
  \item{x }{An \R object accepted by \code{\link{dim}}, or a vector}
  \item{dim }{An integer specifying the size of the dimension to return.}
}

\value{
If \code{x} is anything other than a vector the result from \code{size} will be identical to \code{dim} (i.e. a vector of mode \link{integer}). 

If \code{is.vector(x) == TRUE} then \code{size} will return a vector equal to \code{c(1,length(x))} (i.e. 1 row and n columns).
}

\note{
\code{is.vector} returns FALSE if \code{x} is a factor.  This seems a little odd, but for now, if you provide a factor then \code{size} will return NULL with a warning.  
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{dim}}, \code{\link{is.vector}}.
}

\examples{
dat_a <- c(1,2,3,4,5)
dat_b <- matrix(c(1,2,3,4,5,6), nrow=2)

size(2)			# 1 1
size(dat_a) 	# 1 5
size(dat_b)		# 2 3
size(as.factor(dat_a)) # NULL, with a warning

stopifnot(all(size(dat_b)==dim(dat_b)))

# Just the number of columns
size(dat_b, dim=2)
# Which is the same as...
size(dat_b)[2]

}
