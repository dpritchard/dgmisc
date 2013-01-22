\name{combn_sub}
\alias{combn_sub}

\title{Random subset of all possible combinations}
\description{This function returns a random subset of possible combinations defined by \code{combn(x, m)}}

\usage{combn_sub(x, m, sub=NA)}

\arguments{
  \item{x }{Vector source for combinations, or integer \code{n} for
    \code{x <- \link{seq_len}(n)}.}
  \item{m }{Number of elements to choose.}
  \item{sub }{An integer specifying the length of the subset to return.  Must be less than \code{choose(x,m)}}
}

\details{
The function will try to enumerate all possible combinations using \code{combn(x, m)}. If this fails a warning will be issued, but the function will then construct a matrix with \code{m} rows and \code{sub} columns. Each column contains a unique combination of \code{x}. This is the same format returned by \code{combn}. 

If \code{sub} is not supplied then it is set to \code{choose(x,m)}, though this will result in an error if the size of the matrix returned by \code{combn} is too large for R to handle.

If \code{sub == choose(x,m)} then the result will be identical to \code{combn(x, m)}
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{choose}}, \code{\link{combn}}.
}

\examples{
# Should be OK:
out <- combn_sub(41,2, 2)

# Will generate warnings, but should be OK:
out_warn <- combn_sub(41,20, 2)

# The following will be slow as all sin, 
# but it should return 5000 unique columns:
out_large <- combn_sub(41,20, 5000)
stopifnot(!any(duplicated(t(out_large))))

# Should return all 820 combinations:
out_820 <- combn_sub(41,2)
stopifnot(all(dim(out_820)==c(2,820)))

# Which should be equal to calling combin() directly:
stopifnot(all(out_820 == combn(41,2)))
}