\name{length_unique}
\alias{length_unique}

\title{Length Unique}
\description{Returns the number of unique elements.  Reports if there are possible double-ups and what they might be.}

\usage{length_unique(x, verbose=TRUE)}

\arguments{
  \item{x }{A vector to test for uniqueness.}
  \item{verbose }{A logical.  Should a formal warning() be thrown.}
}

\details{
Counts the number of unique values in \code{x}.  If the number of unique items does not equal the overall number of items then a double-up is suspected.  If \code{verbose=TRUE} then a formal warning is thrown, otherwise it is up to the user to test \code{pos_double} manually.  
}

\value{
A named list containing:
	\item{lenuni }{The number of unique items in \code{x}}
	\item{pos_double }{A logical denoting if double-ups were detected.}
	\item{pos_double_names }{If \code{pos_double} is \code{TRUE}, a character string denoting the names of possible double-ups.  Otherwise \code{NA}}
}

\author{
Daniel Pritchard and Gretchen Brownstein
}

\examples{
dat <- c('Apples', 'Oranges', 'Cheese')
length_unique(dat)

dat_nonunique <- c('Apples', 'Oranges', 'Cheese', 'Apples')
length_unique(dat_nonunique)
}

\keyword{length}
\keyword{unique}
