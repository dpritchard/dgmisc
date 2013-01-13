\name{length_unique}
\alias{length_unique}

\title{Length Unique}
\description{Returns the number of unique elements.  Reports if there are possible double-ups and what they might be.}

\usage{length_unique(x)}

\arguments{
  \item{x }{A vector to test for uniqueness.}
}
\details{
Counts the number of unique values in \code{x}.  If the number of unique items does not equal the overall number of items then a double-up is suspected and a warning is thrown.  
}
\value{
A name list containing:
	\item{lenuni }{The number of unique items in \code{x}}
	\item{pos.double }{A logical denoting if double-ups were detected.}
	\item{pos.double.names }{If \code{pos.double} is \code{TRUE}, a character string denoting the names of possible double-ups.  Otherwise \code{NA}}
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
