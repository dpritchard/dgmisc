\name{combn_sub}
\alias{combn_sub}

\title{Random subset of all possible combinations}
\description{This function returns a random subset (of n <= 5000) of possible combinations defined by \code{combn(n, m)}}

\usage{combn_sub(n, m, sub=NA)}

\arguments{
  \item{n }{An integer supplied to \link{combn}. See Details.}
  \item{m }{Number of elements to choose.}
  \item{sub }{An integer specifying the length of the subset to return.  Must be less than \code{choose(n,m)}, which must in turn be less than \code{5000}. See Details.}
}

\details{
	\code{n} defines the upper limit of a sequence of integers from which to select \code{x} items from. See \code{\link{combn}} for more information.  Note that supplying a vector of integers (which is supported by \code{combn} is not yet supported here.

	The number of returned combinations (\code{sub}) has been arbitrarily limited to 5000 items for performance reasons.  That being said, 5000 sub-samples from a population is usually large enough for most purposes. Contact the author if you disagree. If \code{sub} is not supplied then it is set to \code{choose(n,m)}, though this will result in an error if this number exceeds the 5000-item limit.
	
	If the number of possible combinations is less than 500000 then all possible combinations will be enumerated using \code{combn(n, m)} and a random subset will be selected from this.  Again, 500000 is an arbitrary limit based on performance considerations.
	
	If the number of possible combinations is greater than 500000 (i.e. more than 1000-times the maximum allowable number of returnable items) the function will construct a matrix with \code{sub} unique combination of \code{x}. This proceeds via a for-loop which could be slow for large values of \code{sub} (hence the limit of 5000). 
}

\value{
	A matrix with \code{sub} columns. Each column contains a unique combination of \code{x} of length \code{m}.  This is the same format returned by \link{combn}. If \code{sub == choose(n, m)} then the result should be identical to \code{combn(n, m)}.  
}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{choose}}, \code{\link{combn}}.
}

\examples{
# Simple (and quick) examples:
out_a <- combn_sub(41,2, 2)
out_b <- combn_sub(41,20, 2)

# The following will be quite quick because choose(41,37) < 500000.
# In this scenario, all possible combinations are generated 
#  and 5000 items are selected from the result. 
quite_fast <- combn_sub(41,37,5000)

# The following is very slow because choose(41,36) > 500000.
# In this scenario, a for loop is used to generate 5000 unique items.
quite_slow <- combn_sub(41,36,5000)
stopifnot(!any(duplicated(t(quite_slow))))

# Should return all 820 combinations:
out_820 <- combn_sub(41,2)
stopifnot(all(dim(out_820)==c(2,820)))

# Which should be equal to calling combin() directly:
stopifnot(all(out_820 == combn(41,2)))

# Should fail
\dontrun{
	
combn_sub(41,20, 5001)
combn_sub(41,20)
}
}