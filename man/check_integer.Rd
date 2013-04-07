\name{check_integer}
\alias{check_integer}

\title{Check if a number is an integer}
\description{Uses \code{grep} to test if the object contains only numbers.}

\usage{check_integer(n)}

\arguments{
  \item{n }{An object to test.}
}

\value{
	Returns a logical (TRUE or FALSE).
}

\author{
Originally from \url{http://stackoverflow.com/a/3937820}
}

\seealso{
\code{\link{grep}}, \code{\link{format}}.
}

\examples{
check_integer(1) # True
check_integer(1.2) # False
check_integer(1e10) # True
check_integer(1e-10) # False
check_integer('cheese') # Unsurprisingly: False
}