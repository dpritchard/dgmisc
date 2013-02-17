\name{fr_fit}
\alias{fr_fit}

\title{Fit predator-prey functional responses.}
\description{DESCRIPTION}

\usage{fr_fit(formula, data, response, start=list(), fixed=list(), boot=FALSE, nboot=999, para=TRUE)}

\arguments{
  \item{formula }{DESC.}
  \item{data }{DESC.}
  \item{response }{DESC.}
  \item{start }{DESC.}
  \item{fixed }{DESC.}
  \item{boot }{DESC.}
  \item{nboot }{DESC.}
  \item{para }{DESC.}
}

\details{
TODO.   
}

\seealso{
\code{\link{fr_functions}}.
}

\author{
Daniel Pritchard
}

\examples{
# TODO
# dat = read.csv('../dev_data/BootStrapData/Simulid.csv')
# out <- fr_fit(eaten~density, data=dat, response=rogersII, start=list(a = 1.2, h = 0.015), fixed=list(P=1, T=1), boot=T)
}
