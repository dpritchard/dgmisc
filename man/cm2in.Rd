\name{cm2in}
\alias{cm2in}

\title{Convert centimeters to inches} \description{A useful utility function when
plotting if, like most in the western world, you have no concept of 'inches'}

\usage{cm2in(x)}

\arguments{
  \item{x }{A numeric (as tested by \code{as.numeric}) value to be converted}
}

\details{
Uses an approximation of 1 centimeter = 2.54 inches.
}

\author{
Daniel Pritchard
}

\examples{
cm2in(1) 

cm2in(2.54) 

\dontrun{
# An A4 plot
pdf(file="cms_are_king.pdf", width=cm2in(21.0), height=cm2in(29.7))
plot(x=c(1,2,3), y=(3,2,1))
dev.off()
}
}