\name{cor_line}
\alias{cor_line}

\title{Draws a correlation line}
\description{Draws a correlation line, defined eigenvector of the first principle compoenent.}
\usage{
cor_line(x, y, outlim=FALSE, ...)
}

\arguments{
  \item{x }{The x variable}
  \item{y }{The y variable}
  \item{outlim }{Should the line be draw outside the limits of the data?}
  \item{... }{Other variables passed to \code{\link{lines}} and \code{\link{segments}}}
}

\details{
This draws a 'correlation line' on a plot of data. A correlation line is distinct from a line drawn from a regression output (e.g. that produced by \code{\link{lm}}) in that a correlation minimises residuals orthagonnaly to the line, not parallel with the y-axis (as a regression does). This is one of the main reasons why 'correlation doe snot equal causation'.  There aren't a great number of good reasons to draw a correlation lin, but it is useful for teaching / demonstration.  
If \code{outlim} is FALSE (the default) only a line that fits inside the convex hull of x and y is drawn. If \code{outlim} is true, a dshed line (\code{lty=3}) is drawn first, and then the a line within the convex hull is drawn over this.
}

\value{
Draws a line on the exisiting plot object using a call to \code{\link{lines}}
}
\author{
Daniel Pritchard
}

\examples{
x <- c(-1.03, 0.79, -0.67, -0.1, 0.05, -0.76, 1.91, -0.52, -0.12, 
0.07, 0.1, -1.92, 0.3, 1.42, -0.25, -0.23, -0.26, 1.19, 0.25, 
-0.07, -1.55, 0.38, 1.86, 0.74, -0.48, 1.37, -0.73, 0.11, 0.5, 
-2.32)
y <- c(0.59, 2.29, 0.36, -0.58, -0.86, 1.12, 0.09, -0.15, -1.47, 0.69, 
-0.77, 1.23, -0.47, -0.17, -0.19, -0.84, -0.34, -0.47, 0.98, 
-0.46, -0.84, 0.18, 0.59, -1.21, -0.83, 2.8, 0.71, 0.02, -1.18, 
-0.82)

layout(matrix(c(1,2),nrow=1))
par(mai=c(0.8,0.8,0.4,0.4))
layout(matrix(c(1,2),nrow=1))

plot(x, y, main = "Regression", xlim=c(-2.5,2.5), ylim=c(-2,3), xaxs="i", yaxs="i")
yxlm <- lm(y ~ x)
lines(x, predict(yxlm), col = "red", lwd = 2)
segments(x, y, x, fitted(yxlm), col = "red")

plot(x, y, main = "Correlation", xlim=c(-2.5,2.5), ylim=c(-2,3), xaxs="i", yaxs="i")
corfit <- cor_line(x, y, outlim=TRUE, col='blue')
}
