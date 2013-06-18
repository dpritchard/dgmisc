\name{ecofind}
\alias{ecofind}
\alias{plot.ecofind}

\title{Find ecotone locations and widths}
\description{Finds ecotones based on species composition via DCA}
\usage{
ecofind(veg, dist, window=5, axis_no=1)

\method{plot}{ecofind}(x, scaling=0.5, plot_info=TRUE, 
    plot_se=TRUE, plot_cngpts=TRUE, spp_no=2, 
    plot_lab=NA, \ldots)
}

\arguments{
  \item{veg }{A matrix of community data. Must be compatible with \code{\link{decorana}}}
  \item{dist }{A vector. Quadrat distances along transect. Must have \code{nrow(veg)} values.}
  \item{window }{An odd integer. Defines the width of the moving window.}
  \item{axis_no }{An integer. Which DCA axis to use for derivative calculations?}
  \item{x }{An object of class ecofind.}
  \item{scaling }{Arbitary scaling factor for plotting 1st and 2nd derivatives. See Details.}
  \item{plot_info }{A logical. Should the legend and other info be plotted?}
  \item{plot_se }{A logical. Should the 1.96*se lines be drawn?}
  \item{plot_cngpts }{A logical. Should the 2nd derivitive changepoints be drawn?}
  \item{spp_no }{An integer. Species with occurances <= to this are excluded from the plot.}
  \item{plot_lab }{A string. If present it is printed at the top of the figure.}
  \item{\ldots }{Other arguments. Not used}
}
\details{
This function impliments the ecotone-finding methods of Walker \emph{et al.} (2003), as used by Brownstein \emph{et al.} (2013).  In essence it first performs a detrended correspondance analysis on the community matrix in \code{veg} and then caclulates the 1st and 2nd derivitives of the DCA axis defined by \code{axis_no} versus distance along the transect (provided by \code{dist}).  The \code{plot} method presents this information in a helpful (but not publication-ready) way.  

See \code{\link{decorana}} for more information on the DCA, but it is assumed that \code{veg} has rows = quadrats and columns = species.  Here we use the site scores returned by \code{\link{scores.decorana}} which implicitly scales the DCA axes by a constant.

Changepoints are identified by changes in sign of the 2nd derivative which happen outside 1.96 times the standard error of the 2nd derivative (the 'threshold').  These changepoints are provided as a conveniance to the user, but are not based on any hard-and-fast rule.  Careful interpretation by the user is still required.  

Unlike Walker \emph{et al.} (2003) we do not allow the window to get smaller near the ends of the transect, leading to NA's at transect extremes.  
}

\value{
A list of class \code{ecofind} with the following items:

\item{result }{A named matrix, with columns: distance, dcaN, first_derivative, second_derivative and sd_change_pt. Where the 'N' of dcaN is the axis specified by \code{axis_no}.}
\item{func_meta }{A named vector, with items: window and axis_no}
\item{veg }{The original \code{veg} matix passed to ecofind}
\item{dist }{The original \code{dist} vector passed to ecofind}
}

\references{
Walker, S., Wilson, J. B., Steel, J. B., Rapson, G. L., Smith, B., King, W. M., and Cottam, Y. H.  2003.  Properties of ecotones: Evidence from five ecotones objectively determined from a coastal vegetation gradient.  Journal of Vegetation Science, 14: 579--590. \url{http://dx.doi.org/10.1111/j.1654-1103.2003.tb02185.x}
}

\author{
Gretchen Brownstein, Daniel Pritchard
}

\examples{
## G TODO
}
