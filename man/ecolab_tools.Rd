\name{ecolab_tools}
\alias{ecolab_tools}
\alias{ecolab_get}
\alias{ecolab_tryget}
\alias{ecolab_write}
\alias{ecolab_sections}
\alias{ecolab_rename}
\alias{ecolab_join}
\alias{ecolab_symbols}
\alias{ecolab_ids}
\alias{ecolab_validate}

\title{ECOLAB Tools}
\description{Tools for working with MIKE by DHI ECOLAB templates}
\usage{
ecolab_get(pfs, type=c('sv', 'c', 'f', 'av', 'p', 'do'))
ecolab_tryget(pfs, issveq=FALSE)
ecolab_write(pfs, filename, elevate=TRUE)
ecolab_sections()
ecolab_rename(pfs)
ecolab_join(pfs1, pfs2, drop=FALSE, validate=FALSE)
ecolab_symbols(pfs, hlevel=NULL)
ecolab_ids(pfs, hlevel=NULL)
ecolab_validate(pfs, elevate=FALSE)
}

\arguments{
  \item{pfs, pfs1, pfs2 }{An R list, possibly of class \code{pfs} or \code{ecolab}. Contains PFS information.}
  \item{type }{What ecolab variable types should be got?}
  \item{issqev }{Is it a state variable equation? If so, we do things different.}
  \item{filename }{A string. The file to write.}
  \item{elevate }{Should warnings become errors? Like a Dalek coming up a flight of stairs.}
  \item{drop }{Should duplicate items in \code{pfs2} be dropped?}
  \item{elevate }{Should the object be validated first? Like a Dalek with nothing better to do.}
  \item{helevel }{A string. The names of the higher level in the PFS hierarchy.}
}


\details{
These functions rely heavily on \code{\link{pfs_tools}}.
}
\value{
TODO: Documentation
}
\author{
Daniel Pritchard
}

\seealso{
\code{\link{pfs_tools}}
}
\examples{
# TODO
}