\name{pfs_tools}
\alias{pfs_tools}
\alias{pfs_read}
\alias{pfs_readsec}
\alias{pfs_dequote}
\alias{pfs_write_section}
\alias{pfs_write_comment}
\alias{pfs_write_keyvals}
\alias{pfs_indent_str}
\alias{pfs_format_val}

\title{PFS Tools}
\description{Tools for working with MIKE by DHI PFS files}
\usage{
pfs_read(filename=NULL)
pfs_readsec(fid, nline=0)
pfs_dequote(string)
pfs_write_section(pfs, fid, indent=0, hlevel=NULL)
pfs_write_comment(pfs, fid, indent, hlevel)
pfs_write_keyvals(pfs, fid, indent, hlevel)
pfs_indent_str(indent)
pfs_format_val(key, val, hlevel)
}

\arguments{
  \item{filename }{A string. The file to write.}
  \item{fid }{An internal R file object.}
  \item{nline }{An integer. Denotes the line in the original PFS file.}
  \item{string }{A string. the string to format.}
  \item{pfs }{An R list, possibly of class \code{pfs} or \code{ecolab}. Contains PFS information.}
  \item{indent }{An integer. The level of indentation.}
  \item{helevel }{A string. The names of the higher level in the PFS heriachy.}
  \item{key }{A string. A PFS key}
  \item{val }{A string, possibly coercible to a numeric. A PFS key.}
}


\details{
TODO: Documentation
}
\value{
TODO: Documentation
}

\section{Warning }{Currently \code{pfs_read} should work for most PFS formatted files. This might be useful in the future, but doing anything other than \emph{reading} is very poorly supported. For example: \code{pfs_format_val}, which is needed to write out data in a MIKE compatible way, is currently very skewed towards specific ECOLab template intricacies, which might break in unpredictable ways in the future.}

\author{
Daniel Pritchard
}

\seealso{
\code{\link{ecolab_tools}}
}
\examples{
# TODO
}