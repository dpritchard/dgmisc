\name{bb_tools}

\alias{bb_parse}
\alias{bb_plot_ca}
\alias{bb_plot_spp_rich}

\title{CMLR Braun Blanquet Tools}
\description{A collection of tools for parsing and plotting Braun Blanquet (BB) condition and abundance data from the CMLR database.}

\usage{
bb_parse(raw_bb, verbose = TRUE, log = FALSE)

bb_plot_ca(parsed_bb, samp_unit = NA, plot_to = "screen", verbose = TRUE, disturbance = FALSE)

bb_plot_spp_rich(parsed_bb, samp_unit = NA, samp_year = NA, plot_to = "screen", verbose = TRUE)
}

\arguments{
  \item{raw_bb }{A dataframe read directly from a \code{.csv} file from the CMLR database.  See details.}
  \item{verbose }{A logical. Should output be printed to screen?}
  \item{log }{A logical. Should output be logged to file?}
  \item{parsed_bb }{A dataframe of class \code{parsed.bb} (i.e. that returned by \code{bb_parse}). If raw data is passed it will be run through \code{bb_parse} with an unavoidable message printed to screen.}
  \item{samp_unit }{Which sampling unit(s) to plot? For \code{bb_plot_ca} a single character string. For \code{bb_plot_spp_rich} a single character string or a vector of character strings.}
  \item{samp_year }{Which sampling season / year combination to plot? A single character string of the format \code{'Season Year'}.}
  \item{plot_to }{To where should the plots be sent? Must be either \code{screen} or \code{png}.}
  \item{disturbance }{A logical. If \code{TRUE} the the user will be prompted for disturbance start and end date(s).}
}

\details{
At a minimum \code{raw_bb} must contain columns named: 

\code{Year},  \code{Season},  \code{Date},  \code{Scientific.Name},  \code{Modified.Braun.Blanquet.Cover.Abundance.Score..1.7.}  and  \code{Condition.Score..1.5.}.  

This is consistent with raw data extracted from the CMLR database on 30/12/2012.  

\code{parsed_bb} should be the dataframe returned by \code{bb_parse}.  Note that \code{bb_parse} returns a named list, not a dataframe (see below).  The data required by the plotting functions is stored in the first slot, named \code{parsed_bb}.  See the example for guidance on how to access it. 

If \code{samp_unit} is not supplied, or contains an \code{NA} then the functions will attempt to extract this information from the data.  For \code{bb_plot_ca} this must result in a single unique result.  For \code{bb_plot_spp_rich} this will result in sampling units being plotted.  

If \code{samp_year} is not supplied then it is extracted from the data.  This only applies to \code{bb_plot_spp_rich} and must result in a single unique result.

Currently the dates required when \code{disturbance=TRUE} are not available in the database. For now, this is an inelegant, and temporary solution to get the job done. 
}

\value{
\code{bb_parse} returns a named list containing:
	\item{parsed_bb }{A dataframe of class 'parsed.bb' with columns named: \code{Year},  \code{Season},  \code{Date},  \code{SampUnit},  \code{Species},  \code{Abund},  \code{Cond},  \code{RDate},  \code{SeasonYear},  \code{HasNonzeroCond},  \code{HasNonzeroAbund}.}
	\item{cond_abund_info }{A matrix containing useful information about the raw dataset.}
	\item{has_multiple_condition }{A logical. Are there multiple condition scores for each Species / Date / SampUnit combination?}
	\item{multiple_condition }{If \code{has_multiple_condition=TRUE}, a dataframe denoting the offending entries.  Otherwise NA.}
	\item{has_missing_abund }{A logical. Are there missing abundance scores for entires which have a condition score?}
	\item{missing_abund }{If \code{has_missing_abund=TRUE}, a dataframe denoting the offending entries.  Otherwise NA.}
	\item{has_multiple_abundance }{A logical.  Are there multiple abundance scores for each Species / Date / SampUnit combination?}
	\item{multiple_abundance }{If \code{has_multiple_abundance=TRUE}, a dataframe denoting the offending entries.  Otherwise NA.}

\code{bb_plot_ca} and \code{bb_plot_spp_richness} do not return items to the workspace, but rather they plot figures to the screen or to png files using sensible defaults.   
}

\author{
Daniel Pritchard and Gretchen Brownstein
}

\examples{

\dontrun{

# Don't forget to change your working directory before you start (this is where output will go!)

# Read Data
# Assumes data is extracted from the CMLR database. Developed with data extracted on 2012-12-30
# Choose a '.csv' file with only 1 site:
bb_data<-read.csv(file=file.choose())
#  Choose a '.csv' file with multiple sites.
bb_data_multi<-read.csv(file=file.choose()) 

## Parse data
out <- bb_parse(bb_data, verbose=T, log=TRUE)
outmulti <- bb_parse(bb_data_multi, verbose=TRUE, log=FALSE)

# Note that the above function returns a list.  The parsed data is in out$parsed_bb and is a dataframe with a class of 'parsed.bb'.
class(out$parsed_bb)
# Other functions won't run unless the input data is of class 'parsed.bb', but they will coerce the data via bb_parse() with a message about how this is a very bad idea.

## Plotting condition / abundance versus time plots.
# Plot to screen
# Parsed data (the recommended approach):
bb_plot_ca(out$parsed_bb, plot_to='screen', disturbance=TRUE)
# Raw data (not recommended): 
bb_plot_ca(bb_data, plot_to='screen')
# Plot to PNG
bb_plot_ca(out$parsed_bb, plot_to='png')

# Example code for batch processing of data:
to_plot <- as.character(unique(outmulti$SampUnit))
for (a in 1:length(to_plot)) {
	bb_plot_ca(outmulti, plot_to='png', samp_unit=to_plot[a])
	}

## Plotting 'species richness' bar graphs
bb_plot_spp_rich(outmulti, samp_year='Autumn 2012', samp_unit=c('WW02', 'WW03','WW04'), plot_to='screen')
}

}
