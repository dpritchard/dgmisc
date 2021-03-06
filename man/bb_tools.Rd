\name{bb_tools}
\alias{bb_tools}
\alias{bb_read}
\alias{bb_parse}
\alias{bb_plot_ca}
\alias{bb_plot_spp_rich}

\title{CMLR Braun Blanquet Tools}
\description{A collection of tools for compiling, parsing and plotting Braun Blanquet (BB) condition and abundance data from the CMLR database.}

\usage{
bb_read(file, survey_year=NA, survey_season=NA, SciNames=FALSE)

bb_parse(raw_bb, verbose = TRUE, log = FALSE)

bb_plot_ca(parsed_bb, samp_unit = NA, plot_to = 'screen', 
	verbose = TRUE, disturbance = FALSE, DistDates=NULL)

bb_plot_spp_rich(parsed_bb, samp_unit = NA, samp_year = NA, 
	plot_to = 'screen', verbose = TRUE)
}

\arguments{
  \item{file }{A string specifying a filename to read and process.}
  \item{survey_year }{An interger. What year was the survey carried out, eg 2013}
  \item{survey_season }{A character string. What season was the survey carried out, eg "Summer" }
  \item{SciNames}{A logical. Does the file to be processed contain a 'Scienfic Names' column (see details)}
  \item{raw_bb }{A dataframe read directly from a \code{.csv} file from the CMLR database.  See details.}
  \item{verbose }{A logical. Should output be printed to screen?}
  \item{log }{A logical. Should output be logged to file?}
  \item{parsed_bb }{A dataframe of class \code{parsed.bb} (i.e. that returned by \code{bb_parse}). If raw data is passed it will be run through \code{bb_parse} with an unavoidable message printed to screen.}
  \item{samp_unit }{Which sampling unit(s) to plot? For \code{bb_plot_ca} a single character string. For \code{bb_plot_spp_rich} a single character string or a vector of character strings.}
  \item{samp_year }{Which sampling season / year combination to plot? A single character string of the format \code{'Season Year'}.}
  \item{plot_to }{To where should the plots be sent? Must be either \code{screen} or \code{png}.}
  \item{disturbance }{A logical. If \code{TRUE} the the user will be prompted for disturbance start and end date(s).}
  \item{DistDates}{a data frame with the disturbance dates for each site see the format below}
}

\details{
\code{bb_read} takes data-forms filled out in the field and returns the information they contain in a sensible long-format dataframe.  
   Importantly, \strong{the file must have 6 rows of metadata.} If it doesn't this function will produce unpredictable results (probably an error).
   Several internal checks must be passed for the function to run.  A failure of these checks will print an error to screen detailing the file and the error.  Currently these check cannot be circumvented.  The specific tests that must be passed are (in order):
   \itemize{
   \item First column of data must be strings and must not be blank. They are assumed to be the FieldName 
   \item \emph{if} \strong{SciNames = TRUE} The second column is the ScientificName, data must be strings and must not be blank.
   \item If there is something in the first column (\emph{i.e.} a species) then this must have data associated with it at some point in the file.  
   \item If there are data in a row, it must have at the first two columns filled out (\emph{i.e.} it must have a FieldName and Presence tick associated with it.).
   \item The Presence, Abundance and Condition columns can not be blank.
   \item The first column must be read by R as strings (\emph{i.e.} it cannot be all numeric codes).  If they are deemed to be numeric values then this is perceived as an error.  
   \item Cell A2 must look something like the phrase 'Sampling Unit' and cell B2 cannot be blank.
   \item Cell A3 must look something like the word 'Date' and cell B3 cannot be blank.
   \item Cell A4 must look something like the word 'Assessor' and cell B4 cannot be blank. 
   \item Cell A5 must look something like the word 'Notes' BUT cell B4 does not need to contain anything
}
At a minimum \code{raw_bb} must contain columns named: 

\code{Year},  \code{Season},  \code{Start.Date..YYYY.MM.DD.},  \code{Scientific.Name},  \code{Modified.Braun.Blanquet.Cover.Abundance.Score..1.7.}  and  \code{Condition.Score..1.5.}.  

This is consistent with raw data extracted from the CMLR database on 05/07/2013.  

\code{parsed_bb} should be the dataframe returned by \code{bb_parse}.  Note that \code{bb_parse} returns a named list, not a dataframe (see below).  The data required by the plotting functions is stored in the first slot, named \code{parsed_bb}.  See the example for guidance on how to access it. 

If \code{samp_unit} is not supplied, or contains an \code{NA} then the functions will attempt to extract this information from the data.  For \code{bb_plot_ca} this must result in a single unique result.  For \code{bb_plot_spp_rich} this will result in sampling units being plotted.  

If \code{samp_year} is not supplied then it is extracted from the data.  This only applies to \code{bb_plot_spp_rich} and must result in a single unique result.

Currently the dates required when \code{disturbance=TRUE} are not available in the database. For now, you have the choice of two inelegant and temporary solutions to get the job done. 1) enter the dates by hand when prompted OR 2) provide a data frame to \code{DistDates} with columns in this exact order: \strong{plot} (the name of the plot as given in the database), \strong{Disturbance} (did the plot have a disturbance: Y or N), \strong{DistNumber} (the number of times the plot was harassed, a number 0 to n), columns 4 to 6 give the disturbance start dates (if any) and columns 7 to 9 give the disturbance end dates (if any). If providing a data frame only 3 disturbances per plot are allowed but any number can be entered via method 1 (currently).  
}

\value{
\code{bb_read } returns a named list containing:
    \item{metadata }{A dataframe with columns 'SamplingUnit', 'Date' and 'Assessor'.  Contains a single row.}
	\item{data }{A long-format dataframe with columns 'Date', 'Assessor', 'SamplingUnit', 'FieldName', 'Scientific Name'(if provided), 'Modified Braun-Blanquet Cover/Abundance Score (1-7)',  'Condition Score (1-5)','Comments'.  Contains a row for each species.}
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

# Don't forget to change your working directory before you start (this is where
# output will go!)
# 
# Read Data
# Assumes data is extracted from the CMLR database. Developed with data extracted on
# 2013-07-05
# Choose a '.csv' file with only 1 site:
bb_data<-read.csv(file=file.choose())
#  Choose a '.csv' file with multiple sites.
bb_data_multi<-read.csv(file=file.choose()) 


## Parse data
out <- bb_parse(bb_data, verbose=TRUE, log=TRUE)
outmulti <- bb_parse(bb_data_multi, verbose=FALSE, log=FALSE)

# Note that the above function returns a list.  The parsed data is in out$parsed_bb
# and is a dataframe with a class of 'parsed.bb'.
class(out$parsed_bb)
# Other functions won't run unless the input data is of class 'parsed.bb', but they
# will coerce the data via bb_parse() with a message about how this is a very bad
# idea.

## Plotting condition / abundance versus time plots.
# Plot to screen
# Parsed data (the recommended approach):
bb_plot_ca(out$parsed_bb, plot_to='screen', disturbance=TRUE)
# Raw data (not recommended): 
bb_plot_ca(bb_data, plot_to='screen')
# Plot to PNG
bb_plot_ca(out$parsed_bb, plot_to='png')

# Example code for batch processing of data:
to_plot <- as.character(unique(outmulti$parsed_bb$SampUnit))
for (a in 1:length(to_plot)) {
	bb_plot_ca(outmulti$parsed_bb, plot_to='png', samp_unit=to_plot[a])
}

## Plotting 'species richness' bar graphs
bb_plot_spp_rich(outmulti$parsed_bb, samp_year='Autumn 2012', samp_unit=c('WW02',
'WW03','WW04'), plot_to='screen')
}

}
