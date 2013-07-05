\name{pim_tools}
\alias{pim_tools}
\alias{pim_read}
\alias{pim_parse}
\alias{pim_sum}

\title{CMLR Point Intercept Method Tools}
\description{A collection of tools for dealing with Point Intercept Method (PIM) data.}

\usage{
pim_read(file, samp_year=NA, samp_season=NA)
pim_parse(raw_pim)
pim_sum(subunit)
}

\arguments{
  \item{file }{A string specifying a filename to read and process.}
  \item{samp_year }{An interger. What year was the survey carried out, eg 2013}
  \item{samp_season }{A character string. What season was the survey carried out, eg "Summer" }
  \item{raw_pim }{A dataframe read directly from a \code{.csv} file from the CMLR database.  See Details.}
  \item{subunit }{A dataframe of class \code{parsed.pim} (i.e. the output from \code{pim_parse}). See Details.}
}
\details{
      
   \code{pim_read} takes data-forms filled out in the field and returns the information they contain in a sensible long-format dataframe.  
   
   Importantly, \strong{the file must have 6 rows of metadata.} If it doesn't this function will produce unpredictable results (probably an error).
   
   Several internal checks must be passed for the function to run.  A failure of these checks will print an error to screen detailing the file and the error.  Currently these check cannot be circumvented.  The specific tests that must be passed are (in order):
   \itemize{
   \item First two columns of data must be strings and must not be blank. They are assumed to be the FieldName and the ScientificName.
   \item If there is something in the first two columns (\emph{i.e.} a species) then this must have data associated with it at some point in the file.  
   \item If there are data in a row, it must have at the first two columns filled out (\emph{i.e.} it must have a FieldName or a ScientificName associated with it.).
   \item There can be no blank columns in the middle of the dataset.
   \item The first two columns must be read by R as strings (\emph{i.e.} they cannot be all numeric codes).  If they are deemed to be numeric values then this is perceived as an error.  
   \item Cell A2 must look something like the phrase 'Sampling Unit' and cell B2 cannot be blank.
   \item Cell A3 must look something like the word 'Date' and cell B3 cannot be blank.
   \item Cell A4 must look something like the word 'Assessor' and cell B4 cannot be blank.  
   \item Cell A5 must look something like the phrase 'Transect No.' and cell B5 must be able to be coerced to an integer.  
   \item Steps must be labelled sequentially from 1 to n and n must match with the number of data collected (\emph{i.e.} if there are n steps, there must be n*2 data columns.)
   \item There must be an equal number of strata and condition scores and their column headers must be 's' and 'c' (in that order).
   }
   
   Essentially the file head should look something like this:
   \tabular{lllllll}{
   	\tab A \tab B \tab C \tab D \tab E \tab F \cr
   	\tab---------------------
    \tab---------------------
    \tab---------------------
    \tab---------------------
    \tab---------------------
    \tab---------------------\cr
    1 |\tab PIMs stratum \ldots \cr
    2 |\tab Sampling unit \tab BNS01 \cr
    3 |\tab Date \tab 19/10/2012 \cr
    4 |\tab Assessor \tab DP \cr
    5 |\tab Transect No. \tab 1 \cr
    6 |\tab Step \tab \tab 1 \cr
    7 |\tab Field name \tab Scientific name \tab s \tab c \tab s \tab c \cr
    8 |\tab Lept_gran \tab Leptospermum \tab 1 \tab 4 \tab 1 \tab 3 \cr
  }
For \code{pim_parse} at a minimum \code{raw_pim} must contain columns named: 

\code{Year},  \code{Season},  \code{Start.Date..YYYY.MM.DD.},  \code{Scientific.Name},  \code{Sampling.Unit.ID}, \code{Step}, \code{Stratum}, \code{Condition.Score..1.5.} and  \code{Plot.Transect.Number}.  

This is consistent with raw data extracted from the CMLR database on 05/07/2013.  

\code{pim_sum} takes the output from \code{pim_parse} (an error will be thrown otherwise). The function is \emph{intended} to process one site at a time, if more that one site is detected, the function will continue, but a snippy warning is issued. The function returnes a dataframe summarising the pim data by sampling unit (species frequencies and mean condition). See example below for how to do this.
   
}

\value{
	\code{pim_read} returns a named list with two items:
	\item{metadata }{A dataframe with columns 'SamplingUnit', 'Date', 'Assessor' and 'TransectNo'.  Contains a single row.}
	\item{data }{A long-format dataframe with columns 'Date', 'Assessor', 'SamplingUnit', 'Transect', 'Step', 'FieldName', 'ScientificName', 'Strata' and 'Condition'.  Contains a row for each data point (i.e. each species at each step).}
	
	Items returned by \code{pim_read} are either strings or numerics.  If required, coercing to factors will need to be done after the data have been read in.
    
    \code{pim_parse} returns a dataframe of class \code{parsed.pim} with columns 'Date','SampUnit', 'Transect','Step','Strata','ScientificName' and 'Condition'
    
    \code{pim_sum} returns dataframe (of no specific class) with columns 'Season', 'Year','SamplingUnit', 'Transect', 'Date', 'ScientificName', 'Frequency' and 'MeanCondition'. 
}

\author{
Gretchen Brownstein and Daniel Pritchard
}

\section{todo}{
    \itemize{
    \item Make date dynamic.
    \item Make methods code dynamic.
    \item Enforce the 6-row rule.
    \item Check date formatting. Return an R date.
    \item Allow free-format metadata in key:value pairs (odd columns = key; even columns = value).
    }
	
}

\seealso{
\code{\link{bb_tools}}.
}

\examples{
\dontrun{
# First: ensure your current working directory contains the .csv files to process.
all_csv <- sort(Sys.glob('*.csv'))
allmeta = NULL
alldata = NULL
for(a in 1:length(all_csv)){
    print(cat('File number ', a, sep=''))
    pimout <- pim_read(all_csv[a])
    alldata <- rbind(alldata, pimout$data)
    allmeta <- rbind(allmeta, pimout$metadata)
}

# Read Data
# Assumes data is extracted from the CMLR database. Developed with data extracted on
# 2013-07-05

pim_data<-read.csv(file=file.choose())
parsed_data<-pim_parse(pim_data)

#this is to run pim_sum (and set working directory to where you want the file to end up), this sums all transects together for a plot

summary_pim = NULL

for(a in unique(parsed_data$seasonyearsu)){
  print(cat('season-year-SamplingUnit', a, sep=' '))
  subunit<- subset(parsed_data, seasonyearsu== a )
  pimsum<-pim_sum(subunit)
  summary_pim<-rbind(summary_pim, pimsum)
}

# To sum each transect indiviually, do this:

parsed_data$site_trans <-paste(parsed_data$seasonyearsu, parsed_data$Transect) #makes new id

summary_pim = NULL

for(a in unique(parsed_data$site_trans)){
  print(cat('season-year-SamplingUnit', a, sep=' '))
  subunit<- subset(parsed_data, site_trans== a )
  pimsum<-pim_sum(subunit)
  summary_pim<-rbind(summary_pim, pimsum)
}
}}
