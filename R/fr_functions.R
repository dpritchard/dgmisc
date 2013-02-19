## Functional Response Functions 
# Each function specification needs to be listed in 'resp_known' with a description.

## resp_known is the master list of usable functions.
## each named entry here must have a corresponding function entry (e.g. fr_rogersII.R) and vice versa!
fr_responses <- function(show=FALSE){
	resp_known <- list('rogersII'="Roger's Type II decreasing prey function.")
	if(show){
		cat('\n')
		cat('Response', '\t', 'Description', '\n', sep='')
		cat('--------', '\t', '-----------', '\n', sep='')
		for (a in 1:length(resp_known)) { 
			cat(names(resp_known)[a], '\t', resp_known[[a]], '\n', sep='')
		}
		cat('\n')
	} else {
		return(resp_known)
	}
}