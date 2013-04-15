## Functional Response Functions 
# Each function specification needs to be listed in 'resp_known' with a description.

## resp_known is the master list of usable functions.
## each named entry here must have a corresponding function entry (e.g. fr_rogersII.R) and vice versa!
fr_responses <- function(show=FALSE){
	resp_known <- list("typeI"="A generic linear type I response.", "rogersII"="Roger's Type II decreasing prey function.")
	
    if(show){
		cat('\n')
		cat('Response', '\t\t', 'Parameters', '\t\t', 'Description', '\n', sep='')
		cat('--------', '\t\t', '----------', '\t\t', '-----------', '\n', sep='')
		for (a in 1:length(resp_known)) { 
            params <- paste(names(formals(fun=get(names(resp_known)[a]))), collapse=',')
			cat(names(resp_known)[a], '\t\t', params, '\t\t', resp_known[[a]], '\n', sep='')
		}
		cat('\n')
	} else {
		return(resp_known)
	}
}