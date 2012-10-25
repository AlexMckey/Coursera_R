complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	  ## Your code here

	nobs <- integer(length(id))
	for (k in 1:length(id))
	{
		nid <- id[k]
		sdir <- paste(getwd(),directory,sep="/")
		sid <- paste(ifelse(nid>=100,"",ifelse(nid>=10,"0","00")),nid,sep="")
		sfile <- paste(sid,"csv",sep=".")
		sfn <- paste(sdir,sfile,sep="/")
		data <- read.csv(sfn)
		good <- complete.cases(data)
		nobs[k] <- sum(good)
	}
	data.frame(id,nobs)
}