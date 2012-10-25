getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
	sdir <- paste(getwd(),directory,sep="/")
	sid <- paste(ifelse(id>=100,"",ifelse(id>=10,"0","00")),id,sep="")
	sfile <- paste(sid,"csv",sep=".")
	sfn <- paste(sdir,sfile,sep="/")
	d <- read.csv(sfn)
	if (summarize) print(summary(d))
	d
}