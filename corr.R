corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        filenames <- as.vector(list.files(directory, full.names = TRUE, pattern= "*.csv"))
        out <- numeric()
        for (f in filenames) {
                data <- read.csv (f, header= T)	       
                n <- sum(complete.cases(data))
                if (n > threshold) {
                        sulf <- data["sulfate"]
                        nitr <- data["nitrate"]
                        out <- append(out,cor(sulf,nitr,use="complete.obs"))
                }
        }
        out
}