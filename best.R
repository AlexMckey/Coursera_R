best <- function(state, outcome) {
## Read outcome data

options("warn" = -1)

sdir <- paste(getwd(),"ProgAssignment2-data",sep="/")
sfn1 <- paste(sdir,"outcome-of-care-measures.csv",sep="/")
sfn2 <- paste(sdir,"hospital-data.csv",sep="/")
d <- read.csv(sfn1, colClasses = "character")
hosp <- read.csv(sfn2, colClasses = "character")

## Check that state and outcome are valid

ocn = c("heart attack","heart failure","pneumonia")
if (sum(ocn == outcome) == 0) stop("invalid outcome")
st = unique(hosp$State)
if (sum(st == state) == 0) stop("invalid state")

## Return hospital name in that state with lowest 30-day death

d <- subset(d, d$State == state, select = c(2,11,17,23))
d[,2] <- as.numeric(d[,2])
d[,3] <- as.numeric(d[,3])
d[,4] <- as.numeric(d[,4])
pdx <- match(outcome, ocn) + 1
idx <- which.min(d[,pdx])

## rate

options("warn" = 0)

d[idx,1]

}