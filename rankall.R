rankall <- function(outcome, num = "best") {
## Read outcome data

options("warn" = -1)

sdir <- paste(getwd(),"ProgAssignment2-data",sep="/")
sfn1 <- paste(sdir,"outcome-of-care-measures.csv",sep="/")
d <- read.csv(sfn1, colClasses = "character")
d <- d[,c(2,7,11,17,23)]
d <- d[order(d[,2]),]

## Check that state and outcome are valid

ocn = c("heart attack","heart failure","pneumonia")
if (sum(ocn == outcome) == 0) stop("invalid outcome")
pdx <- match(outcome, ocn) + 2

## For each state, find the hospital of the given rank

d[,3] <- as.numeric(d[,3])
d[,4] <- as.numeric(d[,4])
d[,5] <- as.numeric(d[,5])
##d <- d[order(d[,2],d[,pdx]),]

f <- function(x) {
	if (outcome == "heart attack") tmp <- x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,x$Hospital.Name),]
	else if (outcome == "heart failure") tmp <- x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,x$Hospital.Name),]
	else tmp <- x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,x$Hospital.Name),]
	if (num == "best") idx <- 1
	else if (num == "worst") idx <- nrow(tmp)
	else idx <- num
	tmp[idx,1]
}

res <- data.frame()
for (state in unique(d$State))
{
	hospital <- f(subset(d, State == state))
	res <- rbind(res, data.frame(hospital, state))
}
res

## Return a data frame with the hospital names and the
## (abbreviated) state name
}