## Function to search for the best hospital 
## according to some criteria.
best <- function(state, outcome) {
    ## Read outcome data
    dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(state %in% unique(dataSet$State)))
        stop("invalid state")

    criterionColumn = -1
    if (outcome == "heart attack")
        criterionColumn = 11
    else if (outcome == "heart failure")
        criterionColumn = 17
    else if (outcome == "pneumonia")
        criterionColumn = 23
    else
        stop("invalid outcome")
    
    ## Return hospital name in that state with
    ## lowest 30-day death rate    
    # coerce the criterion column to numeric
    suppressWarnings(dataSet[, criterionColumn] <- as.numeric(dataSet[, criterionColumn]))
    # sort by the criterion, then by the hospital name (tie case)
    sortedData <- dataSet[order(dataSet[, criterionColumn], dataSet[, 2]), ]
    sortedData <- sortedData[sortedData[, 7] == state, ]
    sortedData[1, 2]
}