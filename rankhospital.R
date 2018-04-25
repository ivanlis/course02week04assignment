## Function searching for hospital number num
## in a given state according to some outcome
rankhospital <- function(state, outcome, num = "best") {
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
  ## the given rank 30-day death rate    
  # coerce the criterion column to numeric
  suppressWarnings(dataSet[, criterionColumn] <- as.numeric(dataSet[, criterionColumn]))
  # sort by the criterion, then by the hospital name (tie case)
  sortedData <- dataSet[order(dataSet[, criterionColumn], dataSet[, 2], na.last = NA), ]
  sortedData <- sortedData[sortedData[, 7] == state, ]
  
  # extract the hospital number num
  rankNum = 
    if (num == "best")
      1
    else if (num == "worst")
      nrow(sortedData)
    else
      num
  
  if (rankNum > nrow(sortedData))
    return(NA)
  
  sortedData[rankNum, 2]
}