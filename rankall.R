## Function returning hospitals number num
## across all states according to some criterion
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dataSet <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  criterionColumn = -1
  if (outcome == "heart attack")
    criterionColumn = 11
  else if (outcome == "heart failure")
    criterionColumn = 17
  else if (outcome == "pneumonia")
    criterionColumn = 23
  else
    stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank 
  # coerce the criterion column to numeric
  suppressWarnings(dataSet[, criterionColumn] <- as.numeric(dataSet[, criterionColumn]))
  # sort the data set first by state and then by the given outcome
  #   then by hospital name
  dataSet <- dataSet[order(dataSet[,7], 
                           dataSet[,criterionColumn], dataSet[,2], na.last = NA),]
  # list of all states
  allStates <- unique(dataSet[,7])
  # first entry of each state
  firstEntryByState <- match(x = allStates, dataSet[, 7])
  
  rowsToExtract <- c()
  # 
  if (num == "best") {
    rowsToExtract <- firstEntryByState
  }
  else if (num == "worst") {
    rowsToExtract <- firstEntryByState
    for (i in 1:(length(rowsToExtract) -1 )) {
      rowsToExtract[i] <- rowsToExtract[i + 1] - 1
    }
    rowsToExtract[length(rowsToExtract)] <- nrow(dataSet)
  }
  else
  {
    rowsToExtract <- firstEntryByState + num - 1
    # for each state, check if we're still in its range
    # if the opposite is the case, specify NA
    for (i in 1:(length(rowsToExtract))) {
      if (rowsToExtract[i] > nrow(dataSet) || dataSet[rowsToExtract[i], 7] != allStates[i])
        rowsToExtract[i] <- NA
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  # columns: hospital, state
  dataSet <- dataSet[rowsToExtract, c(2, 7)]
  names(dataSet) <- c("hospital", "state")
  rownames(dataSet) <- allStates
  # needed for the case of NAs
  dataSet$state <- allStates
  dataSet
}