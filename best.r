best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")   ## reads the data from the csv file, setting NA to Not available
  
  ## Check that state and outcome are valid
  permissableoutcomes = c("heart attack","heart failure","pneumonia")  ## defines the permissable outcomes as per the instructions
  if (!outcome %in% permissableoutcomes) { stop("invalid outcome")}    ## if the permissable outcomes are not set in the function then return the message "invalid outcome"
  
  permissableState = unique(data[,7])                                  ## defines the permissable states as any unique value in column 7 (list of state) of the data 
  if (!state %in% permissableState) stop("invalid state")              ## if the permissable states are not set in the function then return the message "invalid state"
  
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,permissableoutcomes)]           ## take the list of permissable outcomes and match them to the colnames in the data
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,]                               ## get a subset of the data file relating to the state entered in the function argument
  minimumvalue <- which.min(as.double(data.state[,colName]))           ## find the minimum value for the relevant outcome (matching to the colname defined above)
  data.list <- sort(data.state[minimumvalue,"Hospital.Name"])          ## create an object called data.list that sorts the data.state alphabetically
  data.list[1]                                                         ## return the first item in the data.list object
  
  
}


best("NY", "heart attack")   
