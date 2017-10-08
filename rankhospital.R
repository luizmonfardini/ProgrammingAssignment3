rankhospital <- function(state, outcome, num = "best") {
  ## Read data
  data <- read.csv("~/Downloads/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  ## Verify if that outcome and state are valid
  goodOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% goodOutcome) { stop("not a valid outcome")}
  
  goodState = unique(data[,7])
  if (!state %in% goodState) stop("not a valid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,goodOutcome)]
  
  ## Hospital name in that state with the rank 30-day death rate
  data.state <- data[data$State==state,]
  # use outcome to order data
  sorted.data.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  #handle num input
  if (num=="best") num = 1
  if (num=='worst') num = nrow(sorted.data.state)
  
  sorted.data.state[num,"Hospital.Name"]
}
