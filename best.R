best <- function(state, outcome) {
  #read outcome-of-car-measures data
  data <- read.csv("~/Downloads/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  #check if the state and outcomes are valid
  states <- data[ , 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if ((state %in% states) == FALSE) {
    stop(print("not a valid state"))
  }
  else if ((outcome %in% outcomes) == FALSE) {
    stop(print("not a valid outcome"))
  }
  
  #get the subset of the data with the state
  new_data <- subset(data, State == state)
  
  #get the outcome column from the data file
  if (outcome == "heart attack") {
    outcome_column <- 11
  }
  else if (outcome == "heart failure") {
    outcome_column <- 17
  }
  else {
    outcome_column <- 23
  }
  
  #remove NA's in the outcome column
  required_columns <- as.numeric(new_data[,outcome_column])
  bad <- is.na(required_columns)
  find_data <- new_data[!bad, ]
  
  #find the hospitals with the min outcome
  columns_considered <- as.numeric(find_data[, outcome_column])
  find_rows <- which(columns_considered == min(columns_considered))
  find_hospitals <- find_data[find_rows, 2]
  
  #if there are multiple hospitals with, then return the first
  #hospital name from the alphabetically sorted hospital names list

  if (length(find_hospitals) > 1) {
    hospitals_sorted <- sort(find_hospitals)
    hospitals_sorted[1]
  }
  else {
    find_hospitals
  }
}
