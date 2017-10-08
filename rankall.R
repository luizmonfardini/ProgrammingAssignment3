rankall <- function(outcome, num = "best") {
  
  #read the data
  data <- read.csv("~/Downloads/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  #create a list of states 
  namestate <- levels(factor(data[, 7]))
  namehospital <- vector(mode="character") 
  
  for (i in seq(namestate)) {
    namehospital[i] <- rankhospital(namestate[i], outcome, num)
  }
  data.frame(namehospital, namestate)
}
