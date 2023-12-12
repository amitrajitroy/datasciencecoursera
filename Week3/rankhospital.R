## Ranking hospitals by outcome in a state

# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).

# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. 
# For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.

## Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name.

rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with specified ranking
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  data[, index] <- suppressWarnings(as.numeric(data[, index]))
  data <- na.omit(data)
  slice <- subset(data, State == state)
  slice <- slice[order(slice[, index], na.last = TRUE), 2]
  slice <- na.omit(slice)
  if (num == "best") {
    slice[1]
  } else if (num == "worst") {
    slice[length(slice)]
  } else if (num <= length(slice)) {
    slice[num]
  } else {
    NA
  }
}
