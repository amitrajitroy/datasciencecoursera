## Finding the best hospital in a state

## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are tied for best, then hospital “b” should be returned).


# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.

## Here is some sample output from the function.
# source("best.R")
# best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  data[, index] <- suppressWarnings(as.numeric(data[, index]))
  data <- na.omit(data)
  slice <- subset(data, State == state)
  slice <- slice[order(slice[, index], na.last = TRUE), 2]
  slice <- na.omit(slice)
  slice[1]
}
