## Ranking hospitals in all states

# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
# hospitals when deciding the rankings.

## Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
# that the rankhospital function handles ties.

## NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call
# the rankhospital function from the previous section.
# The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
# the function should throw an error via the stop function with the exact message “invalid outcome”. The num
# variable can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are better).
# If the number given by num is larger than the number of hospitals in that state, then the function should
# return NA.

rankall <- function(outcome, num) {
  ## Read outcome data
  data <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome and num are valid
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  if (is.numeric(num)) {
    if (num > 100 || num < 1) {
      stop("invalid rank")
    }
  } else if (!(num %in% c("best", "worst"))) {
    stop("invalid rank")
  }
  
  ## Return hospital name in each state with specified ranking
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  data[, index] <- suppressWarnings(as.numeric(data[, index]))
  data <- na.omit(data)
  states <- unique(data$State)
  result <- data.frame(hospital = character(length(states)), state = states, stringsAsFactors = FALSE)
  for (i in 1:length(states)) {
    slice <- subset(data, State == states[i])
    slice <- slice[order(slice[, index], na.last = TRUE), 2]
    slice <- na.omit(slice)
    if (num == "best") {
      result[i, 1] <- slice[1]
    } else if (num == "worst") {
      result[i, 1] <- slice[length(slice)]
    } else {
      if (num <= length(slice)) {
        result[i, 1] <- slice[num]
      } else {
        result[i, 1] <- NA
      }
    }
  }
  result
}
