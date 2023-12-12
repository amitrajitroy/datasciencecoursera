

## Plot the 30-day mortality rates for heart attack
## Read the outcome data into R via the read.csv function and look at the first few rows.
outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

## To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset),
outcome[, 11] <- as.numeric(outcome[, 11])

## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

## Because we originally read the data in as character (by specifying colClasses = "character" we need to
## coerce the column to be numeric. You may get a warning about NAs being introduced but that is okay.
