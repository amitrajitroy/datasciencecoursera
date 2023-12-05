complete <- function(directory, id=1:332){
  
  # Get the list of files
  files <- list.files(directory, full.names = TRUE)
  
  # Create an empty data frame
  data <- data.frame()
  
  # Loop through the files and add to the data frame the complete observations
  for (i in id) {
    file <- read.csv(files[i])
    nobs <- sum(complete.cases(file))
    data <- rbind(data, data.frame(id = i, nobs = nobs))
  }
  
  # Remove duplicates
  #data <- data[!duplicated(data), ]
  
  # Return the data frame
  return(data)
  
}