corr <- function(directory, threshold=0){
  
    # Get the list of files
    files <- list.files(directory, full.names = TRUE)
    
    # Create an empty vector to store the correlations
    correlations <- numeric()
    
    # Loop through the files and calculate the correlation between sulfate and nitrate
    for (file in files) {
      data <- read.csv(file)
      complete_cases <- complete.cases(data)
      if (sum(complete_cases) > threshold) {
        sulfate <- data[complete_cases, "sulfate"]
        nitrate <- data[complete_cases, "nitrate"]
        correlations <- c(correlations, cor(sulfate, nitrate, use = "pairwise.complete.obs"))
      }
    }
    
    # Return the vector of correlations
    return(correlations)
  
}