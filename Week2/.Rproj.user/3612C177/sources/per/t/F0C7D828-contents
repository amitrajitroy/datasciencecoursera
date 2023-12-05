pollutantmean <- function(directory, pollutant, id=1:332){
 
    # Get the list of files
    files <- list.files(directory, full.names = TRUE)
    
    # Create an empty data frame
    data <- data.frame()
    
    # Loop through the files and add the data to the data frame
    for (i in id) {
      file <- read.csv(files[i])
      data <- rbind(data, file)
    }
    
    # Calculate the mean of the pollutant across all monitors
    mean(data[[pollutant]], na.rm = TRUE)
  
}