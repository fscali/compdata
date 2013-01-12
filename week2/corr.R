corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  files <- dir(directory)
  vec <- numeric(0)
  for (f in files){
    path <- paste(directory, f, sep="/")
    data <- read.csv(path)
    completeMask <- complete.cases(data)
    totalComplete <- sum(completeMask)
    if (totalComplete > threshold){
      myCor <- cor(data$nitrate, data$sulfate, use="complete")
      vec <- c(vec, myCor)
      
    }
    
  }
  
  invisible(vec)
}