getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  cId<-sprintf("%03s",id)
  path <- paste(directory, cId, sep="/")
  path <- paste(path, "csv", sep=".")
  data <- read.csv(path)
  #printing to the console inside a function requires an
  #explicit print instruction
  if (summarize)
    print(summary(data))
  
  invisible(data)

}