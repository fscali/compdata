complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  complete.data <- data.frame(id=rep(""), nobs=rep(1), stringsAsFactors=FALSE)
  n <- 1
  
  for (i in id){
    
    cId<-sprintf("%03s",i)
    path <- paste(directory, cId, sep="/")
    path <- paste(path, "csv", sep=".")
    data <- read.csv(path)
    completeMask <- complete.cases(data)
    complete.data[n, ] <- c(i, sum(completeMask))
    n<-n+1
    
  }
  complete.data
  
}