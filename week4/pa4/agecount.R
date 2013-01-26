agecount <- function(age = NULL){
  
  if (is.null(age)){
    stop("Age is NULL!!")
  }
  
  homicides <- readLines("homicides.txt")
  
  ageC <- as.character(age)
  pattern <- paste("[^0-9]", ageC, " +years +old *?</dd>", sep="")
 # print(pattern)
  v <- grepl(pattern, homicides);
  sum(v)
  
  
}