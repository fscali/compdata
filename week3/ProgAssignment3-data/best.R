best<-function(state, outcome){
  
  options("warn" = -1)
  ## read outcome data
  
  #note:it's important to use the colClasses option
  #otherwise the mortality rate data winds up getting read into factors rather than character vectors
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses="character")
 
  possibleDisease=c("heart attack", "heart failure", "pneumonia")
  possibleStates=unique(outcome1$State)
  
  ## check that state and outcome are valid
  
  if (! state %in% possibleStates){
    stop("invalid state")
  }
  
  if (! outcome %in% possibleDisease){
    stop("invalid outcome")
  }
 
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  d <- subset(outcome1, outcome1$State == state, select=c(2,11,17,23), )
  d[,2] <- as.numeric(d[,2])
  d[,3] <- as.numeric(d[,3])
  d[,4] <- as.numeric(d[,4])
  
  pdx <- match(outcome, possibleDisease) +1 
  
  d <- d[order(d[,pdx], d[1], na.last=TRUE),]
  
  #idx <- which.min(d[,pdx])
  
  options("warn" = 0)
  #d[idx,1]
  d[1,1]
  
  
}