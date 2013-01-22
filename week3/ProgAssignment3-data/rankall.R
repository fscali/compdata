rankall <- function(outcome, num = 'best') {
  #read in the data, then change the columns we are sorting on to numeric
  oc <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  oc[,11] <- suppressWarnings(as.numeric(oc[,11]))
  oc[,17] <- suppressWarnings(as.numeric(oc[,17]))
  oc[,23] <- suppressWarnings(as.numeric(oc[,23]))
  
  #create a list of states for verification
  states <- sort(unique(oc$State))
  
  #make a list of outcomes to check for
  conditions <- c('heart attack', 'heart failure', 'pneumonia')
  
  if (!outcome %in% conditions) { stop('invalid outcome') }
  
  #pick a column to select based on the outcome
  if (outcome == 'heart attack' ) { selector <- 11 }
  if (outcome == 'heart failure' ) { selector <- 17 }
  if (outcome == 'pneumonia' ) { selector <- 23 }
  
  #split into a list of dataframes based on the state
  myDataSplitted <- split(oc, oc$State)
  
  #sort each dataframe based on the outcome and get the requested num-th value
  myDataSplittedAndSorted <- lapply(myDataSplitted, function(x){ 
    
    sorted<-x[order(x[,selector], x[,2], na.last=TRUE),] 
    sorted <- sorted[!is.na(sorted[,selector]),]
    if (num == 'best') { rowSelector <- 1 }
    else if (num == 'worst') { rowSelector <- nrow(sorted) }
    else {rowSelector <- as.numeric(num)}
    sorted [rowSelector, c(2,7)]
  })
  
  #lapply always returns a list, so I have to cast it back to dataframe
  df <- as.data.frame(do.call(rbind, myDataSplittedAndSorted))
  
  #finally assigna the column names..
  colnames(df) <- c("hospital","state")
  
  #..and return the result
  df
  
  
  # less R-ish way
  
  #create a list for the data frame outcomes
  #hospitals <- c()
  #loop through all states
#   for (i in states) {
#     #make a subset, just for that states
#     soc <- oc[oc$State==i,] 
#     
#     #get a dataframe, sorted on the outcome, then drop the NAs out
#     sorted <- soc[order(soc[,selector],soc[,2], na.last=TRUE),c(7,2,selector)]
#     
#     #sorted <- na.omit(sorted)
#     #sorted <- subset(sorted,!is.na(sorted[,3]))
#     sorted <- sorted[!is.na(sorted[,3]),]
#     
#     
#     #figure out indexes for 'best' and 'worst'
#     if (num == 'best') { rowSelector <- 1 }
#     else if (num == 'worst') { rowSelector <- nrow(sorted) }
#     else {rowSelector <- as.numeric(num)}
#     
#     #return the name of the hospital at that rating
#     hospitals <- append(hospitals, sorted[rowSelector,2])
#   }
#   
#   #create output dataframe
#   data.frame(hospital=hospitals, state=states)
}