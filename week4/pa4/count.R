count <- function(cause=NULL){
  homicides <- readLines("homicides.txt")
  #check that "cause" is non-NULL, else throw error
  if (is.null(cause)){
    stop("Error cause=NULL")  
  }
  
  allowedCause <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  if (! cause %in% allowedCause){
    stop("Error cause not allowed")
  }
  
  allowedCauseRegex <- c("<dd>Cause: +[Aa]sphyxiation", "<dd>Cause: +[Bb]lunt [Ff]orce", "<dd>Cause: +[Oo]ther", "<dd>Cause: +[Ss]hooting", "<dd>Cause: +[Ss]tabbing", "<dd>Cause: +[Uu]nknown")
  
  match <- grepl(cause,allowedCause)
  allowedCausePattern <- allowedCauseRegex[match]
  
  match1 <-grepl(allowedCausePattern, homicides)
  
  #extract causes of death
  #return integer containing count of homicides for that cause
  sum(match1)
}