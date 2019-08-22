###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function summarizes the data provided. Calculates counts, number
# of NAs, min, mean, median, and max values. Used to produce the summary
# pages.
#
###########################################################################

summaryFunc <- function(x,weights=rep(1,length(x))){
  # This clause prevents warnings if all(is.na(x))
  # Also need to assign the class of the NA's correctly
  
  if ( all(is.na(x)) ){
    L <- list(
      count = sum(weights),#length(x),
      count.na = sum(is.na(x)),
      min = NA,
      mean = NA,
      median = NA,
      max = NA
    )
    
    for ( i in 3:6 ){
      class(L[[i]]) <- class(x)
    }
    
  } else {
    
    L <-  list(
      count=sum(weights),#length(x),
      count.na=sum(is.na(x)),
      min=round(min(x,na.rm=T),2),
      mean=round(weighted.mean(x,weights,na.rm=T),2),
      median=round(weighted.median(x,weights,na.rm=T),2),
      max=round(max(x,na.rm=T),2)
    )
    
  }
  return(L)
}
