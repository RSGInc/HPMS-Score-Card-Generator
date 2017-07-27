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

summaryFunc <- function(x){
  # This clause prevents warnings if all(is.na(x))
  if ( all(is.na(x)) ){
    L <- list(
      count = length(x),
      count.na = sum(is.na(x)),
      min = NA,
      mean = NA,
      median = NA,
      max = NA
    )
  } else {
    L <-  list(
      count=length(x),
      count.na=sum(is.na(x)),
      min=round(min(x,na.rm=T),2),
      mean=round(mean(x,na.rm=T),2),
      median=round(median(x,na.rm=T),2),
      max=round(max(x,na.rm=T),2)
    )
  }
  return(L)
}
