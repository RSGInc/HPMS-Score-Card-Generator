###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller
#
#
# Description:
#
# This script defines a function to check the data for consistency and errors. 
# These checks can be expanded upon in the next version of the scorecard 
# generator.
#
###########################################################################

# Check imported data for consistency and errors
CheckData <- function(year, state, dat) {
  
  cat(".")
  passedChecks <- TRUE
  
  # All begin points should be before the end points
  if (!all(dat[, begin_point <= end_point])) passedChecks <- FALSE
  
  if (nrow(dat)==0) passedChecks <- FALSE
  
  ### Other data checks go here, for example:
  # make sure state codes are correct
  # make sure there's an F_SYSTEM data_item
  
  if (!passedChecks) warning(paste("The", year, getStateLabelFromNum(state), "data set has failed a data check. Skipping."), immediate. = TRUE, call. = FALSE)
  
  return(passedChecks)
  
}
