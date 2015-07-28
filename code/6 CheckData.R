###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Keller
#
#
# Description:
#
# Author needs to add a description!
#
###########################################################################

# Check imported data for consistency and errors
CheckData <- function(year, state, dat) {
  
  cat(".")
  passedChecks <- TRUE
  
  # All begin points should be before the end points
  if (!all(dat[, begin_point <= end_point])) passedChecks <- FALSE
  
  ### Other data checks go here, for example:
  # make sure state codes are correct
  # make sure there's an F_SYSTEM data_item
  
  if (!passedChecks) warning(paste("The", year, getStateLabelFromNum(state), "data set has failed a data check. Skipping."), immediate. = TRUE, call. = FALSE)
  
  return(passedChecks)
  
}
