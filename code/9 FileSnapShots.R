###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller
#
#
# Description:
#
# Functions for working with formatted state data
#
###########################################################################

# Take a file snap shot of state data for a given year
getFileSnapShot <- function(year) {
  
  folders <- paste0("data/", getSavedStates())
  snapshot <- fileSnapshot(folders, pattern = paste0(year, ".rds"), md5sum = TRUE, full.names = TRUE)
  return(snapshot)
  
}

# Check if any state data has been added/removed/changed for a given year
detectYearChanges <- function(year) {
  
  cat(".")
  before <- NULL
  
  # Get the previous file snapshot...
  tryCatch(expr = {before <- suppressWarnings(readRDS(file = paste0("resources/fss/", year, ".rds")))},
           error = function(e) {}) # If there isn't one, but we're calling this function, we need to create one
  
  # If one is found, compare it to the current snapshot...
  if (!is.null(before)) {
    after <- getFileSnapShot(year)
    
    if (nrow(before$info) != nrow(after$info)) return(TRUE) # if there are a different number of files
    if (any(!(before$info == after$info)[, "md5sum"])) return(TRUE) # if the content of any files are different
    
  } else {
    return(TRUE)
  }
  
  return(FALSE)
  
}
