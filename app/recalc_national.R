###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller and Jeff Dumont
#
#
# Description:
#
# This script contains functions for summarizing the HPMS data across all
# states for a given year.
#
# To update the national data for 2016, the user would call updateNation(years=2016)
#
###########################################################################

updateNation <- function(years = getAllStateYears()) {
  
  message('Checking ', paste(years, collapse=', '), ' for updates')
  
  # Check if any year in 'years' has had any state data added/deleted/modified
  years_changed <- sapply(X = years, FUN = detectYearChanges)
  years_changed <- years[years_changed]
  
  # For each year that has changed, update the national summaries for that year
  if (length(years_changed) > 0) {

    message("Updating national statistics for ", paste(years_changed, collapse=', '))
    
    summaries <- sapply(X = years_changed, FUN = SummarizeNation)
    
    # Save the summaries
    for (i in 1:length(summaries)) {
      cat(".")
      saveRDS(summaries[[i]], file = paste0("data/+National/", names(summaries)[i], ".rds"))
    }
    
    # Save state snapshots
    sapply(X = years_changed, FUN = function(year) {cat(".");saveRDS(getFileSnapShot(year), file = paste0("resources/fss/", year, ".RDS"))})
    message(" complete!")
    
  } else {
    message(" no new data, skipping.")
  }
  
}

# Given a year, summarize the national statistics of all states
SummarizeNation <- function(year) {
  
  # Load data and summarize
  RDSfiles <- getStatePaths(year)
  
  # variables
  variables <- gVariables[,Name]
  
  # create a folder
  dir.create(
    paste0("data\\+National\\",year),
    showWarnings = FALSE,
    recursive = FALSE,
    mode = "0777")
  
  sumDT <- NULL
  
  message("Processing national data. This will take a while to complete.\n")
  for(variable in variables){
    for(stateRDS in 1:length(RDSfiles)){
      
      message('Variable: ', variable, ', State: ', basename(dirname(RDSfiles[stateRDS])),
              ', year:', year)
      stateDT <- readRDS(RDSfiles[stateRDS])[dataitem == variable, ]
      #if ( !is.null(sumDT) && (ncol(stateDT) != ncol(sumDT))) browser()
      sumDT <- rbind(sumDT, stateDT, fill=TRUE)
    }
    
    yearDir <- file.path('data/+National', year)
    if ( !dir.exists(yearDir) ){
      dir.create(yearDir, recursive = TRUE)
    }
    
    saveRDS(sumDT, file=file.path(yearDir, paste0(variable, ".rds") ))
    sumDT <- NULL
    cat(paste0("Completed: ",variable,"\n"))
  }
  
  return(NULL)
  
}
