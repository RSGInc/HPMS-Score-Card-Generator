###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Keller
#
#
# Description:
#
# This script contains functions for summarizing the HPMS data across all
# states for a given year.
#
###########################################################################

# TODO: replace this argument with something more intelligent - i.e. modify import output to indicate the years of data it worked with.
# Otherwise, the next function will take a long time as more data is imported
updateNation <- function(years = getAllStateYears()) {
  
  # Check if any year in 'years' has had any state data added/deleted/modified
  cat("\n\nUpdating national statistics...")
  years_changed <- sapply(X = years, FUN = detectYearChanges)
  years_changed <- names(years_changed)[years_changed]
  
  # For each year that has changed, update the national summaries for that year
  if (length(years_changed) > 0) {
    
    summaries <- sapply(X = years_changed, FUN = SummarizeNation)
    
    # Save the summaries
    for (i in 1:length(summaries)) {
      cat(".")
      saveRDS(summaries[[i]], file = paste0("data/+National/", names(summaries)[i], ".RDS"))
    }
    
    # Save state snapshots
    sapply(X = years_changed, FUN = function(year) {cat(".");saveRDS(getFileSnapShot(year), file = paste0("resources/fss/", year, ".RDS"))})
    cat(" complete!")
    
  } else {
    cat(" no new data, skipping.")
  }
  
}

# Given a year, summarize the national statistics of all states
# TODO: this function needs to actually do something at some point...
SummarizeNation <- function(year) {
  
  # Load data and summarize
  RDSfiles <- getStatePaths(year)
  
  # variables
  #variables <- gVariables[National_Data_Comparison=="Y",Name]
  variables <- gVariables[,Name]
  
  # create a folder
  dir.create(paste0("data\\+National\\",year), showWarnings = FALSE, recursive = FALSE, mode = "0777")
  
  sumDT <- NULL
  cat("Processing national data. This will take a while to complete.\n")
  for(variable in variables)
  {
    for(stateRDS in 1:length(RDSfiles))
    {
      sumDT <- rbind(sumDT,readRDS(RDSfiles[stateRDS])[data_item==variable,])
        
    }
    saveRDS(sumDT,file=paste0("data\\+National\\",year,"\\",variable,".rds"))
    sumDT <- NULL
    cat(paste0("Completed: ",variable,"\n"))
  }
  
  return(NULL)
  
}

# # A user-called function to update the nation statistics of a selected year
# RecalcNation_old <- function() {
#   
#   # Get all years that there is at least some state data for
#   states <- getSavedStates()
#   allyears <- c()
#   for (state in states) {
#     allyears <- c(allyears, getSavedYears(state))
#   }
#   years <- unique(allyears)
#   
#   if (is.null(years)) stop("No data found. Please import data before attempting to calculate national statistics.", call. = FALSE)
#   
#   # Print the options to the user
#   whitespace()
#   cat("\nYears of Available Data:\n\n")
#   cat(paste0(years, collapse = "\n"))
#   cat("\n\n")
#   
#   # Get the user's state selection
#   year <- getUserInput(valid = years, prompt = "For which year would you like to update the national statistics?\nEnter the associated year (e.g., 2014): ")
#   
#   # Summarize Nation and save for later
#   nat_summary <- SummarizeNation(year)
#   saveRDS(nat_summary, file = paste0("data/+National/", year, ".RDS"))
# }