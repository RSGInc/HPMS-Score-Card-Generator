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

# A user-called function to generate a score card either by importing new data or
# specifying previously imported data sets.
Run <- function() {
  
  whitespace()
  
  # This while keeps the tool running until the user hits esc
  while (TRUE) {
    
    # Initial user task choice
    whitespace(gSpaces)
    task <- getUserInput(valid = 1:2, prompt = "What would you like to do?\n\n(1) Import new data\n(2) Generate score card from previously imported data\n\nPlease enter your selection: ")
    
    # Import new data
    if (task == 1) {
      
      whitespace(gSpaces)
      cat("Please use the windows dialog to select one or more data files to import.\n\n")
      files <- choose.files(caption = "Select one or more data files to import.")
      ImportOkay <- ImportFiles(files)
      
      # If no imported data sets came in okay, warn the user
      if (!ImportOkay) {
        warning("None of the supplied files appeared to be in the proper format. No data sets imported.", immediate. = TRUE, call. = FALSE)
      } else {
        
        # Check if any of the new data means we need to update the national summaries
        updateNation()
        
      }
      
      whitespace(4)
      getUserInput("Data import complete! Press 'Enter' to continue.")
      
    # Generate score card(s)
    } else if (task == 2) {
      
      # Score card user task choice
      whitespace(gSpaces)
      sctask <- getUserInput(valid = 1:2, prompt = "Score card generation options:\n\n(1) One card at a time\n(2) All available states in a given year\n\nPlease enter a 1 or 2: ")
      
      # Specify score card save location
      whitespace(gSpaces)
      cat("Please use the windows dialog to select a folder in which to save the scorecard(s).")
      assign("savepath", value = choose.dir(caption = "Select a folder to save the scorecard(s)."), envir = .GlobalEnv)
      
      # Generate a single score card
      if (sctask == 1) {
        
        # Select data to generate score card and assign to global 'all' variable for Jeff D's code to use
        assign("all", value = getStateDataSets(), envir = .GlobalEnv)
        
        # Select a year of national data to compare against and assign to global 'national variable for Jeff D's code to use
        assign("national", value = getNationalDataSet(), envir = .GlobalEnv)
        
        # Generate score card (Dumont's functions)
        cat("This is where Jeff Dumont's work would fit in...\n\n")
        
        tryCatch(expr = {create_pdf(args)}, # need to link this function correctly at some point
                 error = function(e) {warning("Unable to create PDF score card.\n\nA common cause of this warning is that the folder the tool is trying to save to already contains a score card PDF for this combination of data selections and is currently in use. Ensure the PDF is not in use and try again.", immediate. = TRUE, call. = FALSE)})
        
        # try(create_pdf()) # need to link this at some point
        
      } else if (sctask == 2) {
        
        cat("\n\nBatch score card generation has not yet been implemented.\n\n")
        
      }
      
      # Save resulting PDF
      whitespace(4)
      getUserInput("Score card generation complete! Press 'Enter' to continue.")
    
    }
    
  }
  
}

# Prompt the user for the state and year associated with the primary and comparison data sets and
# return those data sets for use in the score card generation process.
getStateDataSets <- function() {
  
  # Get states for which there is data available
  states <- getSavedStates()
  state_codes <- getStateCode(states)
  
  whitespace(gSpaces)
  
  # If no data are available, exit
  if (!length(states) > 0) stop("No data found. Please import data before attempting to generate score cards.", call. = FALSE)
  
  # Print the options to the user
  cat("State Data Sets Available:\n\n")
  cat(paste0("(", state_codes, ") ", states, collapse = "\n"))
  cat("\n\n")
  
  # Get the user's state selection
  state_selection <- getUserInput(valid = c(state_codes, tolower(state_codes)), prompt = "For which state in the above list would you like to generate a score card?\nEnter the associated state code (e.g., NY for New York): ")
  state_selection <- toupper(state_selection)
  
  # Get the years for which there is state data available
  years <- getSavedStateYears(getStateLabel(state_selection))
  
  # Print the options to the user
  cat(paste0("\nData Sets Available for ", getStateLabel(state_selection), ":\n\n"))
  cat(paste0(years, collapse = "\n"))
  cat("\n\n")
  
  # Get the user's year selection
  year_selection <- getUserInput(valid = years, prompt = "For which year of STATE data in the above list would you like to generate a score card for?\nEnter the associated year (e.g., 2014): ")
  
  # Get the user's comparison selection
  cat("\n")
  years_for_comparison <- years[years < year_selection]
  year_compare <- NULL
  if (length(years_for_comparison) > 0) {
    year_compare <- getUserInput(valid = years_for_comparison, prompt = "What year of STATE data would you like to compare against?\nEnter the associated year (e.g., 2013): ", warning.text = "That is not a valid response. Note that a comparison data set must be older.\n")
  }
  
  # Load the data sets
  cat("\nPreparing state selections...\n\n")
  dat <- readRDS(paste0("data/", getStateLabel(state_selection), "/", year_selection, ".RDS"))
  dat[, year_record := year_selection]
  if (!is.null(year_compare)) {
    dat.compare <- readRDS(paste0("data/", getStateLabel(state_selection), "/", year_compare, ".RDS"))
    dat.compare[, year_record := year_compare]
    dat <- rbind(dat, dat.compare, fill = TRUE)
  }
  dat[, state_code := getStateNumFromCode(state_selection)]
  
  return(dat)
  
}

# Prompt the user for the national year data associated with the comparison they would like to make
# return that data set for use in the score card generation process.
getNationalDataSet <- function() {
  
  # Get the years for which there is national data available
  nat_years <- getSavedNatYears()
  
  # Print the options to the user
  cat(paste0("\nAvailable national data sets:\n\n"))
  cat(paste0(nat_years, collapse = "\n"))
  cat("\n\n")
  
  # Get the user's year selection
  nat_compare <- getUserInput(valid = nat_years, prompt = "What year of NATIONAL data would you like to compare against?\nEnter the associated year (e.g., 2013): ", warning.text = "That is not a valid response. Note that a comparison data set must be older.\n")
  
  # Load the data set
  nat <- readRDS(paste0("data/+National/", nat_compare, ".RDS"))
  
  return(nat)
  
}
