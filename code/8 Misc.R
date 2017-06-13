###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller
#
#
# Description:
#
# Miscellaneous functions frequently use by other functions within the
# tool. Many are used to scan the tools internal folders to determine what
# data are available. Included is a developer function for clearing out
# all loaded/processed data and outputs.
#
###########################################################################

# Switch back and forth between state labels and codes
getStateLabel <- function(code) {
  return(gState_Labels[abbr %in% code, label])
}

getStateLabelFromNum <- function(num) {
  return(gState_Labels[index %in% num, label])
}

getStateAbbrFromNum <- function(num) {
  return(gState_Labels[index %in% num, abbr])
}


getStateCode <- function(state) {
  return(gState_Labels[label %in% state, abbr])
}

getStateNumFromCode <- function(code) {
  return(gState_Labels[abbr %in% code, index])
}

# Get a list of national data years that have been previously processed
getSavedNatYears <- function() {
  file_path_sans_ext(dir("data/+National", pattern = "20*"))
}

# Get a list of states that have had at least one data set previously processed
getSavedStates <- function() {
  dir("data", pattern = "^[^\\+]")
}

# Get a list of years that a state has data processed for
getSavedStateYears <- function(state) {
  file_path_sans_ext(dir(paste0("data/", state), pattern = "*.RDS"))
}

# Get a list of years across all states has some data
getAllStateYears <- function() {
  
  years <- c()
  states <- getSavedStates()
  for (state in states) years <- c(years, file_path_sans_ext(dir(paste0("data/", state), pattern = "*.RDS")))
  
  return(sort(unique(years)))
}

# Get a list of file paths for all state data given a year
# Returns NULL if none are found
getStatePaths <- function(year) {
  
  paths <- c()
  
  # Loop through state folders and see if a 'year' file exists
  states <- getSavedStates()
  for (state in states) {
    
    years <- dir(paste0("data/", state), pattern = "*.RDS")
    if (year %in% file_path_sans_ext(years)) {
      path <- paste0("data/", state, "/", years[year == file_path_sans_ext(years)])
      paths <- c(paths, path)
    }
  }
  return(paths)
}

# A function that prompts the user for a text input and validates it
getUserInput <- function(prompt = "", valid = "any", warning.text = "That is not a valid response. Please check your answer and try again.\n") {
  
  if (identical(valid, "any")) { # user can supply any input
    input <- readline(prompt)
  } else {
    
    cc <- 1
    input <- NA
    while (!input %in% valid) { # user input must be valid
      
      if (cc > 1) warning(warning.text, immediate. = TRUE, call. = FALSE)
      input <- readline(prompt)
      cc <- cc + 1
      
    }
  }
  
  return(input)
  
}

# A function to add white space to the console
whitespace <- function(n = 100) cat(paste(rep("\n", n), collapse = ""))

# A progress bar (unfinished)
progress <- function(current_stage, stages) {
  
  stages[current_stage] <- paste0("[", stages[current_stage], "]")
  
  cat(paste(stages, collapse = "  -->  "))
  
}

# This function is for testing/reset purposes, and removes all data, data snap shots, and saved score cards
deleteAllData <- function() {
  
  unlink(paste0("data/", getSavedStates()), recursive = TRUE)
  unlink(paste0("resources/fss/", dir("resources/fss/", pattern = "*.RDS")))
  unlink(paste0("data/+National/", dir("data/+National/", pattern = "*.RDS")))
  unlink(paste0("resources/sc/", dir("resources/sc/", pattern = "*.pdf")))
  unlink(paste0("output/", dir("output/", pattern = "*.pdf")))
  
}


getStatesForYear = function(year) {
  
  statesForYear = c()
  states <- getSavedStates()
  for (state in states) 
  {
    if(year%in%as.numeric(file_path_sans_ext(dir(paste0("data/", state), pattern = "*.RDS"))))
    {
      statesForYear = c(statesForYear,state)    
    }  
  }
  return(statesForYear)
}


### END OF TOOL LOADING
cat(" complete!\n")
