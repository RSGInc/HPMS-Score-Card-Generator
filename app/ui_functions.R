###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller & Jeff Dumont
# Modififed: Matt Landis
#
# Description:
#
# This code controls the menu prompts in R Studio's console window. Functions
# included in this script are:
# Run()
# getStateDataSets()
# getNationalDataSet()
#
###########################################################################

# A user-called function to generate a score card either by importing new data or
# specifying previously imported data sets.
Run <- function(task=NA, ...) {
  
  whitespace()
  
  # This while keeps the tool running until the user hits esc
  while (TRUE) {
    
    success <- FALSE
    
    # Initial user task choice
    # p_text <- "What would you like to do?\n\n(1) Import new state data\n(2) Generate score card from previously imported data\n(3) Import new population data\n\nPlease enter your selection: "
    p_text <- 'What would you like to do?\n\n(1) Import new state data\n(2) Generate score card from previously imported data\n(3) Run Batch Process\n\nPlease enter your selection: '                           
    
    if ( is.na(task) ){
      whitespace(gSpaces)
      task <- getUserInput(valid = 1:3, prompt = p_text)
    }
    
    # Import new data
    if (task == 1) {
      
      whitespace(gSpaces)
      #cat("Please use the windows dialog to select one or more data files to import.\n\n")
      #files <- choose.files(caption = "Select one or more data files to import.")
      
      #cat("Please use the windows dialog to select one sample panel data file to import.\n\n")
      #spfile <- choose.files(caption = "Select one sample panel data file to import.",multi=FALSE)
      
      ImportOkay <- ImportData(...)
      
      # If no imported data sets came in okay, warn the user
      if (!ImportOkay) {
        warning("None of the supplied files appeared to be in the proper format. No data sets imported.", immediate. = TRUE, call. = FALSE)
      } else {
        
        # Check if any of the new data means we need to update the national summaries
        # we are not automatically going to do this
        #updateNation()
        
      }
      
      whitespace(4)
      task = NA
      getUserInput("Data import complete! Press 'Enter' to continue. Press 'Esc' to Exit.")
      
      # Generate score card(s)
    } else if (task == 2) {
      
      # Score card user task choice
      whitespace(gSpaces)
      #sctask <- getUserInput(valid = 1:2, prompt = "Score card generation options:\n\n(1) One card at a time\n(2) All available states in a given year\n\nPlease enter a 1 or 2: ")
      sctask <- 1
      
      
      # Sscore card save location
      savepath <- "output/"
      
      # Generate a single score card
      if (sctask == 1) {
        # Select data to generate score card
        data.list <- getStateDataSets(...)
        
        # Select a year of national data to compare against and assign to global 'national variable for Jeff D's code to use
        # TODO: Jeff's code needs to actually use this at some point
        #national <- getNationalDataSet()
        national<-NULL
        
        # Load the population data
        #population <- readRDS(paste0("resources/dat/",data.list[["year_selection"]],"_population.RDS"))
        
        # Generate score card
        whitespace(gSpaces)
        
        create_pdf(data = data.list[["dat"]],
                   state = data.list[["state_code"]],
                   year = data.list[["year_selection"]],
                   year_compare = data.list[["year_compare"]],
                   # population = population,
                   # national = national,
                   path = savepath)
  
      } else if (sctask == 2) {
      
        cat("\n\nBatch score card generation has not yet been implemented.\n\n")
        
      }
      
      
      task = NA
      
      # Detect whether score card generation was successful and return to main menu
      whitespace(4)

      getUserInput("Score card generation complete! Press 'Enter' to continue. Press 'Esc' to Exit.")

    
    } else if (task == 3) {
      
      whitespace(gSpaces)
      
      year <- getUserInput("Please enter which year you want to run: ") 
      
      states <- getStatesForYear(year)

      national<-NULL
      
      savepath <- "output/"
      
      for(state in states)
      {
        data.list = getStateDataSets(getStateCode(state),year,as.character(as.numeric(year)-1))
        
        tryCatch(expr = {suppressWarnings(create_pdf(data = data.list[["dat"]],
                                                     state = data.list[["state_code"]],
                                                     year = data.list[["year_selection"]],
                                                     year_compare = data.list[["year_compare"]],
                                                     #population = population,
                                                     national = national,
                                                     path = savepath));
          success <- TRUE},
          error = function(e) {e; dev.off(); warning("Unable to create PDF score card.\n\nA common cause of this warning is that the folder the tool is trying to save to already contains a score card PDF for this combination of data selections and is currently in use. Ensure the PDF is not in use and try again.", immediate. = TRUE, call. = FALSE)})
        
      }
      
      cat(state," scorecard is complete.")
      
      whitespace(gSpaces)
      #cat("Please use the windows dialog to select one or more data files to import.\n\n")
      #file <- choose.files(caption = "Select one file to import.",multi=FALSE)
      #stop('population is no longer needed for the pavement summary')
      #con <- connect_to_db()

      #population <- sqlQuery(con,paste0("select * from ", poptable) )
  
      #odbcClose(con)
      
      #population <- read.table(file,sep=",",header=TRUE,colClasses=)
      
      #population <- data.table(urban_id=population[order(population[,"URBAN_ID"]),"URBAN_ID"],pop=population[order(population[,"URBAN_ID"]),"POPULATION"])
      
      #population <- population[,.(pop=unique(pop)),by=urban_id]
      
      #saveRDS(population,file=paste0("resources/dat/population.rds"))
      
      whitespace(4)
      task = NA
      getUserInput("Batch complete! Press 'Enter' to continue. Press 'Esc' to Exit.")
      
    }
      
    
    
  } # while
  
}

# Prompt the user for the state and year associated with the primary and comparison data sets and
# return those data sets for use in the score card generation process.
getStateDataSets <- function(state_selection, year_selection, year_compare) {

  if ( missing(state_selection) ){
    
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
  }

  if ( missing(year_selection) ){  
    # Get the years for which there is state data available
    years <- getSavedStateYears(getStateLabel(state_selection))
    
    # Print the options to the user
    cat(paste0("\nData Sets Available for ", getStateLabel(state_selection), ":\n\n"))
    cat(paste0(years, collapse = "\n"))
    cat("\n\n")
    
    # Get the user's year selection
    year_selection <- getUserInput(valid = years, prompt = "For which year of STATE data in the above list would you like to generate a score card for?\nEnter the analysis year (e.g., 2014): ")
  }
  
  if ( missing(year_compare) ){
    # Get the user's comparison selection
    cat("\n")
    years_for_comparison <- years[years < year_selection]
    
    
    year_compare <- NULL
    if (length(years_for_comparison) > 0) {
      year_compare <- getUserInput(valid = years_for_comparison, prompt = "What year of STATE data would you like to compare against?\nEnter the comparison year (e.g., 2013): ", warning.text = "That is not a valid response. Note that a comparison data set must be older.\n")
    } else {
      # FIXME: What should happen if length(years_for_comparison) == 0 ?
      stop('No years available for comparison')
    }
  }
  
  # Load the analysis and comparison year data sets

  #summary_str <- str_interp('\nLoading data for ${state_selection}, ${year_selection}, comparing to ${year_compare}')
  summary_str <- '\nLoading state selections\n\n'
  cat(summary_str)
  
  data_file = file.path('data',
                        getStateLabel(state_selection),
                        paste0(year_selection, ".rds"))
  
  if ( !file.exists(data_file) ){
    stop('Data for ', state_selection, ' and ', year_selection, ' has not been imported')
  }
  
  dat <- readRDS(data_file)
  dat[, year_record := as.numeric(year_selection)]
  if (!is.null(year_compare)) {
    dat.compare <- readRDS(paste0("data/", getStateLabel(state_selection), "/", year_compare, ".rds"))
    dat.compare[, year_record := as.numeric(year_compare)]
    dat <- rbind(dat, dat.compare, fill = TRUE)
  }
  
  #dat <<- dat # this is useful for debugging plotting issues
  
  # Attempt to load the data associated with the four previous years
  dat.prev <- list()
  cc <- 1
  for (i in 1:4) {
    
    # If the comparison year is one of the previous four, we already have it ==> skip
    if (as.numeric(year_selection) - i != as.numeric(year_compare)) {
      
      suppressWarnings(expr = {tryCatch(expr = {dat.prev[[cc]] <- readRDS(paste0("data/", getStateLabel(state_selection), "/", as.numeric(year_selection) - i, ".rds")); cc <- cc + 1},
                                        error = function(e) {})})
    }
    
  }
  
  # Append to analysis and comparison years
  dat.prev <- do.call(rbind, dat.prev)
  dat <- rbind(dat, dat.prev, fill = TRUE)
  
  setkeyv(dat, c("state_code","year_record","route_id","data_item","begin_point","end_point"))
  
  return(list(dat = dat,
              year_selection = as.numeric(year_selection),
              year_compare = as.numeric(year_compare),
              state_code = getStateNumFromCode(state_selection)))
  
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
  nat <- readRDS(paste0("data/+National/", nat_compare, ".rds"))
  
  return(nat)
  
}
