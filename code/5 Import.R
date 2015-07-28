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

# This function runs the entire data import process
ImportFiles <- function(files = choose.files()) {
  
  success <- c()
  
  for (file in files) {
    
    # Read data
    cat(paste0("\nProcessing ", basename(file), "...\n"))
    data <- ReadFile(file)
    
    if (is.null(data)) {
      success <- c(success, FALSE)
    } else {
      
      # Chop imported data into year-state combinations
      data <- SegmentDataSet(data)
      
      # Check data sets
      cat("Checking...")
      passedChecks <- unlist(lapply(X = data, FUN = function(x) CheckData(year = x[["year"]], state = x[["state"]], dat = x[["data"]])))
      data[!passedChecks] <- NULL
      cat(" complete!")
      
      # Format data sets
      cat("\nFormatting...")
      for (i in 1:length(data)) data[[i]][["data"]] <- FormatDataSet(data[[i]][["data"]])
      cat(" complete!")
      
      # Save data sets
      cat("\nSaving to disk...")
      for (i in 1:length(data)) SaveDataSet(year = data[[i]][["year"]], state = data[[i]][["state"]], dat = data[[i]][["data"]])
      cat(" complete!\n")
      
      success <- c(success, TRUE)
      
    }
    
  }
  return(any(success))
}

# Attempt to import a data file specified by the user
ReadFile <- function(file) {
  
  # These are the expected columns and classes
  colClasses <- c("year_record" = "integer", "state_code" = "integer", "route_id" = "factor",
                  "begin_point" = "numeric", "end_point" = "numeric", "data_item" = "factor",
                  "value_numeric" = "numeric", "value_text" = "character", "value_date" = "character")
  
  # Try to load the data
  tryCatch(expr = {dat <- fread(file, colClasses = colClasses);
                   factorCols <- names(colClasses[colClasses == "factor"]);
                   dat[, (factorCols) := lapply(X = .SD, FUN = factor), .SDcols = factorCols];
                   return(dat)},
           error = function(e) {warning(paste(basename(file), "does not appear to be in the proper format. Skipping."), immediate. = TRUE, call. = FALSE); return(NULL)})
  
}

# If an imported data contains data for multiple states and/or years, break it into multiple data sets
SegmentDataSet <- function(dat) {
  
  dat.list <- list()
  datasets <- unique(dat[, .(year_record, state_code)])
  
  cc <- 1
  for (i in 1:datasets[, .N]) {
    year <- datasets[i, year_record]
    state <- datasets[i, state_code]
    dat.list[[cc]] <- list("year" = year, "state" = state, "data" = dat[year_record == year & state_code == state, !c("year_record", "state_code"), with = FALSE])
    cc <- cc + 1
  }
  
  return(dat.list)
  
}

# Format a given data set for use in the score card generation process
FormatDataSet <- function(dat) {
  
  cat(".")
  data.expanded <- merge(dat,
                         dat[data_item == "F_SYSTEM", .(route_id, F_SYSTEM.begin_point = begin_point, F_SYSTEM.end_point = end_point, F_SYSTEM = value_numeric)],
                         by = "route_id", all.x = TRUE, allow.cartesian = TRUE)
  
  # TODO: Could use a more explicit way of matching F_SYSTEM values to road segments
  # Currently, we're losing about 3% of records from mismatches
  data.collapsed <- data.expanded[begin_point <= F_SYSTEM.end_point & begin_point >= F_SYSTEM.begin_point & end_point <= F_SYSTEM.end_point & end_point >= F_SYSTEM.begin_point,
                                  .(route_id, F_SYSTEM, begin_point, end_point, data_item, value_numeric, value_text)]
  
  return(data.collapsed)
  
}

# Save a data set in the appropriate location for a given state and year
SaveDataSet <- function(year, state, dat) {
  
  state <- getStateLabelFromNum(state)
  
  path <- paste0("data/", state)
  file <- paste0(year, ".RDS")
  fullpath <- paste0(path, "/", file)
  
  # Create new directory if needed
  if (!dir.exists(path)) dir.create(path)
  
  # See if a data set for this year and state already exists
  okayToSave <- TRUE
  if (file %in% dir(path)) {
    # File exists, check with user
    overwrite <- getUserInput(valid = c("Y", "y", "N", "n"), prompt = paste0("Processed file already exists for ", state, " ", year, ". Overwrite it? (Y/N): "))
    if (!tolower(overwrite) == "y") okayToSave <- FALSE
  }
  
  if (okayToSave) saveRDS(dat, file = fullpath)
  
}
