###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Keller
# Modified: Matt Landis (May 2017)
#
#
# Description:
#
# This script defines a function to check the data for consistency and errors. 
# These checks can be expanded upon in the next version of the scorecard 
# generator.
#
###########################################################################     

# Load summary data for state and year
loadSummaryData <- function(state_code, year){

  con <- odbcConnect("HPMS")
  
  st_yr_key = paste0(state_code, str_sub(year, start=3, end=4)) 
  
  query <- paste('SELECT Year_Record, State_Code, Data_Item,
  Count(*) AS Record_Count,
  Sum(End_Point - Begin_Point) AS Miles,
  Count(Distinct Route_ID) AS Route_ID_Count
  FROM Review_Sections WHERE StateYearKey=', st_yr_key,
  'GROUP BY Year_Record, State_Code, Data_Item;')
  
  
  data <- sqlQuery(con, query)
  data <- cleanUpQuery(data)
    
  odbcClose(con)
  
  return(data)
}


checkSummary <- function(year, state_code, data){
  # Load FHWA summary

  cat('Checking data summary against FHWA...')

  fhwa_sum <- loadSummaryData(state_code, year)
  
  if (is.null(fhwa_sum)){
    return(TRUE)
  } else {
    # names(fhwa_sum) <- tolower(names(fhwa_sum))
    if ('stateyearkey' %in% names(fhwa_sum)) fhwa_sum[, stateyearkey := NULL]
    
    keys <- c('year_record', 'state_code', 'data_item')
    
    fhwa_sum[, record_count := as.numeric(record_count)]
    fhwa_sum[, route_id_count := as.numeric(route_id_count)]
    fhwa_sum1 <- melt(fhwa_sum, id.vars=keys, variable.name = 'measure',
                      value.name = 'fhwa')
    
    # Summarize the imported data
    # Copy the data first to avoid changing by reference!
    from_db <- copy(data)
    from_db <- from_db[, .(record_count = as.numeric(.N),
                           miles = sum(section_length),
                           route_id_count = as.numeric(length(unique(route_id)))),
                       by=list(year_record, state_code, data_item)]
    
    from_db1 <- melt(from_db, id.vars=keys, variable.name='measure',
                     value.name='db')

    
    keys <- c(keys, 'measure')
    setkeyv(fhwa_sum1, cols=keys)
    setkeyv(from_db1, cols=keys)
    
    # Join original to new
    comp <- from_db1[fhwa_sum1]
    
    check <- all.equal(comp$db, comp$fhwa)
  }
 
  if(!isTRUE(check)){
    cat('...FAILED!\n')
    # Save a dataset
    comp[, diff := fhwa - db]
    diffs <- comp[abs(diff) >= 1e-3, ]
    return(diffs)
    
  } else {
    cat('...passed!\n')
    return(TRUE)
  }
}

# Check imported data for consistency and errors
CheckImport <- function(year, state_code, dat) {
  
  cat(".")
  passedChecks <- TRUE

  if (nrow(dat)==0) passedChecks <- FALSE

  # All begin points should be before the end points
  if (!all(dat[, begin_point <= end_point])) passedChecks <- FALSE
  
  # Check imported data against summary table -------------------------------
  
  check <- checkSummary(year, state_code, dat)
  
  if ( !isTRUE(check) ){
    
    # Save the mismatches
    path <- file.path('data', getStateLabelFromNum(state_code))
    file <- paste0(year, '_summary_fail_on_import.csv')
    fullpath <- file.path(path, file)
    
    # Create new directory if needed
    if (!dir.exists(path)) dir.create(path)
    
    # Write the file
    write.csv(x=check, file=fullpath, na='', row.names=FALSE)
    
    warntext <- paste(year, getStateAbbrFromNum(state_code),
                      'failed summary check.  Saving diffs to',
                      fullpath, '\n')
    warning(warntext)
    
    # Allow the data to proceed, for now...
    # passedChecks <- FALSE
  }
  
  ### Other data checks go here, for example:
  # make sure state codes are correct
  # make sure there's an F_SYSTEM data_item
  
  if (!passedChecks) warning(paste("The", year, getStateLabelFromNum(state_code), "data set has failed a data check. Skipping."), immediate. = TRUE, call. = FALSE)
  
  return(passedChecks)
  
}
