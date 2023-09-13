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
loadSummaryData <- function(stateid, year){
  
  con <- connect_to_db()
  on.exit({odbcClose(con)})
  
  st_yr_key = paste0(stateid, str_sub(year, start=3, end=4)) 
  
  query <- str_glue(
  'SELECT DataYear, StateId, DataItem,
  Count(*) AS Record_Count,
  Sum(EndPoint - BeginPoint) AS Miles,
  Count(Distinct RouteId) AS RouteId_Count
  FROM {sections_table} WHERE StateYearKey={st_yr_key} 
  GROUP BY DataYear, StateId, DataItem;'
  )
  data <- sqlQuery(con, query)
  data <- cleanUpQuery(data)

  return(data)
}


checkSummary <- function(year, stateid, data){
  # Load SQL summary

  cat('Checking R data summary against SQL summary')

  sql_sum <- loadSummaryData(stateid, year)
  
  if (is.null(sql_sum)){
    return(TRUE)
  } else {
    # names(sql_sum) <- tolower(names(sql_sum))
    if ('stateyearkey' %in% names(sql_sum)) sql_sum[, stateyearkey := NULL]
    
    keys <- c('datayear', 'stateid', 'data_item')
    
    sql_sum[, record_count := as.numeric(record_count)]
    sql_sum[, routeid_count := as.numeric(routeid_count)]
    sql_sum1 <- data.table::melt(sql_sum, id.vars=keys, variable.name = 'measure',
                      value.name = 'from_sql')
    
    # Summarize the imported data
    # Copy the data first to avoid changing by reference!
    r_sum <- copy(data)
    r_sum <- r_sum[, .(record_count = as.numeric(.N),
                           miles = sum(end_point-beginpoint),
                           routeid_count = as.numeric(length(unique(routeid)))),
                       by=list(datayear, stateid, data_item)]
    
    r_sum1 <- data.table::melt(r_sum, id.vars=keys, variable.name='measure',
                     value.name='from_r')

    
    keys <- c(keys, 'measure')
    setkeyv(sql_sum1, cols=keys)
    setkeyv(r_sum1, cols=keys)
    
    # Join original to new
    comp <- r_sum1[sql_sum1]
    
    check <- all.equal(comp$from_r, comp$from_sql)
  }

  if(!isTRUE(check)){
    #cat('... saving differences\n')
    # Save a dataset
    comp[, r_vs_sql := from_r - from_sql]
    diffs <- comp[abs(r_vs_sql) >= 1e-3, ]
    return(diffs)
    
  } else {
    cat('...passed!\n')
    return(TRUE)
  }
}

# Check imported data for consistency and errors
CheckImport <- function(year, stateid, dat) {
  
  cat(".")
  passedChecks <- TRUE

  if (nrow(dat)==0) passedChecks <- FALSE

  # All begin points should be before the end points
  if (!all(dat[, beginpoint <= end_point])) passedChecks <- FALSE
  
  # Check imported data against summary table -------------------------------
  
  check <- checkSummary(year, stateid, dat)
  
  if ( !isTRUE(check) ){
    
    # Save the mismatches
    path <- file.path('data', getStateLabelFromNum(stateid))
    file <- paste0(year, '_summary_fail_on_import.csv')
    fullpath <- file.path(path, file)
    
    # Create new directory if needed
    if (!dir.exists(path)) dir.create(path)
    
    # Write the file
    write.csv(x=check, file=fullpath, na='', row.names=FALSE)
    
    warntext <- paste(year, getStateAbbrFromNum(stateid),
                      'failed summary check.  Saving diffs to',
                      fullpath, '\n')
    warning(warntext)
    
    # Allow the data to proceed, for now...
    # passedChecks <- FALSE
  }
  
  ### Other data checks go here, for example:
  # make sure state codes are correct
  # make sure there's an F_SYSTEM data_item
  
  if (!passedChecks) warning(paste("The", year, getStateLabelFromNum(stateid), "data set has failed a data check. Skipping."), immediate. = TRUE, call. = FALSE)
  
  return(passedChecks)
  
}
