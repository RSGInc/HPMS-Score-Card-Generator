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
loadSummaryData <- function(state, year){

  con <- odbcConnect("HPMS")
  
  # Check that state and year are available
  query <- paste('select distinct Year_Record, State_Code from', summary_table,
                  'order by State_Code, Year_Record')
  
  states_years <- data.table(sqlQuery(con, query))
  
  if ( year %in% states_years$Year_Record &
       state %in% states_years$State_Code ){
    
    # Get the summary data
    query <- paste0('select * from ', summary_table, ' where StateYearKey = ',
                    state, as.numeric(year) %% 100)
  
    data <- data.table(sqlQuery(con, query))
  
    odbcClose(con)
  
    return(data)

  } else {

    warntext <- ''
    
    if ( !year %in% states_years$Year_Record ){
      warntext <- paste(warntext, 'Year', year, 'is not in', summary_table, '\n')
    } 
    
    if ( !state %in% states_years$State_Code){
      warntext <- paste(warntext, 'State', state, 'is not in', summary_table, '\n')
    }

    warntext <- paste(warntext, 'returning NULL')
    warning(warntext, immediate.=TRUE)
    return(NULL)
  }
}

compareToSummary <- function(fhwa_sum, data){
  names(fhwa_sum) <- tolower(names(fhwa_sum))
  fhwa_sum[, stateyearkey := NULL]

  keys <- c('year_record', 'state_code', 'data_item')

  fhwa_sum[, record_count := as.numeric(record_count)]
  fhwa_sum[, route_id_count := as.numeric(route_id_count)]
  fhwa_sum1 <- melt(fhwa_sum, id.vars=keys, variable.name = 'measure',
                     value.name = 'fhwa')
  
  # Summarize the imported data
  from_db <- data[, .(record_count = as.numeric(.N),
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

  return(comp)
}


# Check imported data for consistency and errors
CheckImport <- function(year, state, dat) {
  
  cat(".")
  passedChecks <- TRUE

  if (nrow(dat)==0) passedChecks <- FALSE

  # All begin points should be before the end points
  if (!all(dat[, begin_point <= end_point])) passedChecks <- FALSE
  
  # Check imported data against summary table -------------------------------
  
  # Load FHWA summary
  fhwa_sum <- loadSummaryData(state, year)

  if (!is.null(fhwa_sum)){
    comp <- compareToSummary(fhwa_sum, dat)
    check <- all.equal(comp$db, comp$fhwa)
    if (!check ) stop('Imported data are not identical to summary')
    #if (!check) passedChecks <- FALSE
  }
  
  ### Other data checks go here, for example:
  # make sure state codes are correct
  # make sure there's an F_SYSTEM data_item
  
  if (!passedChecks) warning(paste("The", year, getStateLabelFromNum(state), "data set has failed a data check. Skipping."), immediate. = TRUE, call. = FALSE)
  
  return(passedChecks)
  
}
