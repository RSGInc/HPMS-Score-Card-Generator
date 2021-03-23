###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function performs the outlier analysis for a specific variable. The
# outlying thresholds can be set by the user in the resourse folder.
# This function formats the data into the necessary structure to be
# processed by the create_table function.
#
###########################################################################

create_outlier_report <- function(data, year, variable){
  
  highlight_threshold    <- gVariables[Name==variable, OH_Thresh]
  
  report <- getOutliers(data[FACILITY_TYPE != 4], year, variable)

  if ( 'totalmiles' %in% names(report)){
    report[, totalmiles := NULL]
  }
  
  output <- format_report(report, highlight_threshold)
  
  output <- output[variable != 'Number of\nSections']
  
  return(output)

}
