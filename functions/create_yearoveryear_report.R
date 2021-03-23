###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This compares the analysis year against the comparison year at the section
# level.
# 
# This function formats the data into the necessary structure to be
# processed by the create_table function.
#
###########################################################################


create_yearoveryear_report <- function(data, year, variable, yearcomparison){
  
  highlight_threshold <- gVariables[Name==variable,YOYH_Thresh]

  report <- getYOY(data[FACILITY_TYPE != 4],
                   year, yearcomparison, variable, yoy_change='N')
  
  if ( 'totalmiles' %in% names(report)){
    report[, totalmiles := NULL]
  }
  
  output <- format_report(report, highlight_threshold)
  
  output <- output[variable != 'Number of\nSections']
  
  return(output)
  
}
