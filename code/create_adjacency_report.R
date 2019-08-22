###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This performs the adjacency analysis (section based).
# This function formats the data into the necessary structure to be
# processed by the create_table function
#
###########################################################################

create_adjacency_report <- function(data, year, variable){
  
  highlight_threshold <- gVariables[Name==variable, AH_Thresh]

  report <- getAdjacency(data[FACILITY_TYPE != 4],
                         year, variable, adjacency_change = 'N') 

  report[, totalmiles := NULL]
  
  output <- format_report(report, highlight_threshold)
  
  output <- output[variable != 'Number of\nSections']
  
  return(output)

}
