###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: March 2021
# Author: Matt Landis 
#
#
# Description:
#
# Function to calculate coverage validation score
# It is the fraction of each variable not missing where it is required.

###########################################################################

calc_quality_all <- function(data, year){
  
  dt_output <- gVariables[, .(Name)]
  
  
  for ( i in 1:nrow(dt_output) ){
    
    variable = dt_output[i, Name]
   
    score = calc_completeness(data, year, variable)
    
    dt_output[i, coverage_score := score] 
  }
  
  
  dt_output[!is.na(coverage_score), coverage_type := 1]
  dt_output[coverage_score > 0, coverage_type := 2]
  dt_output[coverage_score == 1, coverage_type := 3]
  
  return(dt_output)
}
