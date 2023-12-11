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

calc_completeness_all <- function(data, year, reqs){
  
  # Set minimum score required to be "complete"
  complete_threshold = 0.99
  
  dt_output = merge(
    gVariables[!is.na(Completeness_Weight), .(Name)],
    reqs,
    by = 'Name',
    all=TRUE)
  
  dt_output = dt_output[!is.na(dt_output[, required])]
  
  data = data[!(F_SYTEMorig == 7 & NHS != 1)]

  for ( i in 1:nrow(dt_output) ){
    
    variable = dt_output[i, Name]
   
    score = calc_completeness(data, year, variable)
    
    dt_output[i, coverage_score := score] 
  }
  #browser()
  dt_output[, coverage_type := 1] # Not submitted
  dt_output[coverage_score > 0, coverage_type := 2]     # Submitted but incomplete
  dt_output[coverage_score >= complete_threshold, coverage_type := 3]    # Submitted and complete
  dt_output[, coverage_type := coverage_type * required] # 0 = not required
  
  message('-----------------------')

  return(dt_output)
}
