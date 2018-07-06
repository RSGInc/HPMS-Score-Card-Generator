###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2018
# Author: Matt Landis, based on plotCircle.R by Jeff Dumont 
#
#
# Description:
#
# Function to calculate quality score for a single data item.
# It is based on percent miles meeting criteria
###########################################################################

calcQuality <- function(data,
                         year,
                         year_compare,
                         variable){
  
    if (nrow(data[year_record == year & data_item == variable]) == 0){
    
      type <- 0
    
    } else {
      
      # Check if outliers are set for this variable
      outliers <- getOutliers(data, year, variable)
      
      # Check if adjacency is set for this variable
      adjacency <- getAdjacency(data, year, variable)
        
      # Check expected YOY direction for this variable
      yoy      <- getYOY(data, year, year_compare, variable)
        
      outliers[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
      adjacency[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
      yoy[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
        
      weights <- c(1, 1, 1, 1)
      
      }
      
    return(type)
}
