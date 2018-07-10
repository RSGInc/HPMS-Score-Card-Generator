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
  
  #browser()
  
  if (nrow(data[year_record == year & data_item == variable]) == 0){
    
    score <- 0
    
  } else {
    
    weights <- c(1, 1, 1, 1)
    
    outliers <- getOutliers(data, year, variable)
    outlier_mean <- sum(as.numeric(outliers$perc_miles) * weights) / sum(weights)
    
    # Check if adjacency change is set for this variable
    adjacency_change <- toupper(gVariables[Name == variable, Adjacency_Change])
    
    if ( adjacency_change %in% c('Y', 'N')){
      adjacency <- getAdjacency(data, year, variable, adjacency_change)
      adj_mean <- sum(as.numeric(adjacency$perc_miles) * weights) / sum(weights)
    } else {
      adj_mean <- NA
    }
    
    # Check if yoy direction is set
    yoy_change <- toupper(gVariables[Name == variable, YOY_Change])
    
    if (yoy_change %in% c('Y', 'N')){
      yoy <- getYOY(data, year, year_compare, variable, yoy_change)
      yoy_mean <- sum(as.numeric(yoy$perc_miles) * weights) / sum(weights)
    } else {
      yoy_mean <- NA
    }
    
    #outliers[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
    #adjacency[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
    #yoy[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
    
    # Calculate the mean of all the measures
    score <- round(mean(c(outlier_mean, adj_mean, yoy_mean), na.rm=TRUE))
    if ( is.nan(score) ){
      score <- NA
    }
  }
  
  return(score)
}
