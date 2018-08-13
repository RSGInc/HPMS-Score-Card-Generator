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

calcQualityAll <- function(data, year, year_compare){
  
  dt_output <- gVariables[, .(Name, Item_Number, Label, Grouping, Extent,
                              Outlier_Min, Outlier_Max,
                              Adjacency_Change, YOY_Change,
                              Quality_Weight, Completeness_Weight)]

  dt_output$Outlier_Score <- NA
  dt_output$Adjacency_Score <- NA
  dt_output$YOY_Score <- NA
  dt_output$Quality_Score <- NA
  
  # Filter out non-NHS local roads
  data <- data[!(F_SYTEMorig == 7 & NHS != 1), ]
  
  for ( i in 1:nrow(dt_output)){
    
    variable <- dt_output$Name[i]
    
    cat('\t', variable, '\n')
    
    if (nrow(data[year_record == year & data_item == variable]) == 0){
      
      score <- NA
      
    } else {
      
      weights <- c(Interstate = 1, NHS = 0, FSYSTEM1 = 1, FSYSTEM2 = 1)

      outliers <- getOutliers(data, year, variable)
      
      if ( is.null(outliers) ){
        outlier_mean <- NA
      } else {
        # Note that getOutliers returns perc_miles that exceed outliers
        outlier_mean <- sum((100 - as.numeric(outliers$perc_miles)) * weights, na.rm=TRUE) /
          sum(weights[!(is.na(outliers$perc_miles) | is.nan(outliers$perc_miles))])
      }
      
      dt_output$Outlier_Score[i] <- outlier_mean
      
      # Check if adjacency change is set for this variable
      adjacency_change <- toupper(gVariables[Name == variable, Adjacency_Change])
      
      if ( adjacency_change %in% c('Y', 'N')){
        adjacency <- getAdjacency(data, year, variable, adjacency_change)
        adj_mean <- sum(as.numeric(adjacency$perc_miles) * weights) / sum(weights)
      } else {
        adj_mean <- NA
      }

      dt_output$Adjacency_Score[i] <- adj_mean
      
      # Check if yoy direction is set
      yoy_change <- toupper(gVariables[Name == variable, YOY_Change])
      
      if (yoy_change %in% c('Y', 'N')){
        yoy <- getYOY(data, year, year_compare, variable, yoy_change)
        yoy_mean <- sum(as.numeric(yoy$perc_miles) * weights) / sum(weights)
      } else {
        yoy_mean <- NA
      }
      
      dt_output$YOY_Score[i] <- yoy_mean
      
      #outliers[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
      #adjacency[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
      #yoy[is.nan(as.numeric(perc_miles)), perc_miles := as.character(0)]
      
      # Calculate the mean of all the measures
      score <- round(mean(c(outlier_mean, adj_mean, yoy_mean), na.rm=TRUE))
      if ( is.nan(score) ){
        score <- NA
      }
    }
    dt_output$Quality_Score[i] <- score
  }
  
  return(dt_output)
}
