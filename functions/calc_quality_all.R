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

calc_quality_all <- function(data, year, year_compare){
  
  dt_output <- gVariables[, .(
    Name, Item_Number, Label, Grouping, Extent, Outlier_Min, Outlier_Max,
    Expect_adjacency_change, Expect_YOY_change_2020, Expect_YOY_change,
    Quality_Weight, Completeness_Weight)]

  if ( year_compare <= 2020 ){
    dt_output[, Expect_YOY_change := NULL]
    setnames(dt_output, 'Expect_YOY_change_2020', 'Expect_YOY_change')
  } else {
    dt_output[, Expect_YOY_change_2020 := NULL]
  }

  dt_output$Outlier_Score <- NA
  dt_output$Adjacency_Score <- NA
  dt_output$YOY_Score <- NA
  dt_output$Quality_Score <- NA
  
  # Filter out non-NHS local roads
  data <- data[!(F_SYTEMorig == 7 & NHS != 1), ]
  
  # filter ramps (facility type == 4) 
  data = data[FACILITY_TYPE%in%c(1,2)]
  
  for ( i in 1:nrow(dt_output)){
    
    variable <- dt_output$Name[i]

    # if ( variable %in% c('PSR') ){ browser() }
    # if ( variable %in% c('COUNTER_PEAK_LANES', 'PCT_PASS_SIGHT',
    #   'YEAR_LAST_CONSTRUCTION', 'WIDENING_OBSTACLE', 'ROUTE_NUMBER') ){
    #   browser()
    # } else {
    #   next()
    # }
    
    message('\t', variable)
    
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
      
      dt_output$Outlier_Score[i] <- round(outlier_mean)
      
      # Check if adjacency change is set for this variable
      adjacency_change <- toupper(gVariables[Name == variable, Expect_adjacency_change])
      
      if ( adjacency_change %in% c('Y', 'N')){
        adjacency <- getAdjacency(data, year, variable, adjacency_change)
        adj_mean <- sum(as.numeric(adjacency$perc_miles) * weights, na.rm=TRUE) / 
          sum(weights[!(is.na(adjacency$perc_miles) | is.nan(adjacency$perc_miles))])
      } else {
        adj_mean <- NA
      }

      dt_output$Adjacency_Score[i] <- round(adj_mean)
      
      # Check if yoy direction is set
      yoy_change <- toupper(dt_output[Name == variable, Expect_YOY_change])
      
      if (!is.na(yoy_change) && yoy_change %in% c('Y', 'N')){
        yoy <- getYOY(data, year, year_compare, variable, yoy_change)
        yoy_mean <- sum(as.numeric(yoy$perc_miles) * weights, na.rm=TRUE) /
          sum(weights[!(is.na(yoy$perc_miles) | is.nan(yoy$perc_miles))])
      } else {
        yoy_mean <- NA
      }
      
      # if(yoy_mean > 100 | yoy_mean == 0) browser()
      
      dt_output$YOY_Score[i] <- round(yoy_mean)
      
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
