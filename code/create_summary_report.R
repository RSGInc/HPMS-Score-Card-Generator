###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function creates the summary report and passes an object to the
# create_table_grob function.
#
###########################################################################

create_summary_report <- function(
  data, state, year,
  variable, variable_type, variable_extent, variable_extent_fs, ramps){

  # functional system aggregation
  if( ramps ){
    result1 <- data[state_code == state & year_record == year &
                      data_item == variable & FACILITY_TYPE == 4, , ] 
  } else {
    result1 <- data[state_code == state & year_record == year &
                      data_item == variable & FACILITY_TYPE != 4, , ]
  }
  
  result1[, miles := sum(end_point - begin_point, na.rm = TRUE), by=list(F_SYSTEM)]
  result1[, lanemiles := sum((end_point - begin_point) * THROUGH_LANES, na.rm = TRUE),
          by=list(F_SYSTEM)]
  
  
  if(variable_extent %in% c("SP", "SP*", "FE*")){
    result1[, expandedmiles := sum((end_point - begin_point) * expansion_factor,
                                   na.rm=TRUE), by=list(F_SYSTEM)]

    result1[, expandedlanemiles := sum((end_point - begin_point) * THROUGH_LANES * expansion_factor,
                                       na.rm=TRUE), by=list(F_SYSTEM)]

    if(variable_extent_fs == 4){ # F_SYSTEM 1 is unexpanded
      result1[F_SYSTEM == 1, expandedmiles := miles]     
      result1[F_SYSTEM == 1, expandedlanemiles := lanemiles]     
    }
    
    if(variable_extent_fs == 2){
      result1[Interstate == 1, expandedmiles := miles]     
      result1[Interstate == 1, expandedlanemiles := lanemiles]     
    }          
    
  } else {
    result1[, expandedmiles := miles,]
    result1[, expandedlanemiles := lanemiles,]
  }

  result1 <- switch(variable_type,
                    numeric = result1[, summaryFunc(value_numeric, weights=num_sections), 
                            by=list(F_SYSTEM, miles, expandedmiles, lanemiles, expandedlanemiles)],
                    date = result1[, summaryFunc(lubridate::year(as.Date(as.character(value_date))),
                                          weights=num_sections),
                            by=list(F_SYSTEM, miles, expandedmiles, lanemiles, expandedlanemiles)],
                    result1[, summaryFunc(value_numeric, weights=num_sections)[1:2],
                            by=list(F_SYSTEM, miles, expandedmiles, lanemiles, expandedlanemiles)]
  )
  
  result1[, groupCat := F_SYSTEM + 2]
  result1[, F_SYSTEM := NULL]
  result1 <- result1[!is.na(miles),]
  
  # interstate aggregation
  if(ramps){
    result2 <- data[Interstate == 1 & state_code == state & year_record == year &
                      data_item == variable & FACILITY_TYPE == 4, , ]
  } else {
    result2 <- data[Interstate == 1 & state_code == state & year_record == year &
                      data_item == variable & FACILITY_TYPE != 4, , ] 
  }
  
  result2[,miles := sum(end_point - begin_point, na.rm = TRUE),]
  result2[,lanemiles := sum((end_point - begin_point) * THROUGH_LANES, na.rm = TRUE)]
  
  if(variable_extent %in% c("SP", "SP*")){
    
    result2[, expandedmiles := sum((end_point - begin_point) * expansion_factor, na.rm=TRUE),]
    result2[, expandedlanemiles := sum((end_point - begin_point) * THROUGH_LANES * expansion_factor, na.rm=TRUE)]
  
  } else {

    result2[, expandedmiles := miles,]
    result2[, expandedlanemiles := lanemiles,]
  
  }

  result2 <- switch(variable_type,
                    numeric = result2[, summaryFunc(value_numeric, weights=num_sections),
                            by=list(miles, expandedmiles, lanemiles, expandedlanemiles)],
                    date = result2[, summaryFunc(lubridate::year(as.Date(as.character(value_date))),
                                          weights=num_sections),
                            by=list(miles, expandedmiles, lanemiles, expandedlanemiles)],
                    result2[, summaryFunc(value_numeric, weights=num_sections)[1:2],
                            by=list(miles, expandedmiles, lanemiles, expandedlanemiles)]
  )
  
  result2[, groupCat := 1]
  
  # NHS aggregation
  if(ramps){
    
    result3 <- data[NHS == 1 & state_code == state & year_record == year &
                      data_item == variable & FACILITY_TYPE == 4, , ]
  
  } else {
    
    result3 <- data[NHS==1 & state_code == state & year_record == year &
                      data_item == variable & FACILITY_TYPE != 4, , ]
  
  }
  
  result3[, miles := sum(end_point - begin_point, na.rm=TRUE),]
  result3[, lanemiles := sum((end_point - begin_point) * THROUGH_LANES, na.rm=TRUE)]
  
  if(variable_extent %in% c("SP", "SP*")) {
    
    result3[, expandedmiles := sum((end_point - begin_point) * expansion_factor, na.rm=TRUE),]
    result3[, expandedlanemiles := sum((end_point - begin_point) * THROUGH_LANES * expansion_factor,
                                       na.rm = TRUE), ]
  
  } else {

    result3[, expandedmiles := miles, ]
    result3[, expandedlanemiles := lanemiles, ]
  
  }
  
  result3 <- switch(variable_type,
                    numeric = result3[, summaryFunc(value_numeric, weights=num_sections),
                            by=list(miles, expandedmiles, lanemiles, expandedlanemiles)],
                    date = result3[, summaryFunc(lubridate::year(as.Date(as.character(value_date))),
                                          weights=num_sections),
                            by=list(miles, expandedmiles, lanemiles, expandedlanemiles)],
                    result3[, summaryFunc(value_numeric, weights=num_sections)[1:2],
                            by=list(miles, expandedmiles, lanemiles, expandedlanemiles)]
  )
  
  result3[, groupCat:=2]
  
  result <- rbind(result2, result3, result1)
  
  result[, count := string_format(count)]
  result[, count.na := string_format(count.na)]
  result[, miles := string_format(miles)]
  result[, expandedmiles := string_format(expandedmiles)]
  result[, lanemiles := string_format(lanemiles)]
  result[, expandedlanemiles := string_format(expandedlanemiles)]
  
  
  
  # merges in the empty f_systems so full tables are displayed
  if(nrow(result)> 0){
    result <- merge(data.table(groupCat=1:length(gF_SYSTEM_levels)),
                    result, by="groupCat", all.x=T)
  }

  
  if(variable_type %in% c('numeric', 'date') &
     !variable %in% c('YEAR_LAST_CONSTRUCTION', 'YEAR_LAST_IMPROV') ){
    result[, min := string_format(min)]    
    result[, mean := string_format(mean)]
    result[, median := string_format(median)]
    result[, max := string_format(max)]
  }
  
  if(variable %in% c('YEAR_LAST_CONSTRUCTION', 'YEAR_LAST_IMPROV')){
    result[, mean := round(mean)]
  }
  
  if(nrow(result)>0){
    ob <- create_table_grob(result, variable_type)
    return(ob)
  } else {
    return(textGrob(NoDataString, gp=gpar(fontsize=8,  col="Red")))
  }
}
