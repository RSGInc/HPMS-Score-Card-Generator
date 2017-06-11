###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: June 2017
# Author: Matthew Landis
#
#
# Description:
#
# Generates a summary of pavement characteristics 
# for the top of "pavement: detailed reveiw"
#
###########################################################################

create_pavement_summary <- function(data, state, year){

  browser()
  
  # Subset the data
  dt <- data[state_code == state & year_record == year &
               data_item %in% c('THROUGH_LANES', 'SURFACE_TYPE'),
             list(year_record, state_code, begin_point, end_point,
                  data_item, value_numeric, section_length,
                  F_SYSTEM, Interstate, NHS, FACILITY_TYPE, THROUGH_LANES, URBAN_CODE)]
  
  # Create "by" variable for summaries
  warning("Jeff, please check the calculation of the group variable",
          immediate. = TRUE)
  
  dt[, group := character(length=nrow(dt))]
  dt[Interstate == 1, group := 'Interstate']
  dt[NHS == 1, group := 'Non-Interstate NHS']
  dt[F_SYSTEM == 1 & NHS != 1, group := 'Other/Minor Arterials']
  dt[F_SYSTEM == 2, group := 'Collectors + Locals']
  setkey(dt, group)
  
  # Check
  #dt[, list(data_item, F_SYSTEM, Interstate, NHS, group)]
  #with(dt, table(group, F_SYSTEM, NHS, Interstate, useNA='always'))
  
  
  # Missing through lanes
  dt_through_lanes <- 
    dt[state_code  ==  state & year_record == year & data_item == "THROUGH_LANES",
       list(n_missing = sum(is.na(value_numeric) | is.null(value_numeric))),
       by=group]
  

  # Summarize surface type
  dt_surftype <- 
    dt[state_code == state & year_record == year & data_item == 'SURFACE_TYPE',
       list(n_missing = sum(is.na(value_numeric) | is.null(value_numeric)),
            n_1 = sum(value_numeric == 1 & !is.na(value_numeric)),
            n_11 = sum(value_numeric == 1 & !is.na(value_numeric)),
            n_sec_gt_011 = sum(section_length >= 0.11)),
       by=group]
  
  #tab <- table(dt_surftype$value_numeric, useNA='always')
  
  stop('need to create a table or graphic for inclusion in grid.arrange')
}
