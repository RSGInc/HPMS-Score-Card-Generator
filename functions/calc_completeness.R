###########################################################################
# Title: FHWA HPMS Score Card Generator
# Date: July 2016
# Author: Jeff Dumont 
#
#
# Description:
#
# Function to plot availability scores on title page. It implements the 
# coverage validation provided by FHWA.
#
###########################################################################


coverage_join = function(a, b){
  
  join_cols = c('route_id', 'begin_point', 'end_point')
  setkeyv(a, cols=join_cols)
  setkeyv(b, cols=join_cols)
  
  ab = foverlaps(
    a,
    b,
    by.x = join_cols,
    by.y = join_cols,
    type='within',
    mult='first',
    nomatch=NA)
  
  ab[, (c('begin_point', 'end_point')) := NULL]
  setnames(ab, c('i.begin_point', 'i.end_point'), c('begin_point', 'end_point'))
  setkeyv(ab, join_cols)
  
  # ba = foverlaps(
  #   b,
  #   a,
  #   by.x=join_cols,
  #   by.y=join_cols,
  #   type='within',
  #   mult='first',
  #   nomatch=NA)
  # 
  # ba[, (c('begin_point', 'end_point')) := NULL]
  # setnames(ba, c('i.begin_point', 'i.end_point'), c('begin_point', 'end_point'))
  # setkeyv(ba, join_cols)
  # 
  # measure_cols = setdiff(names(ba), join_cols)
  # setnames(ba, measure_cols, paste0(measure_cols, '_tmp'))
  # 
  # abba = merge(
  #   ab,
  #   ba,
  #   by = join_cols,
  #   all.x=TRUE)
  # 
  # abba[variable != variable_tmp]
  # abba[expansion_factor != expansion_factor_tmp]
  
  return(ab)
  
}


calc_completeness <- function(data, year, variable, x, y){
  
  ts <- Sys.time()
  
  data <- data[!(F_SYTEMorig == 7 & NHS != 1)]
  
  type <- 1
  
  # return(type) # for the time being until we can implement the 1 spatial stuff 
  
  if( data[data_item == variable & year_record == year, .N] > 0){
    
    type <- 2
    
    # complete if present ------------------------------------------------------
    # these variables just need to have something to be complete
    
    if( (variable %in% c(
      "STRUCTURE_TYPE", "STRAHNET_TYPE", "TRUCK", "FUTURE_FACILITY", "CAPACITY"))){
      type <- 3
    }
    
    
    # sample variables --------------------------------------------------------
    
    # these are strictly sample variables
    # interpreted as reported sample variables need to have an expansion factor.
    # if no expansion factor, than the coverage is invalidated
    
    if( variable %in% c(
      "PEAK_LANES", "SPEED_LIMIT", "PCT_PEAK_SINGLE", "PCT_PEAK_COMBINATION",
      "K_FACTOR", "DIR_FACTOR", "FUTURE_AADT", "STOP_SIGNS", "AT_GRADE_OTHER",
      "LANE_WIDTH", "MEDIAN_TYPE", "SHOULDER_TYPE", "WIDENING_OBSTACLE",
      "WIDENING_POTENTIAL", "SURFACE_TYPE")){
      
      if(data[data_item == variable & year_record == year & is.na(expansion_factor), .N] == 0 &
          data[data_item == variable & year_record == year, .N] > 0){
        type <- 3
      }
    } 
    
    
    # route_signing route_qualifier --------------------------------------------
    
    if(variable %in% c("ROUTE_SIGNING","ROUTE_QUALIFIER")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage[, required := FACILITYTYPE %in% c(1,2) & (FSYSTEM %in% c(1,2,3,4)|!is.na(NHS))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3
      }
      # pct_coverage = coverage[required == TRUE, sum(!is.na(variable))] / coverage[required == TRUE, .N] 
    }
    
    
    # urban_code ---------------------------------------------------------------
    
    if(variable == "URBAN_CODE"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := FACILITYTYPE %in% c(1,2,4) & (FSYSTEM %in% c(1,2,3,4,5,6)|!is.na(NHS))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    
    # median_width -------------------------------------------------------------
    
    if(variable == "MEDIAN_WIDTH"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.MEDIAN_TYPE <- data[data_item == "MEDIAN_TYPE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as MEDIANTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.MEDIAN_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := MEDIANTYPE %in% 2:7 & (!is.na(expansion_factor))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    
    # shoulder_width_r ---------------------------------------------------------
    
    if(variable == "SHOULDER_WIDTH_R"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SHOULDER_TYPE <- data[data_item == "SHOULDER_TYPE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SHOULDERTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SHOULDER_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := SHOULDERTYPE %in% 2:6 & (!is.na(expansion_factor))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    # shoulder_width_l --------------------------------------------------------
    
    if(variable == "SHOULDER_WIDTH_L"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SHOULDER_TYPE <- data[data_item == "SHOULDER_TYPE" & year_record == year,]
      dat.MEDIAN_TYPE <- data[data_item == "MEDIAN_TYPE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SHOULDERTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SHOULDER_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as MEDIANTYPE 
 from [coverage] A 
 left join [dat.MEDIAN_TYPE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      
       coverage[, required := SHOULDERTYPE %in% 2:6 & MEDIANTYPE %in% 2:7 & (!is.na(expansion_factor))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    
    # peak_parking -------------------------------------------------------------
    
    if(variable == "PEAK_PARKING"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.URBAN_CODE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := URBANCODE<99999 & (!is.na(expansion_factor))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    
    # turn_lanes_r, turn_lanes_l ----------------------------------------------
    
    if(variable %in% c("TURN_LANES_R","TURN_LANES_L")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.ACCESS_CONTROL <- data[data_item == "ACCESS_CONTROL" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.URBAN_CODE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as ACCESSCONTROL 
 from [coverage] A 
 left join [dat.ACCESS_CONTROL] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      
       coverage[, required := URBANCODE<99999 & ACCESSCONTROL>1 & (!is.na(expansion_factor))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3
      }
      
    }
    
    
    # f_system ----------------------------------------------------------------
    
    if(variable == "F_SYSTEM"){
      
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as FSYSTEM 
 from [dat.FACILITY_TYPE] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) where A.value_numeric in (1,2,4)")
      
      if(sum(is.na(coverage$FSYSTEM)) == 0 & nrow(coverage)>0){
        type <- 3 
        
      }
      
    }# end Fsystem
    
    
    # hov_type ----------------------------------------------------------------
    
    if(variable == "HOV_TYPE"){
      
      dat.HOV_TYPE <- data[data_item == "HOV_TYPE" & year_record == year,]
      dat.HOV_LANES <- data[data_item == "HOV_LANES" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as HOVLANES, 
 B.value_numeric as HOVTYPE 
 from [dat.HOV_LANES] A 
 left join [dat.HOV_TYPE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      if(sum(is.na(coverage$HOVTYPE)) == 0 & nrow(coverage)>0){
        type <- 3 
      }
      
    } # end HOV_TYPE
    
    # hov_lanes ---------------------------------------------------------------
    
    if(variable == "HOV_LANES"){
      
      dat.HOV_LANES <- data[data_item == "HOV_LANES" & year_record == year,]
      dat.HOV_TYPE <- data[data_item == "HOV_TYPE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as HOVTYPE, 
 B.value_numeric as HOVLANES 
 from [dat.HOV_TYPE] A 
 left join [dat.HOV_LANES] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      if(sum(is.na(coverage$HOVLANES)) == 0 & nrow(coverage)>0){
        type <- 3
      }
    } # end HOV_LANES
    
    
    # toll_charged ------------------------------------------------------------
    
    if(variable == "TOLL_CHARGED"){
      
      dat.TOLL_CHARGED <- data[data_item == "TOLL_CHARGED" & year_record == year,]
      dat.TOLL_TYPE <- data[data_item == "TOLL_TYPE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as TOLLTYPE, 
 B.value_numeric as TOLLCHARGED 
 from [dat.TOLL_TYPE] A 
 left join [dat.TOLL_CHARGED] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      if(sum(is.na(coverage$TOLLCHARGED)) == 0 & nrow(coverage)>0){
        type <- 3
      }
    } # end TOLL_CHARGED
    
    
    # toll_type ---------------------------------------------------------------
    
    if(variable == "TOLL_TYPE"){
      
      dat.TOLL_TYPE <- data[data_item == "TOLL_TYPE" & year_record == year,]
      dat.TOLL_CHARGED <- data[data_item == "TOLL_CHARGED" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as TOLLCHARGED, 
 B.value_numeric as TOLLTYPE 
 from [dat.TOLL_CHARGED] A 
 left join [dat.TOLL_TYPE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      if(sum(is.na(coverage$TOLLTYPE)) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } # end TOLL_TYPE
    
    
    # county_code -------------------------------------------------------------
    
    if(variable == "COUNTY_CODE"){
      
      dat.COUNTY_CODE <- data[data_item == "COUNTY_CODE" & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as COUNTYCODE 
 from [dat.FACILITY_TYPE] A 
 left join [dat.COUNTY_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as URBANCODE 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := FACILITYTYPE %in% c(1,2) & (FSYSTEM %in% c(1,2,3,4,5)|(FSYSTEM == 6 & URBANCODE == 99999)|!is.na(NHS))]
      
      if(sum(is.na(coverage$COUNTYCODE[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } # end COUNTY_CODE
    
    
    # facility_type ------------------------------------------------------------
    
    if(variable == "FACILITY_TYPE"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FSYSTEM, 
 B.value_numeric as variable 
 from [dat.F_SYSTEM] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as URBANCODE 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := (FSYSTEM %in% c(1,2,3,4,5)|(FSYSTEM == 6 & URBANCODE == 99999)|!is.na(NHS))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # access_control -----------------------------------------------------------
    
    if(variable == "ACCESS_CONTROL"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")

      coverage[, required := FACILITYTYPE %in% c(1,2) & (FSYSTEM %in% c(1,2,3,4)|!is.na(NHS))]
      
      coverage_check = data.table(coverage)
      
      browser()
      
      coverage = coverage_join(
        dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITYTYPE = value_numeric)],
        dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]) %>%
        
        coverage_join(dat.F_SYSTEM[, .(route_id, begin_point, end_point, FSYSTEM = value_numeric)]) %>%
        
        coverage_join(dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)])
      
      coverage[, required := FACILITYTYPE %in% c(1,2) & (FSYSTEM %in% c(1,2,3,4)|!is.na(NHS))]
      
      # Compare the two versions
      setkeyv(coverage_check, key(coverage))
      setcolorder(coverage_check, neworder=names(coverage))
      coverage_check[, data_item := NULL]
      
      all.equal(coverage_check, coverage)
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # ownership ----------------------------------------------------------------
    
    if(variable == "OWNERSHIP"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as URBANCODE 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := FACILITYTYPE %in% c(1,2) & (FSYSTEM %in% c(1,2,3,4,5)|((FSYSTEM == 6) & (URBANCODE<99999))|!is.na(NHS))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3
      }
    } 
    
    
    # through_lanes, aadt -----------------------------------------------------
    
    if(variable %in% c("THROUGH_LANES","AADT")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as URBANCODE 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := FACILITYTYPE %in% c(1,2,4) & (FSYSTEM %in% c(1,2,3,4,5)|((FSYSTEM == 6) & (URBANCODE<99999))|!is.na(NHS))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # counter_peak_lanes ------------------------------------------------------
    
    if(variable == "COUNTER_PEAK_LANES"){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.THROUGH_LANES <- data[data_item == "THROUGH_LANES" & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as THROUGHLANES 
 from [coverage] A 
 left join [dat.THROUGH_LANES] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as URBANCODE 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
       coverage[, required := FACILITYTYPE %in% c(2) & (URBANCODE < 99999 | THROUGHLANES >= 4) &!is.na(expansion_factor)]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # aadt_single_unit, aadt_combination --------------------------------------
    
    if(variable %in% c("AADT_SINGLE_UNIT","AADT_COMBINATION")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := (FSYSTEM == 1 | !is.na(NHS)) & FACILITYTYPE %in% c(1,2) |!is.na(expansion_factor)]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # nhs ---------------------------------------------------------------------
    
    if(variable %in% c("NHS")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required :=  (FSYSTEM == 1 & FACILITYTYPE < 4)]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # signal_type -------------------------------------------------------------
    
    if(variable %in% c("SIGNAL_TYPE")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.ACCESS_CONTROL <- data[data_item == "ACCESS_CONTROL" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.URBAN_CODE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as ACCESSCONTROL 
 from [coverage] A 
 left join [dat.ACCESS_CONTROL] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required :=  (URBANCODE != 99999 &ACCESSCONTROL == 1 &!is.na(expansion_factor))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # pct_green_time ----------------------------------------------------------
    
    if(variable %in% c("PCT_GREEN_TIME")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.NUMBER_SIGNALS <- data[data_item == "NUMBER_SIGNALS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.URBAN_CODE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NUMBERSIGNALS 
 from [coverage] A 
 left join [dat.NUMBER_SIGNALS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := (URBANCODE < 99999 &NUMBERSIGNALS >= 1 &!is.na(expansion_factor) )]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    # number_signals -----------------------------------------------------------
    
    if(variable %in% c("NUMBER_SIGNALS")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SIGNAL_TYPE <- data[data_item == "SIGNAL_TYPE" & year_record == year,]
      dat.PCT_GREEN_TIME <- data[data_item == "PCT_GREEN_TIME" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SIGNALTYPE , 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SIGNAL_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as PCTGREENTIME 
 from [coverage] A 
 left join [dat.PCT_GREEN_TIME] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
       coverage[, required :=  (!is.na(expansion_factor) & !is.na(PCTGREENTIME)) |(!is.na(expansion_factor) & SIGNALTYPE %in% c(1,2,3,4) )]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # pct_pass_sight -----------------------------------------------------------
    
    if(variable %in% c("PCT_PASS_SIGHT")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.THROUGH_LANES <- data[data_item == "THROUGH_LANES" & year_record == year,]
      dat.MEDIAN_TYPE <- data[data_item == "MEDIAN_TYPE" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE , 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.URBAN_CODE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as THROUGHLANES 
 from [coverage] A 
 left join [dat.THROUGH_LANES] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as MEDIANTYPE 
 from [coverage] A 
 left join [dat.MEDIAN_TYPE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := (!is.na(expansion_factor) &URBANCODE == 99999 &THROUGHLANES == 2 &MEDIANTYPE %in% c(1,2))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3
      }
    } 
    
    
    # iri ---------------------------------------------------------------------
    
    if(variable %in% c("IRI")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as SURFACETYPE, coalesce(A.expansion_factor,B.expansion_factor) as expansion_factor
 from [dat.FACILITY_TYPE] A 
 inner join [dat.SURFACE_TYPE] B on 
 A.route_id = B.route_id and A.begin_point = B.begin_point and A.end_point = B.end_point
 ")
      
      coverage <- sqldf("select 
 A.route_id, A.begin_point, A.end_point, A.data_item, A.FACILITYTYPE, A.SURFACETYPE, 
 B.value_numeric as URBANCODE, coalesce(A.expansion_factor,B.expansion_factor) as expansion_factor 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and A.begin_point = B.begin_point and A.end_point = B.end_point
 ")
      
      coverage <- sqldf("select 
 A.route_id, A.begin_point, A.end_point, A.data_item, A.FACILITYTYPE, A.SURFACETYPE, A.URBANCODE,
 B.value_numeric as FSYSTEM , coalesce(A.expansion_factor,B.expansion_factor) as expansion_factor 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and A.begin_point = B.begin_point and A.end_point = B.end_point
 ")
      
      coverage <- sqldf("select 
 A.route_id, A.begin_point, A.end_point, A.data_item, A.FACILITYTYPE, A.SURFACETYPE, A.URBANCODE,A.FSYSTEM,
 B.value_numeric as NHS, coalesce(A.expansion_factor,B.expansion_factor) as expansion_factor 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and A.begin_point = B.begin_point and A.end_point = B.end_point
 ")
      
      coverage <- sqldf("select 
 A.route_id, A.begin_point, A.end_point, A.data_item, A.FACILITYTYPE, A.SURFACETYPE, A.URBANCODE,A.FSYSTEM,A.NHS, 
 B.value_numeric as variable, coalesce(A.expansion_factor,B.expansion_factor) as expansion_factor 
 from [coverage] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := FACILITYTYPE %in% c(1,2) &(FSYSTEM %in% c(1,2,3) | !is.na(NHS) |(!is.na(expansion_factor) & FSYSTEM == 4 & URBANCODE == 99999)) &SURFACETYPE > 1]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # psr ----------------------------------------------------------------------
    
    if(variable %in% c("PSR")){
      
      keep_cols <- c('route_id', 'begin_point', 'end_point', 'section_length', 'expansion_factor', 'data_item', 'value_numeric')
      dat.variable <- data[data_item == variable & year_record == year, ..keep_cols]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year, ..keep_cols]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year, ..keep_cols]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year, ..keep_cols]
      dat.IRI <- data[data_item == "IRI" & year_record == year & is.na(value_numeric), ..keep_cols]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year & value_numeric > 1, ..keep_cols] 
      
      coverage <- sqldf('select
 A.route_id, A.begin_point, A.end_point, A.expansion_factor, A.value_numeric as variable,
 B.value_numeric as FACILITY_TYPE
 from [dat.variable] A
 left join [dat.FACILITY_TYPE] B on
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) where A.expansion_factor is not NULL and B.value_numeric is not NULL')
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as IRI 
 from [coverage] A 
 left join [dat.IRI] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) where B.value_numeric is NULL")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as SURFACE_TYPE 
 from [coverage] A 
 left join [dat.SURFACE_TYPE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ") 
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as URBANCODE 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- data.table(coverage)
      coverage[, required := is.na(IRI) & !is.na(expansion_factor) & SURFACE_TYPE > 1 & 
          ((FSYSTEM %in% c(4, 5, 6) & URBANCODE < 99999 & FACILITY_TYPE %in% c(1, 2)) |
              (FSYSTEM == 5 & FACILITY_TYPE %in% c(1, 2) & URBANCODE == 99999))]
      
      if( !any(is.na(coverage$variable[coverage$required])) & nrow(coverage) > 0 ){
        type <- 3 
      }
    } 
    
    
    # rutting -----------------------------------------------------------------
    
    if(variable %in% c("RUTTING")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := !is.na(expansion_factor) & SURFACETYPE%in%c(2,6,7,8)]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # faulting ----------------------------------------------------------------
    
    if(variable %in% c("FAULTING")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := !is.na(expansion_factor) & SURFACETYPE %in% c(3,4,9,10)]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # cracking_percent or year_last_construction -------------------------------
    
    if(variable %in% c("CRACKING_PERCENT","YEAR_LAST_CONSTRUCTION")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := !is.na(expansion_factor) & SURFACETYPE %in% 2:10]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3
      }
    } 
    
    
    # thickness_rigid ---------------------------------------------------------
    
    if(variable %in% c("THICKNESS_RIGID")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := !is.na(expansion_factor) & SURFACETYPE %in% 3:10]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # thickness_flexible ------------------------------------------------------
    
    if(variable %in% c("THICKNESS_FLEXIBLE")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := !is.na(expansion_factor) & SURFACETYPE %in% c(2,6,7,8)]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # base_type ---------------------------------------------------------------
    
    if(variable %in% c("BASE_TYPE")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := !is.na(expansion_factor) & SURFACETYPE>1]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # base_thickness -----------------------------------------------------------
    
    if(variable %in% c("BASE_THICKNESS")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
      dat.BASE_TYPE <- data[data_item == "BASE_TYPE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as BASETYPE 
 from [coverage] A 
 left join [dat.BASE_TYPE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )") 
      
       coverage[, required := !is.na(expansion_factor) & SURFACETYPE>1 & BASETYPE>1]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3
      }
    } 
    
    
    # terrain_type ------------------------------------------------------------
    
    if(variable %in% c("TERRAIN_TYPE")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.URBAN_CODE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := !is.na(expansion_factor) & URBANCODE<99999]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3
      }
    } 
    
    
    # maintenance_operations ---------------------------------------------------
    
    if(variable %in% c("MAINTENANCE_OPERATIONS", "MAINTENACE_OPERATIONS")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.URBAN_CODE <- data[data_item == "URBAN_CODE" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      dat.TOLL_CHARGED <- data[data_item == "TOLL_CHARGED" & year_record == year,] 
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 B.value_numeric as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as URBANCODE 
 from [coverage] A 
 left join [dat.URBAN_CODE] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as TOLLCHARGED 
 from [coverage] A 
 left join [dat.TOLL_CHARGED] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ") 
      
       coverage[, required := !is.na(TOLLCHARGE) &FACILITYTYPE %in% c(1,2) &(FSYSTEM %in% 1:5 | !is.na(NHS) | (FSYSTEM == 6 & URBANCODE<99999))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    # Year_last_improv ---------------------------------------------------------
    
    if(variable %in% c("YEAR_LAST_IMPROV")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,]
      dat.YEAR_LAST_CONSTRUCTION <- data[data_item == "YEAR_LAST_CONSTRUCTION" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
 B.value_date as variable,B.expansion_factor 
 from [dat.SURFACE_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_date as YEARLASTCONSTRUCTION 
 from [coverage] A 
 left join [dat.YEAR_LAST_CONSTRUCTION] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := (!is.na(expansion_factor) & SURFACETYPE %in% 2:10)]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    # Year_last_construction --------------------------------------------------
    
    if(variable %in% c("YEAR_LAST_CONSTRUCTION")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.YEAR_LAST_CONSTRUCTION <- data[data_item == "YEAR_LAST_IMPROV" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,A.begin_point,A.end_point,A.data_item,A.value_date as YEARLASTCONSTRUCTION, 
 B.value_date as variable,B.expansion_factor 
 from [dat.YEAR_LAST_CONSTRUCTION] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
       coverage[, required := (!is.na(expansion_factor) & !is.na(YEARLASTCONSTRUCTION))]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    
    # route_number ------------------------------------------------------------
    
    if(variable %in% c("ROUTE_NUMBER")){
      
      dat.variable <- data[data_item == variable & year_record == year,]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
      dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
      dat.ROUTE_SIGNING <- data[data_item == "ROUTE_SIGNING" & year_record == year,]
      dat.NHS <- data[data_item == "NHS" & year_record == year,]
      
      coverage <- sqldf("select 
 A.route_id,B.begin_point,B.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
 coalesce(B.value_text,B.value_numeric) as variable,B.expansion_factor 
 from [dat.FACILITY_TYPE] A 
 left join [dat.variable] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 )")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as FSYSTEM 
 from [coverage] A 
 left join [dat.F_SYSTEM] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as ROUTESIGNING 
 from [coverage] A 
 left join [dat.ROUTE_SIGNING] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
      coverage <- sqldf("select 
 A.*, 
 B.value_numeric as NHS 
 from [coverage] A 
 left join [dat.NHS] B on 
 A.route_id = B.route_id and (
 ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
 ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
 ) ")
      
       coverage[, required := (FSYSTEM %in% c(1,2,3,4) | !is.na(NHS)) & FACILITYTYPE %in% 1:2 & ROUTESIGNING %in% 2:9 ]
      
      if(sum(is.na(coverage$variable[coverage$required])) == 0 & nrow(coverage)>0){
        type <- 3 
      }
    } 
    
  }
  
  cat(paste0("\n\t",variable,": ",round(difftime(Sys.time(),ts,units="secs"),2)," secs"))
  
  return(type)
}


