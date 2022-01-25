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


coverage_join = function(a, b, check=FALSE){
  
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
  
  if ( 'i.expansion_factor' %in% names(ab) ){
    
    # coalesce(expansion_factor, i.expansion_factor)
    ab[is.na(expansion_factor), expansion_factor := i.expansion_factor]
    ab[, i.expansion_factor := NULL]
  }
  
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

get_coverage_data = function(
  data,
  year,
  variable,
  data_items,
  value_date = c('YEAR_LAST_IMPROVEMENT', 'YEAR_LAST_CONSTRUCTION'),
  value_text = c()){
  
  coverage_list = list()
  
  # List cols to keep
  
  if ( variable %in% value_date ){
    value_var = 'value_date'
    # } else if ( variable %in% value_text ){  # Assume target variable is not value_text
    #   value_var = 'value_text'
  } else {
    value_var = 'value_numeric'
  }
  
  keep_cols = c('route_id', 'begin_point', 'end_point', value_var)
  
  variable_dt = data[
    data_item == variable & year_record == year, 
    c(keep_cols, 'expansion_factor'), with = FALSE]
  
  setnames(variable_dt, value_var, 'variable')
  
  # Get all the other data items
  for ( di in data_items ){
    
    # List cols to keep
    if ( di %in% value_date ){
      value_var = 'value_date'
    } else if ( di %in% value_text ){
      value_var = 'value_text'
    } else {
      value_var = 'value_numeric'
    }
    
    keep_cols = c('route_id', 'begin_point', 'end_point', value_var)
    
    di_dt = data[data_item == di & year_record == year, keep_cols, with = FALSE]
    
    setnames(di_dt, value_var, di)
    
    coverage_list[[di]] = di_dt
  }
  
  # Join each data set on route_id, begin_point, end_point
  # These are left joins.  Start with data with largest number of rows
  # like URBAN_ID or F_SYSTEM
  
  for ( i in seq_along(coverage_list) ){
    if ( i == 1 ){
      coverage = coverage_list[[i]]
    } else {
      coverage = coverage_join(coverage, coverage_list[[i]])
    }
  }
  
  coverage = coverage_join(coverage, variable_dt)
  
  return(coverage)
  
}

# NOTE: When updating, use get_coverage_data instead of specifying data manually

calc_completeness <- function(data, year, variable){
  
  ts <- Sys.time()
  on.exit(expr = {
    message(paste0("\t",variable))
  })
  
  # If item is not present, return 0
  if ( data[data_item == variable & year_record == year, .N] == 0 ){
    score = 0
    return(score)
  } else
    
    # complete if present ------------------------------------------------------
  # these variables just need to have something to be complete
  
  if( (variable %in% c(
    "STRUCTURE_TYPE", "STRAHNET_TYPE", "NN"))){
    score = 1
    return(score)
  } else
    
    
    # sample variables --------------------------------------------------------
  
  # these are strictly sample variables
  # interpreted as reported sample variables need to have an expansion factor.
  # if no expansion factor, than the coverage is invalidated
  
  if( variable %in% c(
    "AT_GRADE_OTHER", "DIR_FACTOR", "FUTURE_AADT", "K_FACTOR",
    "LANE_WIDTH", "MEDIAN_TYPE", 
    "PEAK_LANES", "PCT_DH_SINGLE_UNIT", "PCT_DH_COMBINATION",
    "SHOULDER_TYPE", "STOP_SIGNS",
    "WIDENING_POTENTIAL")){
    
    # Calculate fraction of rows with expansion_factors
    score = data[data_item == variable & year_record == year & !is.na(expansion_factor), .N] /
      data[data_item == variable & year_record == year, .N]
    
    return(score)
    
  } else
    
    # through_lanes, aadt -----------------------------------------------------
  
  if(variable %in% c("AADT", "THROUGH_LANES")){
    
    # FACILITY_TYPE in (1,2,4) AND (F_SYSTEM in (1,2,3,4,5) or (F_SYSTEM = 6 and URBAN_ID  <99999) or NHS)
    
    # dat.variable <- data[data_item == variable & year_record == year,]
    # dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    # dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    # dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    # dat.NHS <- data[data_item == "NHS" & year_record == year,]
    # 
    # 
    # coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
    #   coverage_join(
    #     dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)]) %>%
    #   coverage_join(
    #     dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
    #   coverage_join(
    #     dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)]) %>%
    #   coverage_join(
    #     dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)]) 
    # 
    # browser()
    
    coverage = get_coverage_data(
      data,
      year=year,
      variable = variable,
      data_items = c('F_SYSTEM', 'URBAN_ID', 'FACILITY_TYPE',  'NHS'))
    
    coverage[,
             required := FACILITY_TYPE %in% c(1,2,4) &
               (F_SYSTEM %in% c(1,2,3,4,5) | ((F_SYSTEM == 6) & (URBAN_ID < 99999)) | !is.na(NHS))]
    
  } else 
    
    
    # aadt_single_unit, aadt_combination --------------------------------------
  
  if(variable %in% c("AADT_SINGLE_UNIT", "AADT_COMBINATION")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    dat.NHS <- data[data_item == "NHS" & year_record == year,]
    
    coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
      coverage_join(
        dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)]) 
    
    coverage[, required := (F_SYSTEM == 1 | !is.na(NHS)) & FACILITY_TYPE %in% c(1,2) |!is.na(expansion_factor)]
    
  } else 
    
    
    # access_control -----------------------------------------------------------
  
  if(variable == "ACCESS_CONTROL"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    dat.NHS <- data[data_item == "NHS" & year_record == year,]
    
    coverage = coverage_join(
      dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)],
      dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]) %>%
      coverage_join(dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
      coverage_join(dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)])
    
    setDT(coverage)
    
    coverage[, required := 
               !is.na(expansion_factor) & FACILITY_TYPE %in% c(1,2) & (F_SYSTEM %in% c(1,2,3,4) | !is.na(NHS))]
    
  } else 
    
    
    # base_thickness -----------------------------------------------------------
  
  if(variable %in% c("BASE_THICKNESS")){
    
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    dat.BASE_TYPE <- data[data_item == "BASE_TYPE" & year_record == year,] 
    
    coverage = dat.SURFACE_TYPE[, .(route_id, begin_point, end_point, SURFACE_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)]) %>%
      coverage_join(
        dat.BASE_TYPE[, .(route_id, begin_point, end_point, BASE_TYPE = value_numeric)]
      )
    
    coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE>1 & BASE_TYPE>1]
    
  } else
    
    
    # base_type ---------------------------------------------------------------
  
  if(variable %in% c("BASE_TYPE")){
    
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    
    coverage = dat.SURFACE_TYPE[, .(route_id, begin_point, end_point, SURFACE_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)])
    
    coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE > 1]
    
    
  } else
    
    
    # counter_peak_lanes ------------------------------------------------------
  
  if(variable == "COUNTER_PEAK_LANES"){
    
    # Sample and FACILITY_TYPE = 2 AND (URBAN_ID < 99999 OR THROUGH_LANES >=4)
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.THROUGH_LANES <- data[data_item == "THROUGH_LANES" & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    
    coverage = coverage_join(
      dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)],
      dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]) %>%
      coverage_join(
        dat.THROUGH_LANES[, .(route_id, begin_point, end_point, THROUGH_LANES = value_numeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)])
    
    coverage[, required := !is.na(expansion_factor) & FACILITY_TYPE == 2 & (URBAN_ID < 99999 | THROUGH_LANES >= 4)]
    
  } else
    
    
    # county_code -------------------------------------------------------------
  
  if(variable == "COUNTY_ID"){
    
    # FACILITY_TYPE in (1,2) AND (F_SYSTEM in (1,2,3,4,5) or (F_SYSTEM = 6 and URBAN_ID <99999) or NHS
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    dat.NHS <- data[data_item == "NHS" & year_record == year,]
    
    coverage = coverage_join(
      dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)],
      dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)]) %>%
      coverage_join(
        dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)])
    
    coverage[, required := FACILITY_TYPE %in% c(1,2) &
               (F_SYSTEM %in% c(1,2,3,4,5) | (F_SYSTEM == 6 & URBAN_ID == 99999) | !is.na(NHS))]
    
  } else # end COUNTY_ID
    
    
    # cracking_percent -------------------------------
  
  if(variable %in% c("CRACKING_PERCENT")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    coverage = dat.SURFACE_TYPE[, .(route_id, begin_point, end_point, SURFACE_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)])
    
    coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE %in% 2:10]
    
    
  } else
    
    
    # curves and grades ------------------------------------------------------
  if(variable %like% 'CURVES|GRADES'){
    
    # Calculate fraction of rows with expansion_factors
    score = data[data_item == variable & year_record == year & !is.na(expansion_factor), .N] /
      data[data_item == variable & year_record == year, .N]
    
    return(score)
    
  } else 
    
    
    # f_system ----------------------------------------------------------------
  
  if(variable == "F_SYSTEM"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    
    coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
      .[FACILITY_TYPE %in% c(1, 2, 4)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)])
    
    coverage[, required := TRUE]
    
  } else # end F_SYSTEM
    
    
    # facility_type ------------------------------------------------------------
  
  if(variable == "FACILITY_TYPE"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    dat.NHS <- data[data_item == "NHS" & year_record == year,]
    
    coverage = coverage_join(
      dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)],
      dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)]) %>%
      coverage_join(
        dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)]) 
    
    coverage[, required := (F_SYSTEM %in% c(1,2,3,4,5)|(F_SYSTEM == 6 & URBAN_ID == 99999)|!is.na(NHS))]
    
  } else 
    
    
    # faulting ----------------------------------------------------------------
  
  if(variable %in% c("FAULTING")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    coverage <- sqldf(
      "select 
        A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACE_TYPE, 
        B.value_numeric as variable,B.expansion_factor 
        from [dat.SURFACE_TYPE] A 
        left join [dat.variable] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE %in% c(3,4,9,10)]
    
  } else
    
    # iri ---------------------------------------------------------------------
  
  if(variable %in% c("IRI")){
    
    # IRI ValueNumeric Must Exist Where SURFACE_TYPE >1 AND 
    # ( FACILITY_TYPE IN (1,2) AND 
    #  (
    #         PSR ValueText <> 'A'   AND ( F_SYSTEM in (1,2,3)  OR NHS )
    #         OR 
    #        (Sample and F_SYSTEM = 4 and URBAN_ID = 99999)
    #  ) 
    #  OR DIR_THROUGH_LANES >0
    # )
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable, 
      data_items = c(
        'F_SYSTEM', 'URBAN_ID', 'SURFACE_TYPE', 'DIR_THROUGH_LANES', 
        'FACILITY_TYPE', 'NHS', 'PSR'),
      value_text = 'PSR')
    
    coverage[, required := SURFACE_TYPE > 1 & 
               (DIR_THROUGH_LANES > 0 |
                  (FACILITY_TYPE %in% c(1,2) & (
                    (PSR != 'A' & (F_SYSTEM %in% c(1,2,3) | !is.na(NHS))) |
                      (!is.na(expansion_factor) & F_SYSTEM == 4 & URBAN_ID == 99999)
                  )
                  ) 
               )
    ]
    
    # dat.variable <- data[data_item == variable & year_record == year,]
    # dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    # dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    # dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    # dat.NHS <- data[data_item == "NHS" & year_record == year,]
    # dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    
    # coverage <- sqldf(
    #   "select 
    #       A.route_id, A.begin_point, A.end_point,
    #           A.data_item, A.value_numeric as FACILITY_TYPE, 
    #       B.value_numeric as SURFACE_TYPE,
    #           coalesce(A.expansion_factor,B.expansion_factor) as expansion_factor
    #     from [dat.FACILITY_TYPE] A 
    #     inner join [dat.SURFACE_TYPE] B on 
    #           A.route_id = B.route_id and
    #             A.begin_point = B.begin_point and
    #             A.end_point = B.end_point")
    
    # setDT(coverage)
    # coverage = coverage %>% 
    #   coverage_join(
    #     dat.URBAN_ID[, .(route_id, begin_point, end_point, expansion_factor,
    #       URBAN_ID = value_numeric)]) %>%
    #   coverage_join(
    #     dat.F_SYSTEM[, .(route_id, begin_point, end_point, expansion_factor,
    #       F_SYSTEM = value_numeric)]) %>%
    #   coverage_join(
    #     dat.NHS[, .(route_id, begin_point, end_point, expansion_factor, 
    #       NHS = value_numeric)]) %>%
    #   coverage_join(
    #     dat.variable[, .(route_id, begin_point, end_point, expansion_factor,
    #       variable = value_numeric)])
    
  } else 
    
    # LAST_OVERLAY_THICKNESS -----------------------------------------------------
  
  if ( variable == 'LAST_OVERLAY_THICKNESS'){
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable,
      data_items = c('YEAR_LAST_IMPROVEMENT'))
    
    # dat_variable = data[
    #   data_item == variable & year_record == year,
    #   .(route_id, begin_point, end_point, variable == value_numeric, expansion_factor)]
    
    # dat_YEAR_LAST_CONSTRUCTION = data[
    #   data_item == 'YEAR_LAST_CONSTRUCTION' & year_record == year,
    #   .(route_id, begin_point, end_point, YEAR_LAST_CONSTRUCTION == value_date)]
    
    # coverage = coverage_join(dat_variable, dat_YEAR_LAST_CONSTRUCTION)
    
    coverage[, required := !is.na(expansion_factor) & !is.na(YEAR_LAST_IMPROVEMENT)] 
    
  } else
    
    # maintenance_operations ---------------------------------------------------
  
  if(variable %in% c("MAINTENANCE_OPERATIONS")){
    
    coverage = data[data_item == variable & year_record == year,]
    coverage[, required := TRUE]
    
  } else
    
    # managed_lanes ---------------------------------------------------------------
  
  if(variable == "MANAGED_LANES"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.MANAGED_LANES_TYPE <- data[data_item == "MANAGED_LANES_TYPE" & year_record == year,]
    
    coverage <- sqldf(
      "select 
        A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as MANAGED_LANES_TYPE, 
        B.value_numeric as variable 
        from [dat.MANAGED_LANES_TYPE] A 
        left join [dat.variable] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(MANAGED_LANES_TYPE)]
    
    
  } else # end MANAGED_LANES
    
    
    # managed_lanes_type ----------------------------------------------------------------
  
  if(variable == "MANAGED_LANES_TYPE"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.MANAGED_LANES <- data[data_item == "MANAGED_LANES" & year_record == year,]
    
    coverage <- sqldf(
      "select 
        A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as MANAGED_LANES, 
        B.value_numeric as variable 
        from [dat.MANAGED_LANES] A 
        left join [dat.variable] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(MANAGED_LANES)]
    
  } else # end MANAGED_LANES_TYPE
    
    
    # median_width -------------------------------------------------------------
  
  if(variable == "MEDIAN_WIDTH"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.MEDIAN_TYPE <- data[data_item == "MEDIAN_TYPE" & year_record == year,]
    
    # Change for variable --------------------
    
    coverage = dat.MEDIAN_TYPE[, .(route_id, begin_point, end_point, MEDIAN_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)])
    
    coverage[, required := MEDIAN_TYPE %in% 2:7 & (!is.na(expansion_factor))]
    
  } else
    
    
    # nhs ---------------------------------------------------------------------
  
  if(variable %in% c("NHS")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    
    coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) 
    
    coverage[, required :=  (F_SYSTEM == 1 & FACILITY_TYPE < 4)]
    
  } else
    
    
    # number_signals -----------------------------------------------------------
  
  if(variable %in% c("NUMBER_SIGNALS")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SIGNAL_TYPE <- data[data_item == "SIGNAL_TYPE" & year_record == year,]
    dat.PCT_GREEN_TIME <- data[data_item == "PCT_GREEN_TIME" & year_record == year,]
    
    coverage <- sqldf(
      "select 
        A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SIGNALTYPE , 
        B.value_numeric as variable,B.expansion_factor 
        from [dat.SIGNAL_TYPE] A 
        left join [dat.variable] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        )")
    
    coverage <- sqldf(
      "select 
        A.*, 
        B.value_numeric as PCTGREENTIME 
        from [coverage] A 
        left join [dat.PCT_GREEN_TIME] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        ) ")
    
    
    setDT(coverage)
    
    coverage[, required :=  (!is.na(expansion_factor) & !is.na(PCTGREENTIME)) |(!is.na(expansion_factor) & SIGNALTYPE %in% c(1,2,3,4) )]
    
  } else
    
    
    # ownership ----------------------------------------------------------------
  
  if(variable == "OWNERSHIP"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    dat.NHS <- data[data_item == "NHS" & year_record == year,]
    
    coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)]) %>%
      coverage_join(
        dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)]) 
    
    coverage[, required := FACILITY_TYPE %in% c(1,2) & (F_SYSTEM %in% c(1,2,3,4,5)|((F_SYSTEM == 6) & (URBAN_ID<99999))|!is.na(NHS))]
    
    
  } else
    
    
    # pct_green_time ----------------------------------------------------------
  
  if(variable %in% c("PCT_GREEN_TIME")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    dat.NUMBER_SIGNALS <- data[data_item == "NUMBER_SIGNALS" & year_record == year,]
    
    coverage = dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)]) %>%
      coverage_join(dat.NUMBER_SIGNALS[, .(route_id, begin_point, end_point, NUMBER_SIGNALS = value_numeric)])
    
    coverage[, required := ( URBAN_ID < 99999 & NUMBER_SIGNALS >= 1 & !is.na(expansion_factor) )]
    
  } else
    
    
    # pct_pass_sight -----------------------------------------------------------
  
  if(variable %in% c("PCT_PASS_SIGHT")){
    
    # dat.variable <- data[data_item == variable & year_record == year,]
    # dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    # dat.THROUGH_LANES <- data[data_item == "THROUGH_LANES" & year_record == year,]
    # dat.MEDIAN_TYPE <- data[data_item == "MEDIAN_TYPE" & year_record == year,]
    
    # coverage = dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)] %>%
    #   coverage_join(
    #     dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)]) %>%
    #   coverage_join(
    #     dat.THROUGH_LANES[, .(route_id, begin_point, end_point, THROUGH_LANES = value_numeric)]) %>%
    #   coverage_join(
    #     dat.MEDIAN_TYPE[, .(route_id, begin_point, end_point, MEDIAN_TYPE = value_numeric)])
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      data_items = c('URBAN_ID', 'SURFACE_TYPE', 'THROUGH_LANES', 'MEDIAN_TYPE'))
    
    coverage[, required := (!is.na(expansion_factor) & URBAN_ID == 99999 & THROUGH_LANES == 2 & MEDIAN_TYPE %in% c(1,2) & SURFACE_TYPE > 1)]
    
    
  } else
    
    # peak_parking -------------------------------------------------------------
  
  if(variable == "PEAK_PARKING"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    
    coverage = dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_numeric)])
    
    coverage[, required := URBAN_ID < 99999 & (!is.na(expansion_factor))]
    
  } else
    
    
    # psr ----------------------------------------------------------------------
  
  if(variable %in% c("PSR")){
    
    # PSR ValueNumeric Must Exist Where
    #  IRI ValueNumeric IS NULL AND FACILITY_TYPE IN (1,2) AND SURFACE_TYPE  >1 AND
    #   (Sample AND (
    #     F_SYSTEM in (4,6) AND URBAN_ID <99999  OR   
    #     F_SYSTEM = 5 
    #    ) OR (F_SYSTEM = 1 or NHS) AND PSR ValueText = 'A'
    #   )
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable, 
      data_items = c(
        'F_SYSTEM', 'URBAN_ID', 'FACILITY_TYPE', 'SURFACE_TYPE', 'NHS', 'PSR', 'IRI'
      ),
      value_text = 'PSR')
    
    coverage[, required := (is.na(IRI) & FACILITY_TYPE %in% c(1, 2) & SURFACE_TYPE > 1) &
               (
                 !is.na(expansion_factor) & (
                   ( (F_SYSTEM %in% c(4, 6) & URBAN_ID < 99999) | F_SYSTEM == 5 ) |
                     ( (F_SYSTEM == 1 | NHS) & PSR == 'A' ) 
                 )
               )
    ]
    
    # keep_cols <- c('route_id', 'begin_point', 'end_point', 'value_numeric')
    # keep_cols2 = c(keep_cols, 'expansion_factor')
    # dat.variable <- data[data_item == variable & year_record == year, ..keep_cols2]
    # dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year, ..keep_cols]
    # dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year, ..keep_cols]
    # dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year, ..keep_cols]
    # dat.IRI <- data[data_item == "IRI" & year_record == year & is.na(value_numeric), ..keep_cols]
    # dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year & value_numeric > 1, ..keep_cols] 
    
    # setnames(dat.variable, 'value_numeric', 'variable')
    # setnames(dat.FACILITY_TYPE, 'value_numeric', 'FACILITY_TYPE')
    # setnames(dat.IRI, 'value_numeric', 'IRI')
    # setnames(dat.SURFACE_TYPE, 'value_numeric', 'SURFACE_TYPE')
    # setnames(dat.F_SYSTEM, 'value_numeric', 'F_SYSTEM')
    # setnames(dat.URBAN_ID, 'value_numeric', 'URBAN_ID')
    
    # coverage = dat.variable[!is.na(expansion_factor)] %>%
    #   coverage_join(dat.FACILITY_TYPE[!is.na(FACILITY_TYPE)]) %>%
    #   coverage_join(dat.IRI[is.na(IRI)]) %>%
    #   coverage_join(dat.SURFACE_TYPE) %>%
    #   coverage_join(dat.F_SYSTEM) %>%
    #   coverage_join(dat.URBAN_ID)
    
    # coverage[, required := (is.na(IRI) & !is.na(expansion_factor) & SURFACE_TYPE > 1 & 
    #            ((F_SYSTEM %in% c(4, 5, 6) & URBAN_ID < 99999 & FACILITY_TYPE %in% c(1, 2)) |
    #               (F_SYSTEM == 5 & FACILITY_TYPE %in% c(1, 2) & URBAN_ID == 99999))]
    
  } else
    
    
    # route_number ------------------------------------------------------------
  
  if(variable %in% c("ROUTE_NUMBER")){
    
    # Either ValueNumeric or ValueText of ROUTE_NUMBER Must Exist where
    # (F_SYSTEM in (1,2,3,4) or NHS) and FACILITY_TYPE (1,2) and ROUTE_SIGNING in (2,3,4,5,6,7,8,9)  OR
    # F_SYSTEM=1 AND FACILITY_TYPE=6 AND DIR_THROUGH_LANES > 0 AND (IRI IS NOT NULL OR PSR IS NOT NULL)
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.variable[, value_text := as.character(value_text)]
    
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    dat.ROUTE_SIGNING <- data[data_item == "ROUTE_SIGNING" & year_record == year,]
    dat.DIR_THROUGH_LANES = data[data_item == 'DIR_THROUGH_LANES' & year_record == year, ]
    dat.NHS <- data[data_item == "NHS" & year_record == year,]
    dat.IRI <- data[data_item == 'IRI' & year_record == year,]
    dat.PSR <- data[data_item == 'PSR' & year_record == year,]
    
    # NOTE: coalesce used here
    
    coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, expansion_factor, FACILITY_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor,
                         variable = fcoalesce(as.character(value_text), as.character(value_numeric)))]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
      coverage_join(
        dat.ROUTE_SIGNING[, .(route_id, begin_point, end_point, ROUTE_SIGNING = value_numeric)]) %>%
      coverage_join(
        dat.DIR_THROUGH_LANES[, .(route_id, begin_point, end_point, DIR_THROUGH_LANES = value_numeric)]) %>%
      coverage_join(
        dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)])  %>%
      coverage_join(
        dat.PSR[, .(route_id, begin_point, end_point, IRI = value_numeric)]) %>%
      coverage_join(
        dat.IRI[, .(route_id, begin_point, end_point, PSR = value_numeric)])
    
    
    coverage[,
             required := ((F_SYSTEM %in% c(1,2,3,4) | !is.na(NHS)) & FACILITY_TYPE %in% 1:2 & ROUTE_SIGNING %in% 2:9) |
               (F_SYSTEM == 1 & FACILITY_TYPE == 6 & DIR_THROUGH_LANES > 0 & (!is.na(IRI) | !is.na(PSR) ))]
    
    
  } else 
    
    # route_signing route_qualifier --------------------------------------------
  
  if(variable %in% c("ROUTE_SIGNING","ROUTE_QUALIFIER")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    dat.NHS <- data[data_item == "NHS" & year_record == year,]
    
    coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
      coverage_join(
        dat.NHS[, .(route_id, begin_point, end_point, NHS = value_numeric)])
    
    coverage[, required := FACILITY_TYPE %in% c(1,2) & (F_SYSTEM %in% c(1,2,3,4)|!is.na(NHS))]
    
    
  } else
    
    
    # rutting -----------------------------------------------------------------
  
  if(variable %in% c("RUTTING")){
    
    # dat.variable <- data[data_item == variable & year_record == year,]
    # dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    # coverage = dat.SURFACE_TYPE[, .(route_id, begin_point, end_point, SURFACE_TYPE = value_numeric)] %>%
    #   coverage_join(dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)])
    
    # coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE%in%c(2,6,7,8)]
    
    # ----------------
    
    # RUTTING ValueNumeric Must Exist Where (SURFACE_TYPE in (2,6,7,8)) AND (
    #   (FACILITY_TYPE in (1,2) AND (F_SYSTEM = 1 OR NHS OR Sample)) OR 
    #   (DIR_THROUGH_LANES >0 AND (IRI IS NOT NULL OR PSR IS NOT NULL))
    #   )
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      data_items = c(
        'F_SYSTEM', 'SURFACE_TYPE', 'FACILITY_TYPE', 'DIR_THROUGH_LANES', 'NHS', 'IRI', 'PSR'))
    
    coverage[, required := 
               (SURFACE_TYPE %in% c(2,6, 7, 8)) & (
                 (FACILITY_TYPE %in% c(1, 2) & (F_SYSTEM == 1 | NHS | !is.na(expansion_factor))) |
                   (DIR_THROUGH_LANES > 0 & ( !is.na(IRI) | !is.na(PSR) ))
               )
    ]
    
  } else
    
    
    # shoulder_width_l --------------------------------------------------------
  
  if(variable == "SHOULDER_WIDTH_L"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SHOULDER_TYPE <- data[data_item == "SHOULDER_TYPE" & year_record == year,]
    dat.MEDIAN_TYPE <- data[data_item == "MEDIAN_TYPE" & year_record == year,]
    
    coverage = coverage_join(
      dat.SHOULDER_TYPE[, .(route_id, begin_point, end_point, SHOULDER_TYPE = value_numeric)],
      dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]) %>%
      coverage_join(
        dat.MEDIAN_TYPE[, .(route_id, begin_point, end_point, MEDIAN_TYPE = value_numeric)])
    
    coverage[, required := SHOULDER_TYPE %in% 2:6 & MEDIAN_TYPE %in% 2:7 & (!is.na(expansion_factor))]
    
    
  } else
    
    # shoulder_width_r ---------------------------------------------------------
  
  if(variable == "SHOULDER_WIDTH_R"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SHOULDER_TYPE <- data[data_item == "SHOULDER_TYPE" & year_record == year,]
    
    coverage = dat.SHOULDER_TYPE[, .(route_id, begin_point, end_point, SHOULDER_TYPE = value_numeric)] %>%
      coverage_join(dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)])
    
    coverage[, required := SHOULDER_TYPE %in% 2:6 & (!is.na(expansion_factor))]
    
  } else
    
    # signal_type -------------------------------------------------------------
  
  if(variable %in% c("SIGNAL_TYPE")){
    
    # dat.variable <- data[data_item == variable & year_record == year,]
    # dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    # dat.ACCESS_CONTROL <- data[data_item == "ACCESS_CONTROL" & year_record == year,]
    
    # coverage = dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)] %>%
    #   coverage_join(dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]) %>%
    #   coverage_join(dat.ACCESS_CONTROL[, .(route_id, begin_point, end_point, ACCESS_CONTROL = value_numeric)])
    
    # coverage[, required :=  (URBAN_ID != 99999 & ACCESS_CONTROL == 1 & !is.na(expansion_factor))]
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      data_items = c('URBAN_ID', 'NUMBER_SIGNALS')
    )
    
    coverage[, required := !is.na(expansion_factor) & URBAN_ID != 99999 & NUMBER_SIGNALS >= 1]
    
  } else 
    
    # speed_limit -------------------------------------------------------------
  
  if ( variable == 'SPEED_LIMIT' ){
    
    dat.variable = data[
      data_item == variable & year_record == year, 
      .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]
    
    dat.NHS = data[
      data_item == 'NHS' & year_record == year, 
      .(route_id, begin_point, end_point, NHS = value_numeric)]
    
    coverage = dat.variable %>% 
      coverage_join(dat.NHS)
    
    coverage[, required := !is.na(NHS) & !is.na(expansion_factor)]
    
    
  } else 
    
    # surface_type ------------------------------------------------------------
  
  if ( variable == 'SURFACE_TYPE' ) {
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      data_items = c(
        'F_SYSTEM', 'FACILITY_TYPE', 'NHS', 'DIR_THROUGH_LANES', 'IRI', 'PSR'))
    
    coverage[, required := 
               ( FACILITY_TYPE %in% c(1, 2) & (F_SYSTEM == 1 | !is.na(NHS) | !is.na(expansion_factor)) ) |
               ( DIR_THROUGH_LANES > 0 & ( !is.na(IRI) | !is.na(PSR) ) )
    ]
    
  } else
    
    # terrain_type ------------------------------------------------------------
  
  if(variable %in% c("TERRAIN_TYPE")){
    
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      data_items = c('F_SYSTEM', 'URBAN_ID'))
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,] 
    
    coverage2 = dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)] %>%
      coverage_join(dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)])
    
    coverage[, required := !is.na(expansion_factor) & URBAN_ID == 99999 & F_SYSTEM %in% 1:5]
    
  } else
    
    
    # thickness_flexible ------------------------------------------------------
  
  if(variable %in% c("THICKNESS_FLEXIBLE")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    coverage <- sqldf(
      "select 
        A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACE_TYPE, 
        B.value_numeric as variable,B.expansion_factor 
        from [dat.SURFACE_TYPE] A 
        left join [dat.variable] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE %in% c(2,6,7,8)]
    
  } else
    
    
    # thickness_rigid ---------------------------------------------------------
  
  if(variable %in% c("THICKNESS_RIGID")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    coverage <- sqldf(
      "select 
        A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACE_TYPE, 
        B.value_numeric as variable,B.expansion_factor 
        from [dat.SURFACE_TYPE] A 
        left join [dat.variable] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE %in% 3:10]
    
  } else 
    
    # toll_ID ------------------------------------------------------------
  
  if(variable == "TOLL_ID"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    
    coverage <- dat.variable
    
    coverage[, required := !is.na(variable)]
    
  } else # end TOLL_ID
    
    # toll_type ---------------------------------------------------------------
  
  if(variable == "TOLL_TYPE"){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.TOLL_ID <- data[data_item == "TOLL_ID" & year_record == year,]
    
    coverage <- sqldf(
      "select 
        A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as TOLL_ID, 
        B.value_numeric as variable 
        from [dat.TOLL_ID] A 
        left join [dat.variable] B on 
        A.route_id = B.route_id and (
        ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
        ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
        )")
    
    setDT(coverage)
    coverage[, required := TRUE]
    
  } else # end TOLL_TYPE
    
    # turn_lanes_r, turn_lanes_l ----------------------------------------------
  
  if(variable %in% c("TURN_LANES_R","TURN_LANES_L")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.URBAN_ID <- data[data_item == "URBAN_ID" & year_record == year,]
    dat.ACCESS_CONTROL <- data[data_item == "ACCESS_CONTROL" & year_record == year,]
    
    coverage = dat.URBAN_ID[, .(route_id, begin_point, end_point, URBAN_ID = value_numeric)] %>%
      coverage_join(dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric, expansion_factor)]) %>%
      coverage_join(dat.ACCESS_CONTROL[, .(route_id, begin_point, end_point, ACCESS_CONTROL = value_numeric)])
    
    coverage[, required := URBAN_ID < 99999 & ACCESS_CONTROL > 1 & (!is.na(expansion_factor))]
    
  } else
    
    
    # urban_id ---------------------------------------------------------------
  
  if(variable == "URBAN_ID"){
    
    # dat.variable <- data[data_item == variable & year_record == year,]
    # dat.DIR_THROUGH_LANES = data[data_item == 'DIR_THROUGH_LANES' & year_record == year, ]
    # dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year,]
    # dat.F_SYSTEM <- data[data_item == "F_SYSTEM" & year_record == year,]
    # dat.IRI <- data[data_item == 'IRI' & year_record == year,]
    # dat.PSR <- data[data_item == 'PSR' & year_record == year,]
    # 
    # coverage = dat.FACILITY_TYPE[, .(route_id, begin_point, end_point, FACILITY_TYPE = value_numeric)] %>%
    #   coverage_join(
    #     dat.variable[, .(route_id, begin_point, end_point, variable = value_numeric)]) %>%
    #   coverage_join(
    #     dat.F_SYSTEM[, .(route_id, begin_point, end_point, F_SYSTEM = value_numeric)]) %>%
    #   coverage_join(
    #     dat.DIR_THROUGH_LANES[, .(route_id, begin_point, end_point, DIR_THROUGH_LANES = value_numeric)]) %>%
    #   coverage_join(
    #     dat.PSR[, .(route_id, begin_point, end_point, IRI = value_numeric)]) %>%
    #   coverage_join(
    #     dat.IRI[, .(route_id, begin_point, end_point, PSR = value_numeric)])
    
    coverage = get_coverage_data(
      data = data,
      year = year,
      variable = variable,
      data_items = c('F_SYSTEM', 'FACILITY_TYPE', 'DIR_THROUGH_LANES', 'IRI', 'PSR')
    )
    
    coverage[, required :=
               (FACILITY_TYPE %in% c(1,2,4) & (F_SYSTEM %in% 1:7)) |
               (FACILITY_TYPE == 6 & DIR_THROUGH_LANES > 0 & F_SYSTEM == 1 & (!is.na(IRI) | !is.na(PSR)))]
    
    
  } else
    
    # YEAR_LAST_CONSTRUCTION -------------------------------------------------
  
  if(variable %in% c("YEAR_LAST_CONSTRUCTION")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,] 
    
    coverage = dat.SURFACE_TYPE[, .(route_id, begin_point, end_point, SURFACE_TYPE = value_numeric)] %>%
      coverage_join(
        dat.variable[, .(route_id, begin_point, end_point, expansion_factor, variable = value_date)])
    
    coverage[, required := !is.na(expansion_factor) & SURFACE_TYPE %in% 2:10]
    
    
  } else
    
    # Year_last_improv ---------------------------------------------------------
  
  if(variable %in% c("YEAR_LAST_IMPROVEMENT")){
    
    dat.variable <- data[data_item == variable & year_record == year,]
    dat.SURFACE_TYPE <- data[data_item == "SURFACE_TYPE" & year_record == year,]
    dat.YEAR_LAST_CONSTRUCTION <- data[data_item == "YEAR_LAST_CONSTRUCTION" & year_record == year,]
    
    coverage = dat.SURFACE_TYPE[, .(route_id, begin_point, end_point, SURFACE_TYPE = value_numeric)] %>%
      coverage_join(dat.variable[, .(route_id, begin_point, end_point, variable = value_date, expansion_factor)]) %>%
      coverage_join(dat.YEAR_LAST_CONSTRUCTION[, .(route_id, begin_point, end_point, YEAR_LAST_CONSTRUCTION = value_date)])
    
    coverage[, required := (!is.na(expansion_factor) & SURFACE_TYPE %in% 2:10) ]
    
  }
  
  # Score calculation ========================================================= 
  
  coverage[is.na(required), required := FALSE]
  
  if ( coverage[, sum(required)] == 0 ){
    score = 1
  } else {
    score = coverage[, sum(required & !is.na(variable))] /
      coverage[, sum(required)]
  }
  
  return(score)
}


