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
  
  join_cols = c('routeid', 'beginpoint', 'endpoint')
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
  
  if ( 'i.expansionfactor' %in% names(ab) ){
    
    # coalesce(expansionfactor, i.expansionfactor)
    ab[is.na(expansionfactor), expansionfactor := i.expansionfactor]
    ab[, i.expansionfactor := NULL]
  }
  
  ab[, (c('beginpoint', 'endpoint')) := NULL]
  setnames(ab, c('i.beginpoint', 'i.endpoint'), c('beginpoint', 'endpoint'))
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
  # ba[, (c('beginpoint', 'endpoint')) := NULL]
  # setnames(ba, c('i.beginpoint', 'i.endpoint'), c('beginpoint', 'endpoint'))
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
  # abba[expansionfactor != expansionfactor_tmp]
  
  return(ab)
  
}

get_coverage_data = function(
  data,
  year,
  variable,
  dataitems,
  valuedate = c('YEAR_LAST_IMPROVEMENT', 'YEAR_LAST_CONSTRUCTION'),
  valuetext = c()){
  
  coverage_list = list()
  
  # List cols to keep
  
  if ( variable %in% valuedate ){
    value_var = 'valuedate'
    # } else if ( variable %in% valuetext ){  # Assume target variable is not valuetext
    #   value_var = 'valuetext'
  } else {
    value_var = 'valuenumeric'
  }
  
  keep_cols = c('routeid', 'beginpoint', 'endpoint', value_var)
  
  variable_dt = data[
    dataitem == variable & datayear == year, 
    c(keep_cols, 'expansionfactor'), with = FALSE]
  
  setnames(variable_dt, value_var, 'variable')
  
  # Get all the other data items
  for ( di in dataitems ){
    
    # List cols to keep
    if ( di %in% valuedate ){
      value_var = 'valuedate'
    } else if ( di %in% valuetext ){
      value_var = 'valuetext'
    } else {
      value_var = 'valuenumeric'
    }
    
    keep_cols = c('routeid', 'beginpoint', 'endpoint', value_var)
    
    di_dt = data[dataitem == di & datayear == year, keep_cols, with = FALSE]
    
    setnames(di_dt, value_var, di)
    
    coverage_list[[di]] = di_dt
  }
  
  # Join each data set on routeid, beginpoint, endpoint
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
  if ( data[dataitem == variable & datayear == year, .N] == 0 ){
    score = 0
    return(score)
  } else
    
    # complete if present ------------------------------------------------------
  # these variables just need to have something to be complete
  
  if( (variable %in% c(
    "STRUCTURE_TYPE", "STRAHNET_TYPE","MAINTENANCE_OPERATIONS", "NN"))){
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
    
    # Calculate fraction of rows with expansionfactors
    score = data[dataitem == variable & datayear == year & !is.na(expansionfactor), .N] /
      data[dataitem == variable & datayear == year, .N]
    
    return(score)
    
  } else
    
    # through_lanes, aadt -----------------------------------------------------
  
  if(variable %in% c("AADT", "THROUGH_LANES")){
    
    # FACILITY_TYPE in (1,2,4) AND (F_SYSTEM in (1,2,3,4,5) or (F_SYSTEM = 6 and URBAN_ID  <99999) or NHS)
    
    # dat.variable <- data[dataitem == variable & datayear == year,]
    # dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    # dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    # dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    # dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    # 
    # 
    # coverage = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)] %>%
    #   coverage_join(
    #     dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric)]) %>%
    #   coverage_join(
    #     dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
    #   coverage_join(
    #     dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)]) %>%
    #   coverage_join(
    #     dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)]) 
    # 
    #browser()
    
    coverage = get_coverage_data(
      data,
      year=year,
      variable = variable,
      dataitems = c('F_SYSTEM', 'URBAN_ID', 'FACILITY_TYPE',  'NHS'))
    
    coverage[,
             required := FACILITY_TYPE %in% c(1,2,4) &
               (F_SYSTEM %in% c(1,2,3,4,5) | ((F_SYSTEM == 6) & (URBAN_ID < 99999)) | !is.na(NHS))]
    
  } else 
    
    
    # aadt_single_unit, aadt_combination --------------------------------------
  
  if(variable %in% c("AADT_SINGLE_UNIT", "AADT_COMBINATION")){
    #browser()
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    
    # HPMS9
    data.AADT = data[dataitem == 'AADT' & datayear == year, ]
    coverage = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
      coverage_join(
        dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)]) 
    
    coverage[, required := (F_SYSTEM == 1 | !is.na(NHS)) & FACILITY_TYPE %in% c(1,2) |!is.na(expansionfactor)]
    
  } else 
    
    
    # access_control -----------------------------------------------------------
  
  if(variable == "ACCESS_CONTROL"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    
    coverage = coverage_join(
      dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)],
      dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)]) %>%
      coverage_join(dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
      coverage_join(dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)])
    
    setDT(coverage)
    
    coverage[, required := #is.na(variable) & 
                           FACILITY_TYPE %in% c(1,2) & 
                            (F_SYSTEM %in% c(1,2,3) | !is.na(NHS) | !is.na(expansionfactor))]
    # coverage[, required := 
    #            is.na(expansionfactor) & FACILITY_TYPE %in% c(1,2) & (F_SYSTEM %in% c(1,2,3,4) | !is.na(NHS))]
    
  } else 
    
    
    # base_thickness -----------------------------------------------------------
  
  if(variable %in% c("BASE_THICKNESS")){
    
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    dat.BASE_TYPE <- data[dataitem == "BASE_TYPE" & datayear == year,] 
    
    coverage = dat.SURFACE_TYPE[, .(routeid, beginpoint, endpoint, SURFACE_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)]) %>%
      coverage_join(
        dat.BASE_TYPE[, .(routeid, beginpoint, endpoint, BASE_TYPE = valuenumeric)]
      )
    
    coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE>1 & BASE_TYPE>1]
    
  } else
    
    
    # base_type ---------------------------------------------------------------
  
  if(variable %in% c("BASE_TYPE")){
    
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    
    
    coverage = dat.SURFACE_TYPE[, .(routeid, beginpoint, endpoint, SURFACE_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)])
    
    coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE > 1]
    
    
  } else
    
    
    # counter_peak_lanes ------------------------------------------------------
  
  if(variable == "COUNTER_PEAK_LANES"){
    
    # Sample and FACILITY_TYPE = 2 AND (URBAN_ID < 99999 OR THROUGH_LANES >=4)
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.THROUGH_LANES <- data[dataitem == "THROUGH_LANES" & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    
    coverage = coverage_join(
      dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)],
      dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)]) %>%
      coverage_join(
        dat.THROUGH_LANES[, .(routeid, beginpoint, endpoint, THROUGH_LANES = valuenumeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)])
    
    coverage[, required := !is.na(expansionfactor) & FACILITY_TYPE == 2 & (URBAN_ID < 99999 | THROUGH_LANES >= 4)]
    
  } else
    
    
    # county_code -------------------------------------------------------------
  
  if(variable == "COUNTY_ID"){
    
    # FACILITY_TYPE in (1,2) AND (F_SYSTEM in (1,2,3,4,5) or (F_SYSTEM = 6 and URBAN_ID <99999) or NHS
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    
    coverage = coverage_join(
      dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)],
      dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)]) %>%
      coverage_join(
        dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)])
    
    coverage[, required := FACILITY_TYPE %in% c(1,2) &
               (F_SYSTEM %in% c(1,2,3,4,5) | (F_SYSTEM == 6 & URBAN_ID == 99999) | !is.na(NHS))]
    
  } else # end COUNTY_ID
    
    
    # cracking_percent -------------------------------
  
  if(variable %in% c("CRACKING_PERCENT")){
    
    # dat.variable <- data[dataitem == variable & datayear == year,]
    # dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    # 
    # coverage = dat.SURFACE_TYPE[, .(routeid, beginpoint, endpoint, SURFACE_TYPE = valuenumeric)] %>%
    #   coverage_join(
    #     dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric # )]) 
    #                      # HPMS updates
    #                      , FACILITY_TYPE, F_SYSTEM, NHS
    #                      )])
    # 
    # coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE %in% 2:10]
    
    # HPMS9
    
    coverage = get_coverage_data(
      data,
      year      = year,
      variable  = variable,
      dataitems = c(
        'SURFACE_TYPE', 'FACILITY_TYPE', 'F_SYSTEM', 'NHS', 'DIR_THROUGH_LANES', 'IRI', 'PSR'
      ),
      valuetext = 'PSR'
    )
    
    
    coverage[, 
       required := #!is.na(variable) & 
                  SURFACE_TYPE %in% 2:10 & 
                   ( FACILITY_TYPE %in% 1:2 &  
                     ( F_SYSTEM == 1 | !is.na(NHS) | !is.na(expansionfactor) |
                        ( DIR_THROUGH_LANES > 0 & 
                          ( !is.na(IRI) | !is.na(PSR) ) ) )
                   )
    ]
    
    
  } else
    
    
    # curves and grades ------------------------------------------------------
  if(variable %like% 'CURVES|GRADES'){

    # HPMS 9: each Sample needs at least one CURVES_A-F and at least one GRADES_A-F
    data_items = paste0(gsub("_.","", variable), c('_A', '_B', '_C', '_D', '_E', '_F'))
    
    coverage = get_coverage_data(
      data,
      year      = year,
      variable  = variable,
      dataitems = c(data_items, 'F_SYSTEM', 'URBAN_ID', 'SURFACE_TYPE') 
    )

    
    coverage[, 
      (paste0(data_items,'_exists')) := lapply(data_items, function(x) {!is.na(get(x))})
    ]
    
    row_any = function(dt, cols) {
      return(dt[, Reduce(`|`, .SD), .SDcols=cols])
    }
    
    coverage[, one_of_A_F := row_any(coverage, paste0(data_items,'_exists'))]
    
    coverage[, required := one_of_A_F == TRUE &
               !is.na(expansionfactor) & 
               ( F_SYSTEM %in% c(1,2,3) | 
                   (F_SYSTEM == 4 & URBAN_ID == 99999 & SURFACE_TYPE > 1) )]
    
    # Calculate fraction of rows with expansionfactors
    # score = data[dataitem == variable & datayear == year & !is.na(expansionfactor), .N] /
    #   data[dataitem == variable & datayear == year, .N]
    # 
    # return(score)
    
  } else 
    
    if(variable %in% c("DIR_THROUGH_LANES")){

      coverage = get_coverage_data(
        data,
        year      = year,
        variable  = variable,
        dataitems = c(
          'DIR_THROUGH_LANES', 'SURFACE_TYPE', 'FACILITY_TYPE', 'F_SYSTEM', 'NHS', 'IRI', 'PSR'
        ),
        valuetext = 'PSR'
      )
      
      
      coverage[, required := ( F_SYSTEM == 1 ) & (FACILITY_TYPE == 6) & ( IRI > 0 | PSR > 0 ) ] 
      
      
    } else
    
    # f_system ----------------------------------------------------------------
  
  if(variable == "F_SYSTEM"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    
    coverage = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)] %>%
      # .[FACILITY_TYPE %in% c(1, 2, 4)] %>%
      .[FACILITY_TYPE %in% c(1, 2, 4, 5, 6)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric)])
    
    #coverage[, required := TRUE]
    
    coverage[ , required := FACILITY_TYPE %in% c(1,2,4,5,6)]  
    
  } else # end F_SYSTEM
    
    
    # facility_type ------------------------------------------------------------
  
  if(variable == "FACILITY_TYPE"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    
    coverage = coverage_join(
      dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric)],
      dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)]) %>%
      coverage_join(
        dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)]) 
    
    # coverage[, required := (F_SYSTEM %in% c(1,2,3,4,5)|(F_SYSTEM == 6 & URBAN_ID == 99999)|!is.na(NHS))]
    coverage[, required := (F_SYSTEM %in% c(1:7))]
    
  } else 
    
    
    # faulting ----------------------------------------------------------------
  
  if(variable %in% c("FAULTING")){
    
    # HPMS9
    
    coverage = get_coverage_data(
      data,
      year      = year,
      variable  = variable,
      dataitems = c(
        'SURFACE_TYPE', 'FACILITY_TYPE', 'F_SYSTEM', 'NHS', 'DIR_THROUGH_LANES', 'IRI', 'PSR'
      ),
      valuetext = 'PSR'
    )
    
    
    coverage[, 
             required := SURFACE_TYPE %in% c(3,4,9,10) & 
               ( FACILITY_TYPE %in% 1:2 &  
                   ( F_SYSTEM == 1 | !is.na(NHS) | !is.na(expansionfactor) |
                       ( DIR_THROUGH_LANES > 0 & 
                           ( !is.na(IRI) | !is.na(PSR) ) ) )
               )
    ]
    
    # dat.variable <- data[dataitem == variable & datayear == year,]
    # dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    # 
    # coverage <- sqldf(
    #   "select 
    #     A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as SURFACE_TYPE, 
    #     B.valuenumeric as variable,B.expansionfactor 
    #     from [dat.SURFACE_TYPE] A 
    #     left join [dat.variable] B on 
    #     A.routeid = B.routeid and (
    #     ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
    #     ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
    #     )")
    # 
    # setDT(coverage)
    # 
    # coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE %in% c(3,4,9,10)]
    
  } else
    
    # iri ---------------------------------------------------------------------
  
  if(variable %in% c("IRI")){
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable, 
      dataitems = c(
        'F_SYSTEM', 'URBAN_ID', 'SURFACE_TYPE', 'DIR_THROUGH_LANES', 
        'FACILITY_TYPE', 'NHS', 'PSR'),
      valuetext = 'PSR')
    
    coverage[, required := SURFACE_TYPE > 1 & 
               (DIR_THROUGH_LANES > 0 |
                  (FACILITY_TYPE %in% c(1,2) & (
                    (PSR != 'A' & (F_SYSTEM %in% c(1,2,3) | NHS != 1)) |
                      (!is.na(expansionfactor) & F_SYSTEM == 4 & URBAN_ID == 99999))
                    
                    # (PSR != 'A' & (F_SYSTEM %in% c(1,2,3) | !is.na(NHS))) |
                    #   (!is.na(expansionfactor) & F_SYSTEM == 4 & URBAN_ID == 99999)
                  
                  ) 
               )
    ]
    
  } else 
    
    # LAST_OVERLAY_THICKNESS -----------------------------------------------------
  
  if ( variable == 'LAST_OVERLAY_THICKNESS'){
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable,
      dataitems = c('YEAR_LAST_IMPROVEMENT'))
    
    # dat_variable = data[
    #   dataitem == variable & datayear == year,
    #   .(routeid, beginpoint, endpoint, variable == valuenumeric, expansionfactor)]
    
    # dat_YEAR_LAST_CONSTRUCTION = data[
    #   dataitem == 'YEAR_LAST_CONSTRUCTION' & datayear == year,
    #   .(routeid, beginpoint, endpoint, YEAR_LAST_CONSTRUCTION == valuedate)]
    
    # coverage = coverage_join(dat_variable, dat_YEAR_LAST_CONSTRUCTION)
    
    coverage[, required := !is.na(expansionfactor) & !is.na(YEAR_LAST_IMPROVEMENT)] 
    
  } else
    
    # maintenance_operations ---------------------------------------------------

  # if(variable %in% c("MAINTENANCE_OPERATIONS")){
  # 
  #   coverage = data[dataitem == variable & datayear == year,]
  #   coverage[, required := FALSE]
  #   #browser()
  # 
  # } else
    
    # managed_lanes ---------------------------------------------------------------
  
  if(variable == "MANAGED_LANES"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.MANAGED_LANES_TYPE <- data[dataitem == "MANAGED_LANES_TYPE" & datayear == year,]
    
    coverage <- sqldf(
      "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as MANAGED_LANES_TYPE, 
        B.valuenumeric as variable 
        from [dat.MANAGED_LANES_TYPE] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(MANAGED_LANES_TYPE)]
    
    
  } else # end MANAGED_LANES
    
    
    # managed_lanes_type ----------------------------------------------------------------
  
  if(variable == "MANAGED_LANES_TYPE"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.MANAGED_LANES <- data[dataitem == "MANAGED_LANES" & datayear == year,]
    
    coverage <- sqldf(
      "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as MANAGED_LANES, 
        B.valuenumeric as variable 
        from [dat.MANAGED_LANES] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(MANAGED_LANES)]
    
  } else # end MANAGED_LANES_TYPE
    
    
    # median_width -------------------------------------------------------------
  
  if(variable == "MEDIAN_WIDTH"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.MEDIAN_TYPE <- data[dataitem == "MEDIAN_TYPE" & datayear == year,]
    
    # Change for variable --------------------
    
    coverage = dat.MEDIAN_TYPE[, .(routeid, beginpoint, endpoint, MEDIAN_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)])
    
    coverage[, required := MEDIAN_TYPE %in% 2:7 & (!is.na(expansionfactor))]
    
  } else
    
    
    # nhs ---------------------------------------------------------------------
  
  if(variable %in% c("NHS")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    
    coverage = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) 
    
    # coverage[, required :=  (F_SYSTEM == 1 & FACILITY_TYPE < 4)]
    coverage[, required :=  (F_SYSTEM == 1 & FACILITY_TYPE %in% c(1,2,6))]
    
  } else
    
    
    # number_signals -----------------------------------------------------------
  
  if(variable %in% c("NUMBER_SIGNALS")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SIGNAL_TYPE <- data[dataitem == "SIGNAL_TYPE" & datayear == year,]
    dat.PCT_GREEN_TIME <- data[dataitem == "PCT_GREEN_TIME" & datayear == year,]
    
    coverage <- sqldf(
      "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as SIGNALTYPE , 
        B.valuenumeric as variable,B.expansionfactor 
        from [dat.SIGNAL_TYPE] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")
    
    coverage <- sqldf(
      "select 
        A.*, 
        B.valuenumeric as PCTGREENTIME 
        from [coverage] A 
        left join [dat.PCT_GREEN_TIME] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        ) ")
    
    
    setDT(coverage)
    #browser()
    
    coverage[, required := (!is.na(expansionfactor) & SIGNALTYPE %in% c(1,2,3,4) )]
    
    # coverage[, required :=  (!is.na(expansionfactor) & !is.na(PCTGREENTIME)) |(!is.na(expansionfactor) & SIGNALTYPE %in% c(1,2,3,4) )]
    
  } else
    
    
    # ownership ----------------------------------------------------------------
  
  if(variable == "OWNERSHIP"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    
    coverage = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
      coverage_join(
        dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)]) %>%
      coverage_join(
        dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)]) 
    
    # coverage[, required := FACILITY_TYPE %in% c(1,2) & (F_SYSTEM %in% c(1,2,3,4,5)|((F_SYSTEM == 6) & (URBAN_ID<99999))|!is.na(NHS))]
    coverage[, required := FACILITY_TYPE %in% c(1,2,5,6) & F_SYSTEM %in% 1:7]
    
  } else
    
    
    # pct_green_time ----------------------------------------------------------
  
  if(variable %in% c("PCT_GREEN_TIME")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    dat.NUMBER_SIGNALS <- data[dataitem == "NUMBER_SIGNALS" & datayear == year,]
    
    coverage = dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)]) %>%
      coverage_join(dat.NUMBER_SIGNALS[, .(routeid, beginpoint, endpoint, NUMBER_SIGNALS = valuenumeric)])
    
    coverage[, required := ( URBAN_ID < 99999 & NUMBER_SIGNALS >= 1 & !is.na(expansionfactor) )]
    
  } else
    
    
    # pct_pass_sight -----------------------------------------------------------
  
  if(variable %in% c("PCT_PASS_SIGHT")){
    
    # dat.variable <- data[dataitem == variable & datayear == year,]
    # dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    # dat.THROUGH_LANES <- data[dataitem == "THROUGH_LANES" & datayear == year,]
    # dat.MEDIAN_TYPE <- data[dataitem == "MEDIAN_TYPE" & datayear == year,]
    
    # coverage = dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)] %>%
    #   coverage_join(
    #     dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)]) %>%
    #   coverage_join(
    #     dat.THROUGH_LANES[, .(routeid, beginpoint, endpoint, THROUGH_LANES = valuenumeric)]) %>%
    #   coverage_join(
    #     dat.MEDIAN_TYPE[, .(routeid, beginpoint, endpoint, MEDIAN_TYPE = valuenumeric)])
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      dataitems = c('URBAN_ID', 'SURFACE_TYPE', 'THROUGH_LANES', 'MEDIAN_TYPE'))
    
    # coverage[, required := (!is.na(expansionfactor) & URBAN_ID == 99999 & THROUGH_LANES == 2 & MEDIAN_TYPE %in% c(1,2) & SURFACE_TYPE > 1)]
    coverage[, required := (!is.na(expansionfactor) & URBAN_ID == 99999 & THROUGH_LANES == 2 & MEDIAN_TYPE %in% c(1,2) & SURFACE_TYPE > 1)]
    
    
  } else
    
    # peak_parking -------------------------------------------------------------
  
  if(variable == "PEAK_PARKING"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    
    coverage = dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuenumeric)])
    
    coverage[, required := URBAN_ID < 99999 & (!is.na(expansionfactor))]
    
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
      dataitems = c(
        'F_SYSTEM', 'URBAN_ID', 'FACILITY_TYPE', 'SURFACE_TYPE', 'NHS', 'PSR', 'IRI'
      ),
      valuetext = 'PSR')
    
    coverage[, required := (is.na(IRI) & FACILITY_TYPE %in% c(1, 2) & SURFACE_TYPE > 1) &
               (
                 !is.na(expansionfactor) & (
                   ( (F_SYSTEM %in% c(4, 6) & URBAN_ID < 99999) | F_SYSTEM == 5 ) |
                     ( (F_SYSTEM == 1 | !is.na(NHS)) & PSR == 'A' ) 
                 )
               )
    ]
    
    # keep_cols <- c('routeid', 'beginpoint', 'endpoint', 'valuenumeric')
    # keep_cols2 = c(keep_cols, 'expansionfactor')
    # dat.variable <- data[dataitem == variable & datayear == year, ..keep_cols2]
    # dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year, ..keep_cols]
    # dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year, ..keep_cols]
    # dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year, ..keep_cols]
    # dat.IRI <- data[dataitem == "IRI" & datayear == year & is.na(valuenumeric), ..keep_cols]
    # dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year & valuenumeric > 1, ..keep_cols] 
    
    # setnames(dat.variable, 'valuenumeric', 'variable')
    # setnames(dat.FACILITY_TYPE, 'valuenumeric', 'FACILITY_TYPE')
    # setnames(dat.IRI, 'valuenumeric', 'IRI')
    # setnames(dat.SURFACE_TYPE, 'valuenumeric', 'SURFACE_TYPE')
    # setnames(dat.F_SYSTEM, 'valuenumeric', 'F_SYSTEM')
    # setnames(dat.URBAN_ID, 'valuenumeric', 'URBAN_ID')
    
    # coverage = dat.variable[!is.na(expansionfactor)] %>%
    #   coverage_join(dat.FACILITY_TYPE[!is.na(FACILITY_TYPE)]) %>%
    #   coverage_join(dat.IRI[is.na(IRI)]) %>%
    #   coverage_join(dat.SURFACE_TYPE) %>%
    #   coverage_join(dat.F_SYSTEM) %>%
    #   coverage_join(dat.URBAN_ID)
    
    # coverage[, required := (is.na(IRI) & !is.na(expansionfactor) & SURFACE_TYPE > 1 & 
    #            ((F_SYSTEM %in% c(4, 5, 6) & URBAN_ID < 99999 & FACILITY_TYPE %in% c(1, 2)) |
    #               (F_SYSTEM == 5 & FACILITY_TYPE %in% c(1, 2) & URBAN_ID == 99999))]
    
  } else
    
    if(variable %in% c("ROUTE_NAME")){
      
      # ROUTE_NAME must exist where (F_SYSTEM in (1;2;3;4) or NHS) and FACILITY_TYPE (1;2)
      
      coverage = get_coverage_data(
        data,
        year = year,
        variable = variable, 
        dataitems = c(
          'F_SYSTEM', 'FACILITY_TYPE', 'NHS'
        ),
        valuetext = 'PSR')
      
      coverage[,
               required := ((F_SYSTEM %in% c(1,2,3,4) | !is.na(NHS)) & FACILITY_TYPE %in% 1:2 )]
      
      
    } else 
      
    
    # route_number ------------------------------------------------------------
  
  if(variable %in% c("ROUTE_NUMBER")){
    
    # Either ValueNumeric or ValueText of ROUTE_NUMBER Must Exist where
    # (F_SYSTEM in (1,2,3,4) or NHS) and FACILITY_TYPE (1,2) and ROUTE_SIGNING in (2,3,4,5,6,7,8,9)  OR
    # F_SYSTEM=1 AND FACILITY_TYPE=6 AND DIR_THROUGH_LANES > 0 AND (IRI IS NOT NULL OR PSR IS NOT NULL)
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.variable[, valuetext := as.character(valuetext)]
    
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    dat.ROUTE_SIGNING <- data[dataitem == "ROUTE_SIGNING" & datayear == year,]
    dat.DIR_THROUGH_LANES = data[dataitem == 'DIR_THROUGH_LANES' & datayear == year, ]
    dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    dat.IRI <- data[dataitem == 'IRI' & datayear == year,]
    dat.PSR <- data[dataitem == 'PSR' & datayear == year,]
    
    # NOTE: coalesce used here
    
    coverage = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, expansionfactor, FACILITY_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor,
                         variable = fcoalesce(as.character(valuetext), as.character(valuenumeric)))]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
      coverage_join(
        dat.ROUTE_SIGNING[, .(routeid, beginpoint, endpoint, ROUTE_SIGNING = valuenumeric)]) %>%
      coverage_join(
        dat.DIR_THROUGH_LANES[, .(routeid, beginpoint, endpoint, DIR_THROUGH_LANES = valuenumeric)]) %>%
      coverage_join(
        dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)])  %>%
      coverage_join(
        dat.PSR[, .(routeid, beginpoint, endpoint, IRI = valuenumeric)]) %>%
      coverage_join(
        dat.IRI[, .(routeid, beginpoint, endpoint, PSR = valuenumeric)])
    
    
    coverage[,
             required := ((F_SYSTEM %in% c(1,2,3,4) | !is.na(NHS)) & FACILITY_TYPE %in% 1:2 & ROUTE_SIGNING %in% 2:9) |
               (F_SYSTEM == 1 & FACILITY_TYPE == 6 & DIR_THROUGH_LANES > 0 & (!is.na(IRI) | !is.na(PSR) ))]
    
    
  } else 
    
    # route_signing route_qualifier --------------------------------------------
  
  if(variable %in% c("ROUTE_SIGNING","ROUTE_QUALIFIER")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.FACILITY_TYPE <- data[dataitem == "FACILITY_TYPE" & datayear == year,]
    dat.F_SYSTEM <- data[dataitem == "F_SYSTEM" & datayear == year,]
    dat.NHS <- data[dataitem == "NHS" & datayear == year,]
    
    coverage = dat.FACILITY_TYPE[, .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric)]) %>%
      coverage_join(
        dat.F_SYSTEM[, .(routeid, beginpoint, endpoint, F_SYSTEM = valuenumeric)]) %>%
      coverage_join(
        dat.NHS[, .(routeid, beginpoint, endpoint, NHS = valuenumeric)])
    
    coverage[, required := FACILITY_TYPE %in% c(1,2) & (F_SYSTEM %in% c(1,2,3,4)|!is.na(NHS))]
    
    
  } else
    
    
    # rutting -----------------------------------------------------------------
  
  if(variable %in% c("RUTTING")){
    
    # dat.variable <- data[dataitem == variable & datayear == year,]
    # dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    
    # coverage = dat.SURFACE_TYPE[, .(routeid, beginpoint, endpoint, SURFACE_TYPE = valuenumeric)] %>%
    #   coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)])
    
    # coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE%in%c(2,6,7,8)]
    
    # ----------------
    
    # RUTTING ValueNumeric Must Exist Where (SURFACE_TYPE in (2,6,7,8)) AND (
    #   (FACILITY_TYPE in (1,2) AND (F_SYSTEM = 1 OR NHS OR Sample)) OR 
    #   (DIR_THROUGH_LANES >0 AND (IRI IS NOT NULL OR PSR IS NOT NULL))
    #   )
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      dataitems = c(
        'F_SYSTEM', 'SURFACE_TYPE', 'FACILITY_TYPE', 'DIR_THROUGH_LANES', 'NHS', 'IRI', 'PSR'))
    
    coverage[, required := 
               (SURFACE_TYPE %in% c(2,6, 7, 8)) & (
                 (FACILITY_TYPE %in% c(1, 2) & (F_SYSTEM == 1 | !is.na(NHS) | !is.na(expansionfactor))) |
                   (DIR_THROUGH_LANES > 0 & ( !is.na(IRI) | !is.na(PSR) ))
               )
    ]
    # coverage[, required := 
    #            (SURFACE_TYPE %in% c(2,6, 7, 8)) & (
    #              (FACILITY_TYPE %in% c(1, 2) & (F_SYSTEM == 1 | NHS | !is.na(expansionfactor))) |
    #                (DIR_THROUGH_LANES > 0 & ( !is.na(IRI) | !is.na(PSR) ))
    #            )
    # ]
    
  } else
    
    
    # shoulder_width_l --------------------------------------------------------
  
  if(variable == "SHOULDER_WIDTH_L"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SHOULDER_TYPE <- data[dataitem == "SHOULDER_TYPE" & datayear == year,]
    dat.MEDIAN_TYPE <- data[dataitem == "MEDIAN_TYPE" & datayear == year,]
    
    coverage = coverage_join(
      dat.SHOULDER_TYPE[, .(routeid, beginpoint, endpoint, SHOULDER_TYPE = valuenumeric)],
      dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)]) %>%
      coverage_join(
        dat.MEDIAN_TYPE[, .(routeid, beginpoint, endpoint, MEDIAN_TYPE = valuenumeric)])
    
    coverage[, required := SHOULDER_TYPE %in% 2:6 & MEDIAN_TYPE %in% 2:7 & (!is.na(expansionfactor))]
    
    
  } else
    
    # shoulder_width_r ---------------------------------------------------------
  
  if(variable == "SHOULDER_WIDTH_R"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SHOULDER_TYPE <- data[dataitem == "SHOULDER_TYPE" & datayear == year,]
    
    coverage = dat.SHOULDER_TYPE[, .(routeid, beginpoint, endpoint, SHOULDER_TYPE = valuenumeric)] %>%
      coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)])
    
    coverage[, required := SHOULDER_TYPE %in% 2:6 & (!is.na(expansionfactor))]
    
  } else
    
    # signal_type -------------------------------------------------------------
  
  if(variable %in% c("SIGNAL_TYPE")){
    
    # dat.variable <- data[dataitem == variable & datayear == year,]
    # dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    # dat.ACCESS_CONTROL <- data[dataitem == "ACCESS_CONTROL" & datayear == year,]
    
    # coverage = dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)] %>%
    #   coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)]) %>%
    #   coverage_join(dat.ACCESS_CONTROL[, .(routeid, beginpoint, endpoint, ACCESS_CONTROL = valuenumeric)])
    
    # coverage[, required :=  (URBAN_ID != 99999 & ACCESS_CONTROL == 1 & !is.na(expansionfactor))]
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      dataitems = c('URBAN_ID', 'NUMBER_SIGNALS')
    )
    
    coverage[, required := !is.na(expansionfactor) & URBAN_ID != 99999 & NUMBER_SIGNALS >= 1]
    
  } else 
    
    # speed_limit -------------------------------------------------------------
  
  if ( variable == 'SPEED_LIMIT' ){
    
    # HPMS9 -- must exist on NHS as well as Samples
    
    dat.variable = data[
      dataitem == variable & datayear == year, 
      .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)]
    
    dat.NHS = data[
      dataitem == 'NHS' & datayear == year, 
      .(routeid, beginpoint, endpoint, NHS = valuenumeric)]
    
    coverage = dat.variable %>% 
      coverage_join(dat.NHS)
    
    coverage[, required := !is.na(NHS) | !is.na(expansionfactor)]
    
    
  } else 
    
    # surface_type ------------------------------------------------------------
  
  if ( variable == 'SURFACE_TYPE' ) {
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      dataitems = c(
        'F_SYSTEM', 'FACILITY_TYPE', 'NHS', 'DIR_THROUGH_LANES', 'IRI', 'PSR'))
    
    coverage[, required := 
               ( FACILITY_TYPE %in% c(1, 2) & (F_SYSTEM == 1 | !is.na(NHS) | !is.na(expansionfactor)) ) |
               ( DIR_THROUGH_LANES > 0 & ( !is.na(IRI) | !is.na(PSR) ) )
    ]
    
  } else
    
    # terrain_type ------------------------------------------------------------
  
  if(variable %in% c("TERRAIN_TYPE")){
    
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      dataitems = c('F_SYSTEM', 'URBAN_ID'))
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,] 
    
    coverage2 = dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)] %>%
      coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)])
    
    coverage[, required := !is.na(expansionfactor) & URBAN_ID == 99999 & F_SYSTEM %in% 1:5]
    
  } else
    
    
    # thickness_flexible ------------------------------------------------------
  
  if(variable %in% c("THICKNESS_FLEXIBLE")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    
    coverage <- sqldf(
      "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as SURFACE_TYPE, 
        B.valuenumeric as variable,B.expansionfactor 
        from [dat.SURFACE_TYPE] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE %in% c(2,6,7,8)]
    
  } else
    
    
    # thickness_rigid ---------------------------------------------------------
  
  if(variable %in% c("THICKNESS_RIGID")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    
    coverage <- sqldf(
      "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as SURFACE_TYPE, 
        B.valuenumeric as variable,B.expansionfactor 
        from [dat.SURFACE_TYPE] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")
    
    setDT(coverage)
    
    coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE %in% 3:10]
    
  } else 
    
    
    # travel time code ------------------------------------------------------
  if(variable %like% 'TRAVEL_TIME_CODE'){
    #browser()
    
    coverage = get_coverage_data(
      data,
      year = year,
      variable = variable,
      dataitems = c('NHS'))
    
    coverage[, required := !is.na(variable) & !is.na(NHS)]
    
    # score = data[dataitem == variable & datayear == year & !is.na(expansionfactor), .N] /
    #   data[dataitem == variable & datayear == year, .N]
    
    return(score)
    
  } else 
    
    # toll_ID ------------------------------------------------------------
  
  if(variable == "TOLL_ID"){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    
    coverage <- dat.variable
    
    coverage[, required := FALSE] # !is.na(variable)]
    
  } else # end TOLL_ID
    
    # toll_type ---------------------------------------------------------------
  
  if(variable == "TOLL_TYPE"){
    browser()
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.TOLL_ID <- data[dataitem == "TOLL_ID" & datayear == year,]
    
    coverage <- sqldf(
      "select 
        A.routeid,A.beginpoint,A.endpoint,A.dataitem,A.valuenumeric as TOLL_ID, 
        B.valuenumeric as variable 
        from [dat.TOLL_ID] A 
        left join [dat.variable] B on 
        A.routeid = B.routeid and (
        ( A.beginpoint between B.beginpoint and B.endpoint and A.endpoint between B.beginpoint and B.endpoint ) or
        ( B.beginpoint between A.beginpoint and A.endpoint and B.endpoint between A.beginpoint and A.endpoint )
        )")
    
    setDT(coverage)
    coverage[, required := TRUE]
    
  } else # end TOLL_TYPE
    
    # turn_lanes_r, turn_lanes_l ----------------------------------------------
  
  if(variable %in% c("TURN_LANES_R","TURN_LANES_L")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.URBAN_ID <- data[dataitem == "URBAN_ID" & datayear == year,]
    dat.ACCESS_CONTROL <- data[dataitem == "ACCESS_CONTROL" & datayear == year,]
    
    coverage = dat.URBAN_ID[, .(routeid, beginpoint, endpoint, URBAN_ID = valuenumeric)] %>%
      coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuenumeric, expansionfactor)]) %>%
      coverage_join(dat.ACCESS_CONTROL[, .(routeid, beginpoint, endpoint, ACCESS_CONTROL = valuenumeric)])
    
    coverage[, required := URBAN_ID < 99999 & ACCESS_CONTROL > 1 & (!is.na(expansionfactor))]
    
  } else
    
    
    # urban_id ---------------------------------------------------------------
  
  if(variable == "URBAN_ID"){
    
    coverage = get_coverage_data(
      data = data,
      year = year,
      variable = variable,
      dataitems = c('F_SYSTEM', 'FACILITY_TYPE', 'DIR_THROUGH_LANES', 'IRI', 'PSR')
    )
    
    coverage[, required :=
               (FACILITY_TYPE %in% c(1,2,4) & (F_SYSTEM %in% 1:5)) |
               (FACILITY_TYPE == 6 & DIR_THROUGH_LANES > 0 & F_SYSTEM == 1 & (!is.na(IRI) | !is.na(PSR)))]
    # coverage[, required :=
    #            (FACILITY_TYPE %in% c(1,2,4) & (F_SYSTEM %in% 1:7)) |
    #            (FACILITY_TYPE == 6 & DIR_THROUGH_LANES > 0 & F_SYSTEM == 1 & (!is.na(IRI) | !is.na(PSR)))]
    
    
  } else
    
    # YEAR_LAST_CONSTRUCTION -------------------------------------------------
  
  if(variable %in% c("YEAR_LAST_CONSTRUCTION")){
    
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,] 
    
    coverage = dat.SURFACE_TYPE[, .(routeid, beginpoint, endpoint, SURFACE_TYPE = valuenumeric)] %>%
      coverage_join(
        dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = ifelse(datayear <= 2020, begindate,valuedate))])
        # dat.variable[, .(routeid, beginpoint, endpoint, expansionfactor, variable = valuedate)])
    
    coverage[, required := !is.na(expansionfactor) & SURFACE_TYPE %in% 2:10]
    
    
  } else
    
    # Year_last_improv ---------------------------------------------------------
  
  if(variable %in% c("YEAR_LAST_IMPROVEMENT")){
  # browser()
    dat.variable <- data[dataitem == variable & datayear == year,]
    dat.SURFACE_TYPE <- data[dataitem == "SURFACE_TYPE" & datayear == year,]
    dat.YEAR_LAST_CONSTRUCTION <- data[dataitem == "YEAR_LAST_CONSTRUCTION" & datayear == year,]
    
    coverage = dat.SURFACE_TYPE[, .(routeid, beginpoint, endpoint, SURFACE_TYPE = valuenumeric)] %>%
      coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuedate, expansionfactor, begindate)]) %>%
                                     # variable = ifelse( datayear <= 2020, begindate, valuedate), expansionfactor)]) %>%
      # coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuedate, expansionfactor)]) %>%
      coverage_join(dat.YEAR_LAST_CONSTRUCTION[, .(routeid, beginpoint, endpoint, YEAR_LAST_CONSTRUCTION = 
                                                     valuedate)])
                                                     #ifelse( datayear <= 2020, begindate, valuedate))])
      # coverage_join(dat.variable[, .(routeid, beginpoint, endpoint, variable = valuedate, expansionfactor)]) %>%
      # coverage_join(dat.YEAR_LAST_CONSTRUCTION[, .(routeid, beginpoint, endpoint, YEAR_LAST_CONSTRUCTION = valuedate)])
    
    coverage[, required := (!is.na(expansionfactor) & SURFACE_TYPE %in% 2:10) |
                            (YEAR_LAST_CONSTRUCTION < year(begindate) - 20)]
    
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


