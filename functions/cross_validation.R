

calc_cross_validation = function(data, year){
  
  # filter ramps (facility type == 4) 
  data = data[datayear == year & FACILITY_TYPE %in% c(1,2)]
  
  results = list()

 #browser()
  # Tests not used here: evaluated as outliers

 source("dev/cross_validations_HPMS9.R")
 
  # results[["1"]]  = summarize_validation(cross_validation_1(data))
  # results[["14"]] = summarize_validation(cross_validation_14(data))
  # results[["47"]] = summarize_validation(cross_validation_47(data))
  # results[["49"]] = summarize_validation(cross_validation_49(data))
  # results[["52"]] = summarize_validation(cross_validation_52(data))
  # results[["54"]] = summarize_validation(cross_validation_54(data))
  # results[["55"]] = summarize_validation(cross_validation_55(data))
  # results[["56"]] = summarize_validation(cross_validation_56(data))
  # results[["57"]] = summarize_validation(cross_validation_57(data))


  # Tests not used for HPMS 9
  # results[["23"]] = summarize_validation(cross_validation_23(data))  

  # results[["46.1"]] = summarize_validation(cross_validation_46(data, 'IRI'))
  # results[["46.2"]] = summarize_validation(cross_validation_46(data, 'RUTTING'))
  # results[["46.3"]] = summarize_validation(cross_validation_46(data, 'FAULTING'))
  # results[["46.4"]] = summarize_validation(cross_validation_46(data, 'CRACKING_PERCENT'))

  # results[["61.1"]] = summarize_validation(cross_validation_61(data, 'IRI'))
  # results[["61.2"]] = summarize_validation(cross_validation_61(data, 'RUTTING'))
  # results[["61.3"]] = summarize_validation(cross_validation_61(data, 'FAULTING'))
  # results[["61.4"]] = summarize_validation(cross_validation_61(data, 'CRACKING_PERCENT'))

  # results[["64.1"]] = summarize_validation(cross_validation_64(data, 'IRI'))
  # results[["64.2"]] = summarize_validation(cross_validation_64(data, 'RUTTING'))
  # results[["64.3"]] = summarize_validation(cross_validation_64(data, 'FAULTING'))
  # results[["64.4"]] = summarize_validation(cross_validation_64(data, 'CRACKING_PERCENT'))

  # results[["65"]] = summarize_validation(cross_validation_65(data))
  # results[["66"]] = summarize_validation(cross_validation_66(data))



  # Tests used for HPMS 9
  results[["2"]]  = summarize_validation(cross_validation_2(data))

  results[["9"]]  = summarize_validation(cross_validation_9(data))
  
  results[["15"]] = summarize_validation(cross_validation_15(data))
  results[["16"]] = summarize_validation(cross_validation_16(data))
  results[["17"]] = summarize_validation(cross_validation_17(data))
  
  results[["20"]] = summarize_validation(cross_validation_20(data))
  
  results[["22"]] = summarize_validation(cross_validation_22(data))

  results[["39"]] = summarize_validation(cross_validation_39(data))
  results[["40"]] = summarize_validation(cross_validation_40(data))
  results[["41"]] = summarize_validation(cross_validation_41(data))
  results[["42"]] = summarize_validation(cross_validation_42(data))
  results[["43"]] = summarize_validation(cross_validation_43(data))
  results[["44"]] = summarize_validation(cross_validation_44(data))
  results[["45"]] = summarize_validation(cross_validation_45(data))

  results[["51"]] = summarize_validation(cross_validation_51(data))

  results[["53"]] = summarize_validation(cross_validation_53(data))

  results[["54.1"]] = summarize_validation(cross_validation_54_1(data))
  results[["54.2"]] = summarize_validation(cross_validation_54_2(data))
  results[["55.1"]] = summarize_validation(cross_validation_55_1(data))
  results[["55.2"]] = summarize_validation(cross_validation_55_2(data))
  results[["55.3"]] = summarize_validation(cross_validation_55_3(data))
  
  results[["60"]] = summarize_validation(cross_validation_60(data))

  results[["62"]] = summarize_validation(cross_validation_62(data))
  results[["63"]] = summarize_validation(cross_validation_63(data))

  results[["68"]]   = summarize_validation(cross_validation_68(data))
  results[["70"]]   = summarize_validation(cross_validation_70(data))
  results[["72"]]   = summarize_validation(cross_validation_72(data))
  results[["73"]]   = summarize_validation(cross_validation_73(data))
  results[["74"]]   = summarize_validation(cross_validation_74(data))
  results[["75.1"]] = summarize_validation(cross_validation_75_1(data))
  results[["75.2"]] = summarize_validation(cross_validation_75_2(data))
  results[["76.1"]] = summarize_validation(cross_validation_76_1(data))
  results[["76.2"]] = summarize_validation(cross_validation_76_2(data))
  
  results[["x"]]  = summarize_validation(cross_validation_x(data))
  results[["y"]]  = summarize_validation(cross_validation_y(data))

  results = rbindlist(results, idcol=TRUE)
  results = rbindlist(list(gCrossLabels[results, on=.(.id)], measurement_checks(data)))
  
  return(results)
}


###################################################################
# summarize the results of the validation
summarize_validation = function(results){
  tbl <- results[['results']]
  
  if(is.null(tbl)){
    return(
      data.table(section_total=0, 
                 mileage_total=0,
                 sections_pass=NA,
                 mileage_pass=NA)
    ) 
  }
  
  if(nrow(tbl[applies == TRUE & passes == TRUE]) == 0){
    tbl=rbindlist(list(tbl,
      data.table(applies=TRUE, passes=TRUE, N=0, num_sections=0, mileage=0))
    )
  }
  tbl[applies==TRUE,
                         c("section_total", "mileage_total") :=
                         .(sum(num_sections), sum(mileage))]  
  
  tbl[applies==TRUE & passes == TRUE,
                       c("sections_pass", "mileage_pass") :=
                         .(sections_pass=num_sections/section_total,
                           mileage_pass=mileage/mileage_total)]  
  
  tbl$sections_pass[is.nan(tbl$sections_pass)] <- NA
  tbl$mileage_pass[is.nan(tbl$mileage_pass)] <- NA
  
  return(tbl[applies == TRUE & passes == TRUE,
             .(section_total, mileage_total, sections_pass, mileage_pass)])   
}

###################################################################
cross_validation_1 = function(data){
  
  # IRI >= 30 and <= 400
  #browser()
  iri = data[dataitem=="IRI",
             .(routeid, beginpoint, endpoint, IRI=valuenumeric, num_sections)]
  
  if(iri[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = iri

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(IRI), 
               passes  = !is.na(IRI)&IRI>=30&IRI<=400)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_2 = function(data){  
  # only allow Sample where Facility_Type IN 1,2 and 
  # (F_System = 1-5 or F_System = 6 and Urban Code <99999)
  
  #browser()
  
  # Note - this test is not restricted to a particular dataitem so the 
  # mileage and number of sections are much higher than the total mileage
  
  comparison = data[, 
                    .(routeid, beginpoint, endpoint, num_sections,
                      sample = !is.na(expansionfactor), F_SYTEMorig, 
                      URBAN_ID, FACILITY_TYPE)]
  
  if(comparison[, .N]==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !(FACILITY_TYPE %in% c(1, 2) &
                                       (F_SYTEMorig %in% 1:5 | (F_SYTEMorig == 6 & URBAN_ID < 99999)))  , 
                         passes  = !sample)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))
  
}

###################################################################
cross_validation_9 = function(data){
  # Year_Last_Construction	<= DataYear or NULL
  
  #browser()
  year_last_construction = data[dataitem=="YEAR_LAST_CONSTRUCTION",
                                #.(routeid,beginpoint,endpoint,YEAR_LAST_CONSTRUCTION=valuedate, datayear, num_sections)]
								.(routeid,beginpoint,endpoint,
								YEAR_LAST_CONSTRUCTION = as.POSIXct( ifelse( datayear <= 2020, begindate, valuedate ) ), 
								begindate = as.POSIXct( ifelse( datayear <= 2020, NA, begindate ) ),
								datayear, num_sections)]

  if(year_last_construction[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = year_last_construction
  
  # # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(YEAR_LAST_CONSTRUCTION),
               passes  = !is.na(YEAR_LAST_CONSTRUCTION) &
                 YEAR_LAST_CONSTRUCTION <= begindate)][order(applies,passes)]
  
               # passes  = !is.na(YEAR_LAST_CONSTRUCTION)&year(YEAR_LAST_CONSTRUCTION)<=datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_14 = function(data){
  # Lane width must be > 5 and < 19 
  
  #browser()
  lane_width = data[dataitem=="LANE_WIDTH",
                    .(routeid,beginpoint,endpoint,LANE_WIDTH=valuenumeric, num_sections)]
  
  if(lane_width[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = lane_width
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(LANE_WIDTH), 
               passes  = LANE_WIDTH > 5 & LANE_WIDTH < 19)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_15 = function(data){
  # SPEED_LIMIT should be divisible by 5, and < 90 OR = 999
  # should it include a check of 0?
 
  speed_limit = data[dataitem=="SPEED_LIMIT",.(routeid,beginpoint,endpoint,SPEED_LIMIT=valuenumeric, num_sections)]

  if(speed_limit[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  comparison = speed_limit

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(SPEED_LIMIT), 
               passes  = ( (SPEED_LIMIT == 999) | (SPEED_LIMIT %% 5 == 0 & SPEED_LIMIT < 90) ) 
               
             )
  ][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Counter Peak Lanes is SP
cross_validation_16 = function(data){
  # Counter_Peak_Lanes is NULL if FACILITY_TYPE is 1
  
  facility_type      = data[dataitem=="FACILITY_TYPE",
                            .(routeid,beginpoint,endpoint,FACILITY_TYPE=valuenumeric,num_sections)]
  counter_peak_lanes = data[dataitem=="COUNTER_PEAK_LANES",
                            .(routeid,beginpoint,endpoint,COUNTER_PEAK_LANES=valuenumeric)]
  
  if(facility_type[, .N] == 0 | counter_peak_lanes[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = counter_peak_lanes[facility_type,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = FACILITY_TYPE==1, 
               passes  = is.na(COUNTER_PEAK_LANES))][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_17 = function(data){
  # SU AADT + CU AADT < (0.8*AADT)
  # need to confirm when this should apply
  
  #browser()
  aadt_single_unit = data[dataitem=="AADT_SINGLE_UNIT",
                          .(routeid,beginpoint,endpoint,AADT_SINGLE_UNIT=valuenumeric, num_sections)]
  aadt_combination = data[dataitem=="AADT_COMBINATION",
                          .(routeid,beginpoint,endpoint,AADT_COMBINATION=valuenumeric)]
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  if(aadt_single_unit[, .N] == 0 | aadt_combination[, .N] == 0 | aadt[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt_combination[aadt_single_unit,on=.(routeid,beginpoint,endpoint)]
  comparison = aadt[comparison,   on=.(routeid,beginpoint,endpoint)]
  
  # setting NAs to 0
  comparison[is.na(AADT_SINGLE_UNIT)&!is.na(AADT_COMBINATION),AADT_SINGLE_UNIT:=0]
  comparison[is.na(AADT_COMBINATION)&!is.na(AADT_SINGLE_UNIT),AADT_COMBINATION:=0]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(AADT_COMBINATION), # always applies if through_lanes exists
               passes  = AADT_SINGLE_UNIT + AADT_COMBINATION < (0.8*AADT) )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_20 = function(data){
  # Median Width Null if (FACILITY_TYPE is 1 or 4) or Median_Type Code <2
  # what does NULL mean? 0 or not reported or reported but NULL?
  
  #browser()
  median_width = data[dataitem=="MEDIAN_TYPE",
                      .(routeid,beginpoint,endpoint,MEDIAN_TYPE=valuenumeric, num_sections)]
  median_type = data[dataitem=="MEDIAN_WIDTH",
                     .(routeid,beginpoint,endpoint,MEDIAN_WIDTH=valuenumeric)]
  facility_type = data[dataitem=="FACILITY_TYPE",
                       .(routeid,beginpoint,endpoint,FACILITY_TYPE=valuenumeric)]

  if(median_width[, .N] == 0|median_type[, .N] == 0|facility_type[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = median_type[median_width,on=.(routeid,beginpoint,endpoint)]
  comparison = facility_type[comparison,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = MEDIAN_TYPE<1|FACILITY_TYPE%in%c(1,4), 
               passes  = is.na(MEDIAN_WIDTH))][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_22 = function(data){
  # Median Type in (2,3,4,5,6,7)	Median Width > 0
  
  #browser()
  median_width = data[dataitem=="MEDIAN_TYPE",
                      .(routeid,beginpoint,endpoint,MEDIAN_TYPE=valuenumeric, num_sections)]
  median_type = data[dataitem=="MEDIAN_WIDTH",
                     .(routeid,beginpoint,endpoint,MEDIAN_WIDTH=valuenumeric)]

  if(median_width[, .N] == 0|median_type[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = median_type[median_width,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = MEDIAN_TYPE%in%2:7, 
               passes  = MEDIAN_WIDTH>0)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_23 = function(data){
  # Widening_Obstacle must contain A-G where Widening_Potential <9
  
  # TODO: verify widening variables will need no cross checks
  
  #browser()
  widening_obstacle = data[dataitem=="WIDENING_OBSTACLE", 
                           .(routeid,beginpoint,endpoint,WIDENING_OBSTACLE=valuetext, num_sections)]
  widening_potential = data[dataitem=="WIDENING_POTENTIAL",
                            .(routeid,beginpoint,endpoint,WIDENING_POTENTIAL=valuenumeric)]
  
  if(widening_obstacle[, .N] == 0 | widening_potential[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = widening_potential[widening_obstacle,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = WIDENING_POTENTIAL<9, 
               passes  = grepl("[a-gA-G]+",WIDENING_OBSTACLE)&WIDENING_POTENTIAL<9)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_39 = function(data){
  # DIR_Factor must be 100 where Facility_Type = 1
  
  #browser()
  facility_type = data[dataitem=="FACILITY_TYPE",
                       .(routeid,beginpoint,endpoint,FACILITY_TYPE=valuenumeric, num_sections)]
  dir_factor = data[dataitem=="DIR_FACTOR",
                    .(routeid,beginpoint,endpoint,DIR_FACTOR=valuenumeric)]
  
  if(facility_type[, .N] == 0 | dir_factor[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
 
  # join the two together
  comparison = dir_factor[facility_type,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = FACILITY_TYPE==1 &! is.na(DIR_FACTOR), # DIR_FACTOR is SP 
               passes  = DIR_FACTOR==100)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_40 = function(data){
  # DIR_Factor must be 50=< and <=70 where Facility_Type = 2
  
  #browser()
  facility_type = data[dataitem=="FACILITY_TYPE",
                       .(routeid,beginpoint,endpoint,FACILITY_TYPE=valuenumeric, num_sections)]
  dir_factor = data[dataitem=="DIR_FACTOR",
                    .(routeid,beginpoint,endpoint,DIR_FACTOR=valuenumeric)]
  
  if(facility_type[, .N] == 0|dir_factor[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = dir_factor[facility_type,on=.(routeid,beginpoint,endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = FACILITY_TYPE == 2 & !is.na(DIR_FACTOR), # DIR_FACTOR is SP
                         passes  = DIR_FACTOR > 50 & DIR_FACTOR <= 75 )][order(applies,passes)]
                         # passes  = DIR_FACTOR >= 50 & DIR_FACTOR < 70 )][order(applies,passes)] 
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))
  
}

###################################################################
cross_validation_41 = function(data){
  # AADT < FAADT < 4*AADT
  # this is done elsewhere in the scorecard
  
  #browser()
  future_aadt = data[dataitem=="FUTURE_AADT",
                     .(routeid, beginpoint, endpoint, FUTURE_AADT=valuenumeric, valuedate, begindate, num_sections)]
                     # .(routeid, beginpoint, endpoint, FUTURE_AADT=valuenumeric, num_sections)]
  
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]

  if(future_aadt[, .N] == 0|aadt[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = aadt[future_aadt,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,                                              
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(FUTURE_AADT),  # TODO: check if fix may be needed for 2020 and earlier
               passes  =  ( FUTURE_AADT > AADT  & FUTURE_AADT < 4 * AADT & is.na(valuedate) ) |
                            FUTURE_AADT < AADT * 0.2 * ( year(valuedate) - year(begindate) )
                 ) ][order(applies,passes)] 
             # .(applies = !is.na(FUTURE_AADT), 
             #   passes  = FUTURE_AADT > AADT  & FUTURE_AADT < 4 * AADT)][order(applies,passes)] 
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_42 = function(data){

  # (AADT_SINGLE_UNIT x 0.025) < (AADT x (PCT_DH_SINGLE_UNIT/100)) < (AADT_SINGLE_UNIT x 0.4)  

  # browser()
  pct_peak_single = data[
    dataitem=="PCT_DH_SINGLE_UNIT",
    .(routeid,beginpoint,endpoint,PCT_DH_SINGLE_UNIT=valuenumeric, num_sections)]

  aadt_single_unit = data[
    dataitem=="AADT_SINGLE_UNIT",
    .(routeid,beginpoint,endpoint,AADT_SINGLE_UNIT=valuenumeric)]
  
  aadt = data[
    dataitem=="AADT",
    .(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  if(pct_peak_single[, .N] == 0 | aadt_single_unit[, .N] == 0 | aadt[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt_single_unit[pct_peak_single, on = .(routeid,beginpoint,endpoint)]
  comparison = aadt[comparison, on = .(routeid,beginpoint,endpoint)]
  
  # setting NAs to 0
  #comparison[is.na(AADT_SINGLE_UNIT)&!is.na(AADT_COMBINATION),AADT_SINGLE_UNIT:=0]
  #comparison[is.na(AADT_COMBINATION)&!is.na(AADT_SINGLE_UNIT),AADT_COMBINATION:=0]
  
  # apply the condition
  results = comparison[,
    .(
      .N,
      num_sections = sum(num_sections,na.rm=TRUE),
      mileage      = sum(endpoint-beginpoint)
    ),
    .(
      applies = !is.na(PCT_DH_SINGLE_UNIT), 
      passes  = 
        PCT_DH_SINGLE_UNIT < 25 & PCT_DH_SINGLE_UNIT > 0 &
        
        AADT * PCT_DH_SINGLE_UNIT / 100 > (AADT_SINGLE_UNIT * 0.01) &
        AADT * PCT_DH_SINGLE_UNIT / 100 < (AADT_SINGLE_UNIT * 0.5))
      # passes  = AADT * PCT_DH_SINGLE_UNIT / 100 > (AADT_SINGLE_UNIT * 0.025) &
      #   AADT * PCT_DH_SINGLE_UNIT / 100 < (AADT_SINGLE_UNIT * 0.4))
    ][order(applies,passes)]

  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_43 = function(data){

  # (AADT_COMBINATION x 0.01) < (AADT x (PCT_DH_COMBINATION/100)) < (AADT_COMBINATION x 0.5) 

  #browser()
  pct_combination = data[dataitem=="PCT_DH_COMBINATION",
                         .(routeid,beginpoint,endpoint,PCT_DH_COMBINATION=valuenumeric, num_sections)]
  
  aadt_combination = data[dataitem=="AADT_COMBINATION",
                          .(routeid,beginpoint,endpoint,AADT_COMBINATION=valuenumeric)]
  
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]

  if(pct_combination[, .N] == 0 | aadt_combination[, .N] == 0 | aadt[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt_combination[pct_combination,on=.(routeid,beginpoint,endpoint)]
  comparison = aadt[comparison,   on=.(routeid,beginpoint,endpoint)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(PCT_DH_COMBINATION), 
               passes  = 
                (PCT_DH_COMBINATION < 25 & PCT_DH_COMBINATION >= 0 )          &  # TODO: verify if >= or > 0
                (AADT * PCT_DH_COMBINATION / 100) > (AADT_COMBINATION * 0.01) & 
                (AADT * PCT_DH_COMBINATION / 100) < (AADT_COMBINATION * 0.5)
               # passes  = (AADT * PCT_DH_COMBINATION / 100) > (AADT_COMBINATION * 0.025) &
               #   (AADT * PCT_DH_COMBINATION / 100) < (AADT_COMBINATION * 0.4)
             )][order(applies,passes)]
  
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_44 = function(data){
  # AADT_Combination < AADT * 0.4

  #browser()
  aadt_combination = data[dataitem=="AADT_COMBINATION",
                          .(routeid,beginpoint,endpoint,AADT_COMBINATION=valuenumeric, num_sections)]
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  if(aadt_combination[, .N] == 0 | aadt[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt[aadt_combination,on=.(routeid,beginpoint,endpoint)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(AADT_COMBINATION), # always applies if through_lanes exists
               passes  = AADT_COMBINATION < (AADT * 0.4) )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_45 = function(data){
  # AADT_Single_Unit < AADT * 0.4
  
  #browser()
  aadt_single_unit = data[dataitem=="AADT_SINGLE_UNIT",.(routeid,beginpoint,endpoint,AADT_SINGLE_UNIT=valuenumeric, num_sections)]
  aadt = data[dataitem=="AADT",.(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  if(aadt_single_unit[, .N] == 0 | aadt[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt[aadt_single_unit,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(AADT_SINGLE_UNIT), 
               passes  = AADT_SINGLE_UNIT < AADT * 0.4)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_46 = function(data, variable){

  # ValueDate must >= DataYear - 1 where (sample | (ValueText is NULL and F_System > 1 and
  # NHS in (1,2,3,4,5,6,7,8,9))

  # variable %in% c('IRI', 'RUTTING', 'FAULTING', 'CRACKING_PERCENT')
  
  # browser()
  
  comparison = data[dataitem == variable,
               .(routeid, beginpoint, endpoint, datayear, F_SYTEMorig, NHS,
                 valuedate, begindate, valuetext, sample = !is.na(expansionfactor), num_sections)]
  
  if(comparison[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  

  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = sample | (is.na(valuetext) & F_SYTEMorig > 1 & NHS %in% 1:9), 
                         #passes  = year(valuedate) >= datayear - 1)][order(applies,passes)]
						 passes  = ifelse( datayear <= 2020, year(begindate), year(valuedate) ) >= datayear - 1)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){ 
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
}

###################################################################
cross_validation_47 = function(data){
  # Faulting should be <= 1
  
  #browser()
  faulting = data[dataitem=="FAULTING",
                  .(routeid,beginpoint,endpoint,FAULTING=valuenumeric, num_sections)]
  
  if(faulting[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = faulting

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(FAULTING), 
               passes  = !is.na(FAULTING)&FAULTING<=1)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))

}

###################################################################
cross_validation_49 = function(data){
  # K_Factor must be > 4 and <20 
  
  #browser()
  k_factor = data[dataitem=="K_FACTOR",
                  .(routeid,beginpoint,endpoint,K_FACTOR=valuenumeric, num_sections)]

  if(k_factor[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = k_factor
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(K_FACTOR), 
               passes  = K_FACTOR > 4 & K_FACTOR < 30)][order(applies,passes)]
               # passes  = K_FACTOR > 4 & K_FACTOR < 20)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_51 = function(data){
  # Where Surface Type is in (3,4,5,9,10) Cracking Percent should be < 75
  
  #browser()
  cracking_percent = data[dataitem=="CRACKING_PERCENT", 
                          .(routeid,beginpoint,endpoint,CRACKING_PERCENT=valuenumeric, num_sections)]
  surface_type = data[dataitem=="SURFACE_TYPE",
                      .(routeid,beginpoint,endpoint,SURFACE_TYPE=valuenumeric)]
  
  if(cracking_percent[, .N] == 0 | surface_type[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = surface_type[cracking_percent,on=.(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = SURFACE_TYPE%in%c(3,4,5,9,10), 
               passes  = SURFACE_TYPE%in%c(3,4,5,9,10)&CRACKING_PERCENT<=75)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_52 = function(data){
  # Rutting should be < 1
  
  #browser()
  rutting = data[dataitem=="RUTTING",
                 .(routeid,beginpoint,endpoint,RUTTING=valuenumeric, num_sections)]
  
  if(rutting[, .N] ==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = rutting

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(RUTTING), 
               passes  = !is.na(RUTTING)&RUTTING<1)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_53 = function(data){
  # Through_Lanes>1 when Facility_Type = 2
  
  through_lanes = data[dataitem=="THROUGH_LANES",.(routeid,beginpoint,endpoint,THROUGH_LANES=valuenumeric,num_sections)]
  facility_type = data[dataitem=="FACILITY_TYPE",.(routeid,beginpoint,endpoint,FACILITY_TYPE=valuenumeric)]

  if(through_lanes[, .N] == 0 | facility_type[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = facility_type[through_lanes,on=.(routeid,beginpoint,endpoint)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = FACILITY_TYPE==2,
               passes  = FACILITY_TYPE==2&THROUGH_LANES > 1 )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# cross_validation_54 = function(data){
#   # AADT_Combination Should be > 0 
#   # TODO: replace with 2 new x-vals
#   #browser()
#   aadt_combination = data[dataitem=="AADT_COMBINATION",
#                           .(routeid, beginpoint, endpoint, AADT_COMBINATION=valuenumeric, num_sections)]
#   
#   if(aadt_combination[, .N] == 0){
#     warning("Not applicable - Sufficient data from the state are not available")
#     return(list(results=NULL,comparison=NULL))  
#   }
# 
#   comparison = aadt_combination
#   
#   # apply the condition
#   results = comparison[,
#              .(
#                .N,
#                num_sections = sum(num_sections,na.rm=TRUE),
#                mileage      = sum(endpoint-beginpoint)
#               ),
#              .(applies = !is.na(AADT_COMBINATION), # always applies if through_lanes exists
#                passes  = AADT_COMBINATION > 0 )][order(applies,passes)]
#   
#   if(nrow(results[applies == TRUE])==0){
#     warning("Not applicable - Sufficient data from the state are not available")
#     return(list(results=NULL,comparison=NULL))  
#   }
#   
#   
#   return(list(results=results,comparison=comparison))
# 
# }

###################################################################
cross_validation_54_1 = function(data){
  # AADT_Combination Should be > 0 when AADT is > 500
  #browser()
  aadt_combination = data[dataitem=="AADT_COMBINATION",
                          .(routeid, beginpoint, endpoint, AADT_COMBINATION=valuenumeric, num_sections)]
  
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  if(aadt_combination[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  comparison = aadt[aadt_combination, on =.( routeid, beginpoint, endpoint)] #aadt_combination
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(AADT_COMBINATION & AADT > 500), # always applies if through_lanes exists
                         passes  = AADT_COMBINATION > 0)][order(applies,passes)]
  # passes  = AADT_COMBINATION > 0 )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))
  
}

###################################################################
cross_validation_54_2 = function(data){
  # AADT_Combination Should be > 0 
  # TODO: replace with 2 new x-vals
  #browser()
  aadt_combination = data[dataitem=="AADT_COMBINATION",
                          .(routeid, beginpoint, endpoint, AADT_COMBINATION=valuenumeric, num_sections)]
  
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  pct_dh_combination = data[dataitem == "PCT_DH_COMBINATION",
                            .(routeid, beginpoint, endpoint, PCT_DH_COMBINATION = valuenumeric)]
  
  if(aadt_combination[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  comparison =               aadt[aadt_combination, on =.( routeid, beginpoint, endpoint)] #aadt_combination
  comparison = pct_dh_combination[comparison, on =.( routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(AADT_COMBINATION) & PCT_DH_COMBINATION == 0 , # always applies if through_lanes exists
                         passes  = AADT_COMBINATION < 50 )][order(applies,passes)]
  # passes  = AADT_COMBINATION > 0 )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))
  
}
###################################################################
# cross_validation_55 = function(data){
#   # AADT_Single_Unit Shoud be > 0
#   # TODO: replace with 2 new x-vals
#   
#   #browser()
#   aadt_single_unit = data[dataitem=="AADT_SINGLE_UNIT",
#                           .(routeid,beginpoint,endpoint,AADT_SINGLE_UNIT=valuenumeric, num_sections)]
#   
#   if(aadt_single_unit[, .N] == 0){
#     warning("Not applicable - Sufficient data from the state are not available")
#     return(list(results=NULL,comparison=NULL))  
#   }
# 
#   comparison = aadt_single_unit
#  
#   # apply the condition
#   results = comparison[,
#              .(
#                .N,
#                num_sections = sum(num_sections,na.rm=TRUE),
#                mileage      = sum(endpoint-beginpoint)
#               ),
#              .(applies = !is.na(AADT_SINGLE_UNIT), 
#                passes  = AADT_SINGLE_UNIT >0 )][order(applies,passes)]
#   
#   if(nrow(results[applies == TRUE])==0){
#     warning("Not applicable - Sufficient data from the state are not available")
#     return(list(results=NULL,comparison=NULL))  
#   }
#   
#   
#   return(list(results=results,comparison=comparison))
# 
# }

###################################################################
cross_validation_55_1 = function(data){
  # IF AADT > 500 THEN AADT_SINGLE_UNIT should be > 0
  
  #browser()
  aadt_single_unit = data[dataitem=="AADT_SINGLE_UNIT",
                          .(routeid,beginpoint,endpoint,AADT_SINGLE_UNIT=valuenumeric, num_sections)]
  
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  if(aadt_single_unit[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  comparison = aadt[aadt_single_unit, on =.( routeid, beginpoint, endpoint)]  #aadt_single_unit
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(AADT_SINGLE_UNIT) & AADT > 500, 
                         passes  = AADT_SINGLE_UNIT > 0 )][order(applies,passes)]
  # passes  = AADT_SINGLE_UNIT >0 )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))
  
}

###################################################################
cross_validation_55_2 = function(data){
  # AADT_SINGLE_UNIT + AADT_COMBINATION should be < (0.8*AADT)
  
  #browser()
  aadt_single_unit = data[dataitem=="AADT_SINGLE_UNIT",
                          .(routeid,beginpoint,endpoint,AADT_SINGLE_UNIT=valuenumeric, num_sections)]
  
  aadt_combination = data[dataitem=="AADT_COMBINATION",
                          .(routeid, beginpoint, endpoint, AADT_COMBINATION=valuenumeric, num_sections)]
  
  aadt = data[dataitem=="AADT",
              .(routeid,beginpoint,endpoint,AADT=valuenumeric)]
  
  if(aadt_single_unit[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  comparison =       aadt[aadt_combination, on =.( routeid, beginpoint, endpoint)]  #aadt_single_unit
  comparison = comparison[aadt_single_unit, on =.( routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(AADT_SINGLE_UNIT), 
                         passes  = AADT_SINGLE_UNIT  + AADT_COMBINATION < 0.8 * AADT )][order(applies,passes)]
  # passes  = AADT_SINGLE_UNIT >0 )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))
  
}

###################################################################
cross_validation_55_3 = function(data){
  # AADT_SINGLE_UNIT should be < 50 Where PCT_DH_SINGLE_UNIT = 0
  # TODO: replace with 2 new x-vals
  
  #browser()
  aadt_single_unit = data[dataitem=="AADT_SINGLE_UNIT",
                          .(routeid,beginpoint,endpoint,AADT_SINGLE_UNIT=valuenumeric, num_sections)]
  
  pct_dh_single_unit = data[dataitem == "PCT_DH_SINGLE_UNIT",
                            .(routeid, beginpoint, endpoint, PCT_DH_SINGLE_UNIT = valuenumeric)]
  
  if(aadt_single_unit[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  comparison = aadt_single_unit[pct_dh_single_unit, on =.( routeid, beginpoint, endpoint)]  #aadt_single_unit
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(AADT_SINGLE_UNIT) & PCT_DH_SINGLE_UNIT == 0, 
                         passes  = AADT_SINGLE_UNIT   < 50 )][order(applies,passes)]
  # passes  = AADT_SINGLE_UNIT >0 )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))
  
}

###################################################################
cross_validation_56 = function(data){
  # PCT_Peak_Combination >0 and < 25%
  
  #browser()
  pct_peak_combination = data[dataitem=="PCT_DH_COMBINATION",
                              .(routeid,beginpoint,endpoint,PCT_DH_COMBINATION=valuenumeric, num_sections)]
  
  if(pct_peak_combination[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = pct_peak_combination
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(PCT_DH_COMBINATION), 
               passes  = PCT_DH_COMBINATION > 0 & PCT_DH_COMBINATION < 25)][order(applies,passes)]
 
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_57 = function(data){
  # PCT_Peak_Single >0 and < 25%

  #browser()
  pct_peak_single = data[dataitem=="PCT_DH_SINGLE_UNIT",
                         .(routeid,beginpoint,endpoint,PCT_DH_SINGLE_UNIT=valuenumeric, num_sections)]

  if(pct_peak_single[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = pct_peak_single
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(PCT_DH_SINGLE_UNIT), 
               passes  = PCT_DH_SINGLE_UNIT > 0 & PCT_DH_SINGLE_UNIT < 25)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_60 = function(data){
  # Shoulder_Width_L Should be < Median_Width
  
  # browser()
  left_shoulder_width = data[dataitem=="SHOULDER_WIDTH_L",
                             .(routeid,beginpoint,endpoint,SHOULDER_WIDTH_L=valuenumeric, num_sections)]
  median_width = data[dataitem=="MEDIAN_WIDTH",
                      .(routeid,beginpoint,endpoint,MEDIAN_WIDTH=valuenumeric)]
  
  if(left_shoulder_width[, .N] == 0 | median_width[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = median_width[
    left_shoulder_width,
    on = .(routeid,beginpoint,endpoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections, na.rm=TRUE),
               mileage      = sum(endpoint - beginpoint)
              ),
             .(applies = !is.na(SHOULDER_WIDTH_L) & !is.na(MEDIAN_WIDTH), 
               passes  = SHOULDER_WIDTH_L < MEDIAN_WIDTH)][order(applies, passes)]
  
  if( nrow(results[applies == TRUE]) == 0 ){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))

}

###################################################################
# ValueDate Must = Year Record  Where ValueText is Null AND F_System =1 
cross_validation_61 = function(data){ #, variable){
  # variable %in% c('IRI', 'RUTTING', 'FAULTING', 'CRACKING_PERCENT')
  
  comparison = data[dataitem %in% c('IRI', 'RUTTING', 'FAULTING', 'CRACKING_PERCENT'), 
                    .(routeid, beginpoint, endpoint, datayear, F_SYSTEM,
                      valuedate, begindate, valuetext, num_sections,
                      iri              = 1 * ( dataitem == 'IRI' ),
                      rutting          = 1 * ( dataitem == 'RUTTING' ),
                      faulting         = 1 * ( dataitem == 'FAULTING' ),
                      cracking_percent = 1 * ( dataitem == 'CRACKING_PERCENT' ) 
                      ) ]
  
  # comparison = data[dataitem == 'IRI',
  #                   .(routeid, beginpoint, endpoint, datayear, F_SYSTEM,
  #                     valuedate, begindate, valuetext, num_sections)]
  # 
  # comparison[]
  
  # comparison = data[dataitem == variable,
  #                   .(routeid, beginpoint, endpoint, datayear, F_SYTEMorig,
  #                     valuedate, begindate, valuetext, num_sections)]
  
  if(comparison[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  #FIXME: break out by each of the below variables
  #comparison[, .N, .(iri, rutting, faulting, cracking_percent)]
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = (is.na(valuetext) | valuetext == '') & F_SYSTEM == 1 & datayear > 2020, #F_SYTEMorig == 1, 
                         passes  = ( valuedate == begindate ))][order(applies,passes)]
                         #passes  = year(valuedate) >= datayear - 2)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - no sections meet application criteria")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
}

###################################################################
cross_validation_62 = function(data){

  
  # Where F_System =1, and IRI is Null, 
  # PSR ValueNumeric Must be >0 and PSR ValueText must = A

  psr = data[dataitem == "PSR",
                    .(routeid, beginpoint, endpoint,
                      F_SYTEMorig, valuetext, valuenumeric, num_sections)]
  
  iri = data[dataitem == 'IRI',
             .(routeid, beginpoint, endpoint, IRI = valuenumeric)]
  
  if(psr[, .N] == 0 | iri[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = iri[psr, on=.(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = is.na(IRI) & F_SYTEMorig == 1, 
                         passes  = valuenumeric > 0 & valuetext == 'A')][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_63 = function(data){
  # Where Surface Type is in (2,6,7,8) Cracking Percent should not exceed: 
  # X based on Lane Width. See table on AC Cracking Validation Tab.
  
  #browser()
  cracking_percent = data[dataitem=="CRACKING_PERCENT", 
                          .(routeid,beginpoint,endpoint,CRACKING_PERCENT=valuenumeric, num_sections)]
  lane_width = data[dataitem=="SURFACE_TYPE",
                    .(routeid,beginpoint,endpoint,SURFACE_TYPE=valuenumeric)]
  surface_type = data[dataitem=="LANE_WIDTH",
                      .(routeid,beginpoint,endpoint,LANE_WIDTH=valuenumeric)]
  
  if(cracking_percent[, .N] ==0 | lane_width[, .N] == 0 | surface_type[, .N] == 0 ){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = lane_width[cracking_percent, on=.(routeid,beginpoint,endpoint)]
  comparison = surface_type[comparison,on=.(routeid,beginpoint,endpoint)]

  # The max cracking percent based on the table on AC Cracking Validation tab
  max_cracking_pct <- c(rep(NA, 9),
                        81.30, 72.20, 65.00, 59.10, 54.20,
                        50.00, 46.40, 43.30, 40.60)
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = SURFACE_TYPE %in% c(2,6,7,8), 
               passes  = SURFACE_TYPE %in% c(2,6,7,8) & 
                 CRACKING_PERCENT <= max_cracking_pct[1 + LANE_WIDTH])][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
cross_validation_64 = function(data, variable){

  # ValueText Must Be In (A,B,C,D,E) Where ValueDate <> Year Record and
  # F_Sytem = 1 OR if ValueDate < Year Record -1 on NHS

  # variable %in% c('IRI', 'RUTTING', 'FAULTING', 'CRACKING_PERCENT')
  # browser()
  
  comparison = data[dataitem == variable,
                    .(routeid, beginpoint, endpoint, datayear, F_SYSTEM, NHS,
                      valuedate, begindate, valuetext, num_sections)]
  
  if(comparison[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       # .(applies = (year(valuedate) != datayear & F_SYTEMorig == 1) | (year(valuedate) < datayear - 1 & NHS > 1), 
                       .(applies = ( valuedate != begindate & F_SYSTEM == 1 ) | (year(valuedate) < year(begindate) - 1 & !is.na(NHS)), 
                         passes  = valuetext %in% c('A', 'B', 'C', 'D', 'E'))][order(applies,passes
                        )]
  
  

  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - no sections meet application criteria")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_65 = function(data){
    # ValueDate Must Must >= DataYear - 1 Where Sample OR F_System >1 and 
    # NHS in (1,2,3,4,5,6,7,8,9)

  comparison = data[dataitem == "PSR",
                    .(routeid, beginpoint, endpoint, datayear,
                      F_SYSTEM, NHS,
                      valuedate, begindate, sample = !is.na(expansionfactor), num_sections)]
  
  if(comparison[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = sample | (F_SYSTEM > 1 & NHS %in% 1:9), 
                         passes  =  (datayear <= 2020 & year(valuedate) >= datayear - 1) |
                                    (datayear >  2020 & year(valuedate) >= year(begindate) - 1)
                         
                         )
                        # passes  = year(valuedate) >= datayear - 1)
                       ][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
}

###################################################################
cross_validation_66 = function(data){

  # ValueDate Must = Year Record  Where ValueText is "A" AND F_System =1 


  comparison = data[dataitem == "PSR",
                    .(routeid, beginpoint, endpoint, datayear,
                      F_SYTEMorig, valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  if(comparison[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = valuetext == 'A' & F_SYTEMorig == 1, 
                         passes  = (datayear <= 2020 & year(valuedate) == datayear) |
                                   (datayear >  2020 & valuedate == begindate)  # FIXME: year() == year() ?
                         )][order(applies,passes)]
                       # passes  = year(valuedate) == datayear)][order(applies,passes)]

  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_x = function(data){
  # Counter_Peak_Lanes + Peak_Lanes Must Be >= Through Lanes
  # need to confirm when this should apply
  #browser()
  # Counter_Peak_Lanes and Peak_Lanes are SP
  through_lanes      = data[dataitem=="THROUGH_LANES",
                            .(routeid,beginpoint,endpoint,THROUGH_LANES=valuenumeric,num_sections)]
  counter_peak_lanes = data[dataitem=="COUNTER_PEAK_LANES",
                            .(routeid,beginpoint,endpoint,COUNTER_PEAK_LANES=valuenumeric)]
  peak_lanes         = data[dataitem=="PEAK_LANES",
                            .(routeid,beginpoint,endpoint,PEAK_LANES=valuenumeric)]
  
  facility_type      = data[dataitem == "FACILITY_TYPE",
                            .(routeid, beginpoint, endpoint, FACILITY_TYPE = valuenumeric )]

  if(through_lanes[, .N] == 0 | (counter_peak_lanes[, .N] + peak_lanes[, .N]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))
  }
  
  # join the two together
  comparison = counter_peak_lanes[through_lanes,on = .(routeid, beginpoint, endpoint)]
  comparison =         peak_lanes[comparison,   on = .(routeid, beginpoint, endpoint)]
  comparison =      facility_type[comparison,   on = .(routeid, beginpoint, endpoint)]
  
  # setting NAs to 0
  comparison[is.na(COUNTER_PEAK_LANES)&!is.na(PEAK_LANES),COUNTER_PEAK_LANES:=0]
  comparison[is.na(PEAK_LANES)&!is.na(COUNTER_PEAK_LANES),        PEAK_LANES:=0]
  
  # only apply to where we have counter or peak lanes
  comparison = comparison[!(is.na(PEAK_LANES)&is.na(COUNTER_PEAK_LANES)),]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = !is.na(THROUGH_LANES), # always applies if through_lanes exists
               passes  = COUNTER_PEAK_LANES + PEAK_LANES >= THROUGH_LANES |  # TODO: double check if this check still holds, or if only reporting necessary
                         ( COUNTER_PEAK_LANES == 0 & FACILITY_TYPE == 1 )
               )][order(applies,passes)]
              # passes  = COUNTER_PEAK_LANES + PEAK_LANES >= THROUGH_LANES )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where F_System = 1 and Urban Code <> 99999, Signal_Type should = 5
cross_validation_y = function(data){
  
  signal_type = data[dataitem=="SIGNAL_TYPE",
                     .(routeid,beginpoint,endpoint,SIGNAL_TYPE=valuenumeric, num_sections)]
  f_system = data[dataitem=="F_SYSTEM",
                  .(routeid,beginpoint,endpoint,F_SYSTEM=valuenumeric)]
  urban_id = data[dataitem=="URBAN_ID", 
                    .(routeid,beginpoint,endpoint,URBAN_ID=valuenumeric)]

  if(signal_type[, .N] == 0|f_system[, .N] == 0|urban_id[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = f_system[signal_type,on=.(routeid,beginpoint,endpoint)]
  comparison = urban_id[comparison,   on=.(routeid,beginpoint,endpoint)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(endpoint-beginpoint)
              ),
             .(applies = F_SYSTEM == 1 & URBAN_ID != 99999 , 
               passes  = SIGNAL_TYPE == 5 )][order(applies,passes)]

  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}


###################################################################
# Measurement checks
# Section Length Must Not Be > 0.11 Miles
# IRI
# PSR
# Rutting
# Faulting
# Cracking_Percent


measurement_checks = function(data){

  comparison = data[dataitem %in% c("IRI", "PSR", "RUTTING", "FAULTING", "CRACKING_PERCENT"),
                    .(routeid, section_id, beginpoint_og, endpoint_og, dataitem)]
  comparison <- unique(comparison)
  comparison[, sectionlength := endpoint_og - beginpoint_og]
  
  results = comparison[, .(num_sections = .N, mileage = sum(sectionlength)),
                       .(dataitem, passes = sectionlength <= 0.11)][order(dataitem,passes)]
  
  results[, c("section_total", "mileage_total") := .(sum(num_sections), sum(mileage)), .(dataitem)]
  results[, c("sections_pass", "mileage_pass") := .(num_sections / section_total, mileage/mileage_total)]
  results = results[passes==TRUE]
  results[,.id:='129']
  results[,c("num_sections","mileage","passes"):=NULL]
  results[,dataitem:=paste0(dataitem, ": Mileage measurement (<0.11 miles)")]
  setnames(results,"dataitem","Description")
  
  setcolorder(results,c(".id","Description","section_total","mileage_total","sections_pass","mileage_pass" ))
  return(results)

}


