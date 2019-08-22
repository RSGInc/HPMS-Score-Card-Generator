

calc_cross_validation = function(data, year){
  
  # filter ramps (facility type == 4) 
  data = data[year_record == year & FACILITY_TYPE %in% c(1,2)]
  
  # Tests commented out are evaluated as outliers
  results = list()
  results[["53"]] = summarize_validation(cross_validation_53(data))
  results[["x"]]  = summarize_validation(cross_validation_x(data))
  results[["16"]] = summarize_validation(cross_validation_16(data))
  results[["15"]] = summarize_validation(cross_validation_15(data))
  results[["45"]] = summarize_validation(cross_validation_45(data))
  # results[["55"]] = summarize_validation(cross_validation_55(data))
  results[["17"]] = summarize_validation(cross_validation_17(data))
  results[["42"]] = summarize_validation(cross_validation_42(data))
  # results[["57"]] = summarize_validation(cross_validation_57(data))
  results[["44"]] = summarize_validation(cross_validation_44(data))
  # results[["54"]] = summarize_validation(cross_validation_54(data))
  results[["43"]] = summarize_validation(cross_validation_43(data))
  # results[["56"]] = summarize_validation(cross_validation_56(data))
  # results[["49"]] = summarize_validation(cross_validation_49(data))
  results[["39"]] = summarize_validation(cross_validation_39(data))
  results[["40"]] = summarize_validation(cross_validation_40(data))
  results[["41"]] = summarize_validation(cross_validation_41(data))
  results[["y"]]  = summarize_validation(cross_validation_y(data))
  # results[["14"]] = summarize_validation(cross_validation_14(data))
  results[["22"]] = summarize_validation(cross_validation_22(data))
  results[["20"]] = summarize_validation(cross_validation_20(data))
  results[["60"]] = summarize_validation(cross_validation_60(data))
  results[["23"]] = summarize_validation(cross_validation_23(data))

  results[["46.1"]] = summarize_validation(cross_validation_46(data, 'IRI'))
  results[["46.2"]] = summarize_validation(cross_validation_46(data, 'RUTTING'))
  results[["46.3"]] = summarize_validation(cross_validation_46(data, 'FAULTING'))
  results[["46.4"]] = summarize_validation(cross_validation_46(data, 'CRACKING_PERCENT'))

  results[["61.1"]] = summarize_validation(cross_validation_61(data, 'IRI'))
  results[["61.2"]] = summarize_validation(cross_validation_61(data, 'RUTTING'))
  results[["61.3"]] = summarize_validation(cross_validation_61(data, 'FAULTING'))
  results[["61.4"]] = summarize_validation(cross_validation_61(data, 'CRACKING_PERCENT'))

  results[["64.1"]] = summarize_validation(cross_validation_64(data, 'IRI'))
  results[["64.2"]] = summarize_validation(cross_validation_64(data, 'RUTTING'))
  results[["64.3"]] = summarize_validation(cross_validation_64(data, 'FAULTING'))
  results[["64.4"]] = summarize_validation(cross_validation_64(data, 'CRACKING_PERCENT'))

  results[["65"]] = summarize_validation(cross_validation_65(data))
  results[["66"]] = summarize_validation(cross_validation_66(data))
  # results[["1"]]  = summarize_validation(cross_validation_1(data))
  results[["62"]] = summarize_validation(cross_validation_62(data))
  # results[["52"]] = summarize_validation(cross_validation_52(data))
  # results[["47"]] = summarize_validation(cross_validation_47(data))
  results[["63"]] = summarize_validation(cross_validation_63(data))
  results[["51"]] = summarize_validation(cross_validation_51(data))
  results[["9"]]  = summarize_validation(cross_validation_9(data))
  results[["2"]]  = summarize_validation(cross_validation_2(data))
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
cross_validation_53 = function(data){
  # Through_Lanes>1 when Facility_Type = 2
  
  through_lanes = data[data_item=="THROUGH_LANES",.(route_id,begin_point,end_point,THROUGH_LANES=value_numeric,num_sections)]
  facility_type = data[data_item=="FACILITY_TYPE",.(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric)]

  if(nrow(through_lanes)==0|nrow(facility_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = facility_type[through_lanes,on=.(route_id,begin_point,end_point)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_x = function(data){
  # Counter_Peak_Lanes + Peak_Lanes Must Be >= Through Lanes
  # need to confirm when this should apply
  
  # Counter_Peak_Lanes and Peak_Lanes are SP
  through_lanes      = data[data_item=="THROUGH_LANES",
                            .(route_id,begin_point,end_point,THROUGH_LANES=value_numeric,num_sections)]
  counter_peak_lanes = data[data_item=="COUNTER_PEAK_LANES",
                            .(route_id,begin_point,end_point,COUNTER_PEAK_LANES=value_numeric)]
  peak_lanes         = data[data_item=="PEAK_LANES",
                            .(route_id,begin_point,end_point,PEAK_LANES=value_numeric)]

  if(nrow(through_lanes)==0|(nrow(counter_peak_lanes)+nrow(peak_lanes))==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))
  }
  
  # join the two together
  comparison = counter_peak_lanes[through_lanes,on=.(route_id,begin_point,end_point)]
  comparison =         peak_lanes[comparison,   on=.(route_id,begin_point,end_point)]
  
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
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(THROUGH_LANES), # always applies if through_lanes exists
               passes  = COUNTER_PEAK_LANES + PEAK_LANES >= THROUGH_LANES )][order(applies,passes)]
  
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
  
  facility_type      = data[data_item=="FACILITY_TYPE",
                            .(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric,num_sections)]
  counter_peak_lanes = data[data_item=="COUNTER_PEAK_LANES",
                            .(route_id,begin_point,end_point,COUNTER_PEAK_LANES=value_numeric)]
  
  if(nrow(facility_type)==0|nrow(counter_peak_lanes)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = counter_peak_lanes[facility_type,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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
cross_validation_15 = function(data){
  # SPEED_LIMIT should be divisible by 5, and < 90 OR = 999
  # should it include a check of 0?
  
  #browser()
  speed_limit = data[data_item=="SPEED_LIMIT",.(route_id,begin_point,end_point,SPEED_LIMIT=value_numeric, num_sections)]
  
  if(nrow(speed_limit)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  comparison = speed_limit

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(SPEED_LIMIT), 
               passes  = (SPEED_LIMIT==999)|(SPEED_LIMIT%%5==0&SPEED_LIMIT<90))][order(applies,passes)]
  
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
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",.(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric, num_sections)]
  aadt = data[data_item=="AADT",.(route_id,begin_point,end_point,AADT=value_numeric)]
  
  if(nrow(aadt_single_unit)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt[aadt_single_unit,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_55 = function(data){
  # AADT_Single_Unit Shoud be > 0
  
  #browser()
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",
                          .(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric, num_sections)]
  
  if(nrow(aadt_single_unit)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = aadt_single_unit
 
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(AADT_SINGLE_UNIT), 
               passes  = AADT_SINGLE_UNIT >0 )][order(applies,passes)]
  
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
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",
                          .(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric, num_sections)]
  aadt_combination = data[data_item=="AADT_COMBINATION",
                          .(route_id,begin_point,end_point,AADT_COMBINATION=value_numeric)]
  aadt = data[data_item=="AADT",
              .(route_id,begin_point,end_point,AADT=value_numeric)]
  
  if(nrow(aadt_single_unit)==0|nrow(aadt_combination)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt_combination[aadt_single_unit,on=.(route_id,begin_point,end_point)]
  comparison = aadt[comparison,   on=.(route_id,begin_point,end_point)]
  
  # setting NAs to 0
  comparison[is.na(AADT_SINGLE_UNIT)&!is.na(AADT_COMBINATION),AADT_SINGLE_UNIT:=0]
  comparison[is.na(AADT_COMBINATION)&!is.na(AADT_SINGLE_UNIT),AADT_COMBINATION:=0]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_42 = function(data){
  # (SU AADT x 0.04) < (AADT x Percent Peak SU) < (SU AADT x 0.4) 
  # AADT * PCT_PEAK_SINGLE > (AADT_SINGLE_UNIT * 0.04) &
  #  AADT * PCT_PEAK_SINGLE < (AADT_SINGLE_UNIT * 0.4)
  #browser()
  pct_peak_single = data[data_item=="PCT_PEAK_SINGLE",
                         .(route_id,begin_point,end_point,PCT_PEAK_SINGLE=value_numeric, num_sections)]
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",
                          .(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric)]
  aadt = data[data_item=="AADT",
              .(route_id,begin_point,end_point,AADT=value_numeric)]
  
  if(nrow(pct_peak_single)==0|nrow(aadt_single_unit)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt_single_unit[pct_peak_single,on=.(route_id,begin_point,end_point)]
  comparison = aadt[comparison,   on=.(route_id,begin_point,end_point)]
  
  # setting NAs to 0
  #comparison[is.na(AADT_SINGLE_UNIT)&!is.na(AADT_COMBINATION),AADT_SINGLE_UNIT:=0]
  #comparison[is.na(AADT_COMBINATION)&!is.na(AADT_SINGLE_UNIT),AADT_COMBINATION:=0]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(PCT_PEAK_SINGLE), 
               passes  = AADT * PCT_PEAK_SINGLE > (AADT_SINGLE_UNIT * 0.04) &
                 AADT * PCT_PEAK_SINGLE < (AADT_SINGLE_UNIT * 0.4))][order(applies,passes)]
  
  
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
  pct_peak_single = data[data_item=="PCT_PEAK_SINGLE",
                         .(route_id,begin_point,end_point,PCT_PEAK_SINGLE=value_numeric, num_sections)]

  if(nrow(pct_peak_single)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = pct_peak_single
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(PCT_PEAK_SINGLE), 
               passes  = PCT_PEAK_SINGLE > 0 & PCT_PEAK_SINGLE < 25)][order(applies,passes)]
  
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
  aadt_combination = data[data_item=="AADT_COMBINATION",
                          .(route_id,begin_point,end_point,AADT_COMBINATION=value_numeric, num_sections)]
  aadt = data[data_item=="AADT",
              .(route_id,begin_point,end_point,AADT=value_numeric)]
  
  if(nrow(aadt_combination)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt[aadt_combination,on=.(route_id,begin_point,end_point)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_54 = function(data){
  # AADT_Combination Should be > 0 
  
  #browser()
  aadt_combination = data[data_item=="AADT_COMBINATION",
                          .(route_id, begin_point, end_point, AADT_COMBINATION=value_numeric, num_sections)]
  
  if(nrow(aadt_combination)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = aadt_combination
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(AADT_COMBINATION), # always applies if through_lanes exists
               passes  = AADT_COMBINATION > 0 )][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################

cross_validation_43 = function(data){
  #(CU AADT x 0.04) < (AADT x Percent Peak CU) < (CU AADT x 0.4)
  # (AADT * PCT_PEAK_COMBINATION) > (AADT_COMBINATION * 0.04) &
  #   (AADT * PCT_PEAK_COMBINATION) < (AADT_COMBINATION * 0.4)
  #browser()
  pct_combination = data[data_item=="PCT_PEAK_COMBINATION",
                         .(route_id,begin_point,end_point,PCT_PEAK_COMBINATION=value_numeric, num_sections)]
  aadt_combination = data[data_item=="AADT_COMBINATION",
                          .(route_id,begin_point,end_point,AADT_COMBINATION=value_numeric)]
  aadt = data[data_item=="AADT",
              .(route_id,begin_point,end_point,AADT=value_numeric)]

  if(nrow(pct_combination)==0|nrow(aadt_combination)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = aadt_combination[pct_combination,on=.(route_id,begin_point,end_point)]
  comparison = aadt[comparison,   on=.(route_id,begin_point,end_point)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(PCT_PEAK_COMBINATION), 
               passes  = (AADT * PCT_PEAK_COMBINATION) > (AADT_COMBINATION * 0.04) &
                 (AADT * PCT_PEAK_COMBINATION) < (AADT_COMBINATION * 0.4)
             )][order(applies,passes)]
  
  
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
  pct_peak_combination = data[data_item=="PCT_PEAK_COMBINATION",
                              .(route_id,begin_point,end_point,PCT_PEAK_COMBINATION=value_numeric, num_sections)]
  
  if(nrow(pct_peak_combination)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = pct_peak_combination
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(PCT_PEAK_COMBINATION), 
               passes  = PCT_PEAK_COMBINATION > 0 & PCT_PEAK_COMBINATION < 25)][order(applies,passes)]
 
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################

cross_validation_49 = function(data){
  # K_Factor must be > 4 and <20 
  
  #browser()
  k_factor = data[data_item=="K_FACTOR",
                  .(route_id,begin_point,end_point,K_FACTOR=value_numeric, num_sections)]

  if(nrow(k_factor)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = k_factor
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(K_FACTOR), 
               passes  = K_FACTOR > 4 & K_FACTOR < 20)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################

cross_validation_39 = function(data){
  # DIR_Factor must be 100 where Facility_Type = 1
  
  #browser()
  facility_type = data[data_item=="FACILITY_TYPE",
                       .(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric, num_sections)]
  dir_factor = data[data_item=="DIR_FACTOR",
                    .(route_id,begin_point,end_point,DIR_FACTOR=value_numeric)]
  
  if(nrow(facility_type)==0|nrow(dir_factor)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
 
  # join the two together
  comparison = dir_factor[facility_type,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = FACILITY_TYPE==1&!is.na(DIR_FACTOR), # DIR_FACTOR is SP 
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
  facility_type = data[data_item=="FACILITY_TYPE",
                       .(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric, num_sections)]
  dir_factor = data[data_item=="DIR_FACTOR",
                    .(route_id,begin_point,end_point,DIR_FACTOR=value_numeric)]
  
  if(nrow(facility_type)==0|nrow(dir_factor)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = dir_factor[facility_type,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = FACILITY_TYPE==2&!is.na(DIR_FACTOR), # DIR_FACTOR is SP
               passes  = DIR_FACTOR>=50&DIR_FACTOR<=70)][order(applies,passes)]
  
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
  future_aadt = data[data_item=="FUTURE_AADT",
                     .(route_id, begin_point, end_point, FUTURE_AADT=value_numeric, num_sections)]
  aadt = data[data_item=="AADT",
              .(route_id,begin_point,end_point,AADT=value_numeric)]

  if(nrow(future_aadt)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = aadt[future_aadt,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(FUTURE_AADT), 
               passes  = FUTURE_AADT > AADT  & FUTURE_AADT < 4 * AADT)][order(applies,passes)] 
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where F_System = 1 and Urban Code <> 99999, Signal_Type should = 5

cross_validation_y = function(data){
  
  signal_type = data[data_item=="SIGNAL_TYPE",
                     .(route_id,begin_point,end_point,SIGNAL_TYPE=value_numeric, num_sections)]
  f_system = data[data_item=="F_SYSTEM",
                  .(route_id,begin_point,end_point,F_SYSTEM=value_numeric)]
  urban_code = data[data_item=="URBAN_CODE", 
                    .(route_id,begin_point,end_point,URBAN_CODE=value_numeric)]

  if(nrow(signal_type)==0|nrow(f_system)==0|nrow(urban_code)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # join the two together
  comparison = f_system[signal_type,on=.(route_id,begin_point,end_point)]
  comparison = urban_code[comparison,   on=.(route_id,begin_point,end_point)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = F_SYSTEM == 1 & URBAN_CODE != 99999 , 
               passes  = SIGNAL_TYPE == 5 )][order(applies,passes)]

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
  lane_width = data[data_item=="LANE_WIDTH",
                    .(route_id,begin_point,end_point,LANE_WIDTH=value_numeric, num_sections)]
  
  if(nrow(lane_width)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  comparison = lane_width
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_22 = function(data){
  # Median Type in (2,3,4,5,6,7)	Median Width > 0
  
  #browser()
  median_width = data[data_item=="MEDIAN_TYPE",
                      .(route_id,begin_point,end_point,MEDIAN_TYPE=value_numeric, num_sections)]
  median_type = data[data_item=="MEDIAN_WIDTH",
                     .(route_id,begin_point,end_point,MEDIAN_WIDTH=value_numeric)]

  if(nrow(median_width)==0|nrow(median_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = median_type[median_width,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_20 = function(data){
  # Median Width Null if (FACILITY_TYPE is 1 or 4) or Median_Type Code <2
  # what does NULL mean? 0 or not reported or reported but NULL?
  
  #browser()
  median_width = data[data_item=="MEDIAN_TYPE",
                      .(route_id,begin_point,end_point,MEDIAN_TYPE=value_numeric, num_sections)]
  median_type = data[data_item=="MEDIAN_WIDTH",
                     .(route_id,begin_point,end_point,MEDIAN_WIDTH=value_numeric)]
  facility_type = data[data_item=="FACILITY_TYPE",
                       .(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric)]

  if(nrow(median_width)==0|nrow(median_type)==0|nrow(facility_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = median_type[median_width,on=.(route_id,begin_point,end_point)]
  comparison = facility_type[comparison,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_60 = function(data){
  # Shoulder_Width_L Should be < Median_Width
  
  #browser()
  left_shoulder_width = data[data_item=="SHOULDER_WIDTH_L",
                             .(route_id,begin_point,end_point,SHOULDER_WIDTH_L=value_numeric, num_sections)]
  median_width = data[data_item=="MEDIAN_WIDTH",
                      .(route_id,begin_point,end_point,MEDIAN_WIDTH=value_numeric)]
  
  if(nrow(left_shoulder_width)==0|nrow(median_width)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = median_width[left_shoulder_width,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(SHOULDER_WIDTH_L), 
               passes  = SHOULDER_WIDTH_L<MEDIAN_WIDTH)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results,comparison=comparison))

}

###################################################################

cross_validation_23 = function(data){
  # Widening_Obstacle must contain A-G where Widening_Potential <9
  
  #browser()
  widening_obstacle = data[data_item=="WIDENING_OBSTACLE", 
                           .(route_id,begin_point,end_point,WIDENING_OBSTACLE=value_text, num_sections)]
  widening_potential = data[data_item=="WIDENING_POTENTIAL",
                            .(route_id,begin_point,end_point,WIDENING_POTENTIAL=value_numeric)]
  
  if(nrow(widening_obstacle)==0 | nrow(widening_potential)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = widening_potential[widening_obstacle,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_46 = function(data, variable){

  # Value_Date must >= Year_Record - 1 where (sample | (Value_Text is NULL and F_System > 1 and
  # NHS in (1,2,3,4,5,6,7,8,9))

  # variable %in% c('IRI', 'RUTTING', 'FAULTING', 'CRACKING_PERCENT')
  
  #browser()
  
  comparison = data[data_item == variable,
               .(route_id, begin_point, end_point, year_record, F_SYTEMorig, NHS,
                 value_date, value_text, sample = !is.na(expansion_factor), num_sections)]
  
  if(nrow(comparison) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  

  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(end_point-begin_point)
                       ),
                       .(applies = sample | (is.na(value_text) & F_SYTEMorig > 1 & NHS %in% 1:9), 
                         passes  = value_date >= year_record - 1)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
}

###################################################################
# Value_Date Must = Year Record  Where Value_Text is Null AND F_System =1 

cross_validation_61 = function(data, variable){
  # variable %in% c('IRI', 'RUTTING', 'FAULTING', 'CRACKING_PERCENT')
  
  comparison = data[data_item == variable,
                    .(route_id, begin_point, end_point, year_record, F_SYTEMorig,
                      value_date, value_text, num_sections)]
  
  if(nrow(comparison) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(end_point-begin_point)
                       ),
                       .(applies = is.na(value_text) & F_SYTEMorig == 1, 
                         passes  = value_date >= year_record - 2)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - no sections meet application criteria")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
}

###################################################################

cross_validation_64 = function(data, variable){

  # Value_Text Must Be In (A,B,C,D,E) Where Value_Date <> Year Record and
  # F_Sytem = 1 OR if Value_Date < Year Record -1 on NHS

  # variable %in% c('IRI', 'RUTTING', 'FAULTING', 'CRACKING_PERCENT')
  
  comparison = data[data_item == variable,
                    .(route_id, begin_point, end_point, year_record, F_SYTEMorig, NHS,
                      value_date, value_text, num_sections)]
  
  if(nrow(comparison) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(end_point-begin_point)
                       ),
                       .(applies = (value_date != year_record & F_SYTEMorig == 1) | (value_date < year_record - 1 & NHS > 1), 
                         passes  = value_text %in% c('A', 'B', 'C', 'D', 'E'))][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - no sections meet application criteria")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}


###################################################################

cross_validation_65 = function(data){
    # Value_Date Must Must >= Year_Record - 1 Where Sample OR F_System >1 and 
    # NHS in (1,2,3,4,5,6,7,8,9)

  comparison = data[data_item == "PSR",
                    .(route_id, begin_point, end_point, year_record,
                      F_SYTEMorig, NHS,
                      value_date, sample = !is.na(expansion_factor), num_sections)]
  
  if(nrow(comparison) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(end_point-begin_point)
                       ),
                       .(applies = sample | (F_SYTEMorig > 1 & NHS %in% 1:9), 
                         passes  = value_date >= year_record - 1)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE])==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
}

###################################################################
cross_validation_66 = function(data){

  # Value_Date Must = Year Record  Where Value_Text is "A" AND F_System =1 


  comparison = data[data_item == "PSR",
                    .(route_id, begin_point, end_point, year_record,
                      F_SYTEMorig, value_text, value_date, num_sections)]
  
  if(nrow(comparison) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(end_point-begin_point)
                       ),
                       .(applies = value_text == 'A' & F_SYTEMorig == 1, 
                         passes  = value_date == year_record)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}



###################################################################
cross_validation_1 = function(data){
  
  # IRI >= 30 and <= 400
  #browser()
  iri = data[data_item=="IRI",
             .(route_id, begin_point, end_point, IRI=value_numeric, num_sections)]
  
  if(nrow(iri)==0){
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
               mileage      = sum(end_point-begin_point)
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

cross_validation_62 = function(data){

  # Where F_System =1, and IRI is Null, 
  # PSR Value_Numeric Must be >0 and PSR Value_Text must = A

  psr = data[data_item == "PSR",
                    .(route_id, begin_point, end_point,
                      F_SYTEMorig, value_text, value_numeric, num_sections)]
  
  iri = data[data_item == 'IRI',
             .(route_id, begin_point, end_point, IRI = value_numeric)]
  
  if(nrow(psr) == 0 | nrow(iri) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = iri[psr, on=.(route_id, begin_point, end_point)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(end_point-begin_point)
                       ),
                       .(applies = is.na(IRI) & F_SYTEMorig == 1, 
                         passes  = value_numeric > 0 & value_text == 'A')][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################

cross_validation_52 = function(data){
  # Rutting should be < 1
  
  #browser()
  rutting = data[data_item=="RUTTING",
                 .(route_id,begin_point,end_point,RUTTING=value_numeric, num_sections)]
  
  if(nrow(rutting)==0){
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
               mileage      = sum(end_point-begin_point)
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

cross_validation_47 = function(data){
  # Faulting should be <= 1
  
  #browser()
  faulting = data[data_item=="FAULTING",
                  .(route_id,begin_point,end_point,FAULTING=value_numeric, num_sections)]
  
  if(nrow(faulting)==0){
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
               mileage      = sum(end_point-begin_point)
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

cross_validation_63 = function(data){
  # Where Surface Type is in (2,6,7,8) Cracking Percent should not exceed: 
  # X based on Lane Width. See table on AC Cracking Validation Tab.
  
  #browser()
  cracking_percent = data[data_item=="CRACKING_PERCENT", 
                          .(route_id,begin_point,end_point,CRACKING_PERCENT=value_numeric, num_sections)]
  lane_width = data[data_item=="SURFACE_TYPE",
                    .(route_id,begin_point,end_point,SURFACE_TYPE=value_numeric)]
  surface_type = data[data_item=="LANE_WIDTH",
                      .(route_id,begin_point,end_point,LANE_WIDTH=value_numeric)]
  
  if(nrow(cracking_percent)==0|nrow(lane_width)==0|nrow(surface_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = lane_width[cracking_percent,on=.(route_id,begin_point,end_point)]
  comparison = surface_type[comparison,on=.(route_id,begin_point,end_point)]

  # The max cracking percent based on the table on AC Cracking Validation tab
  max_cracking_pct <- c(rep(NA, 9),
                        81.30, 72.20, 65.00, 59.10, 54.20,
                        50.00, 46.40, 43.30, 40.60)
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_51 = function(data){
  # Where Surface Type is in (3,4,5,9,10) Cracking Percent should be < 75
  
  #browser()
  cracking_percent = data[data_item=="CRACKING_PERCENT", 
                          .(route_id,begin_point,end_point,CRACKING_PERCENT=value_numeric, num_sections)]
  surface_type = data[data_item=="SURFACE_TYPE",
                      .(route_id,begin_point,end_point,SURFACE_TYPE=value_numeric)]
  
  if(nrow(cracking_percent)==0|nrow(surface_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = surface_type[cracking_percent,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
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

cross_validation_9 = function(data){
  # Year_Last_Construction	<= Year_Record or NULL
  
  #browser()
  year_last_construction = data[data_item=="YEAR_LAST_CONSTRUCTION",
                                .(route_id,begin_point,end_point,YEAR_LAST_CONSTRUCTION=value_date, year_record, num_sections)]

  if(nrow(year_last_construction)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = year_last_construction

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = !is.na(YEAR_LAST_CONSTRUCTION), 
               passes  = !is.na(YEAR_LAST_CONSTRUCTION)&year(YEAR_LAST_CONSTRUCTION)<=year_record)][order(applies,passes)]
  
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
  
  comparison = data[, 
                    .(route_id, begin_point, end_point, num_sections,
                      sample = !is.na(expansion_factor), F_SYTEMorig, 
                      URBAN_CODE, FACILITY_TYPE)]
  
  if(nrow(comparison)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(end_point-begin_point)
                       ),
                       .(applies = !(FACILITY_TYPE %in% c(1, 2) &
                                       (F_SYTEMorig %in% 1:5 | (F_SYTEMorig == 6 & URBAN_CODE < 99999)))  , 
                         passes  = !sample)][order(applies,passes)]
  
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

  comparison = data[data_item %in% c("IRI", "PSR", "RUTTING", "FAULTING", "CRACKING_PERCENT"),
                    .(route_id, section_id, begin_point_og, end_point_og, data_item)]
  comparison <- unique(comparison)
  comparison[, section_length := end_point_og - begin_point_og]
  
  results = comparison[, .(num_sections = .N, mileage = sum(section_length)),
                       .(data_item, passes = section_length <= 0.11)][order(data_item,passes)]
  
  results[, c("section_total", "mileage_total") := .(sum(num_sections), sum(mileage)), .(data_item)]
  results[, c("sections_pass", "mileage_pass") := .(num_sections / section_total, mileage/mileage_total)]
  results = results[passes==TRUE]
  results[,.id:='129']
  results[,c("num_sections","mileage","passes"):=NULL]
  results[,data_item:=paste0(data_item, ": Mileage measurement (<0.11 miles)")]
  setnames(results,"data_item","Description")
  
  setcolorder(results,c(".id","Description","section_total","mileage_total","sections_pass","mileage_pass" ))
  return(results)

}


