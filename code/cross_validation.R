

###################################################################
# summarize the results of the validation
summarize_validation = function(results){
  
 if(is.null(results[["results"]])){
   return(
     data.table(section_total=NA, 
                mileage_total=NA,
                sections_pass=NA,
                mileage_pass=NA)
     ) 
 }
 
 if(nrow(results[["results"]][applies==TRUE&passes==TRUE])==0){
   results[["results"]]=rbindlist(list(
     results[["results"]],
     data.table(applies=TRUE,passes=TRUE,N=0,num_sections=0,mileage=0))
    )
 }
 results[["results"]][applies==TRUE,c("section_total","mileage_total"):=.(sum(num_sections),sum(mileage))]  
  
 return(results[["results"]][applies==TRUE&passes==TRUE,.(section_total, mileage_total,sections_pass=num_sections/section_total,mileage_pass=mileage/mileage_total)])   
}

# -----------------------------------------------------------------------

###################################################################
# Through_Lanes>1 when Facility_Type = 2
cross_validation_53 = function(data){
  
  through_lanes = data[data_item=="THROUGH_LANES",.(route_id,begin_point,end_point,THROUGH_LANES=value_numeric,num_sections,section_extent)]
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Counter_Peak_Lanes + Peak_Lanes Must Be >= Through Lanes
# need to confirm when this should apply

# Counter_Peak_Lanes and Peak_Lanes are SP

cross_validation_x = function(data){
  
  through_lanes      = data[data_item=="THROUGH_LANES",
                            .(route_id,begin_point,end_point,THROUGH_LANES=value_numeric,num_sections)]
  counter_peak_lanes = data[!is.na(expansion_factor)&
                              section_extent%in%c("SP","SP*")&
                              data_item=="COUNTER_PEAK_LANES",
                            .(route_id,begin_point,end_point,COUNTER_PEAK_LANES=value_numeric)]
  peak_lanes         = data[!is.na(expansion_factor)&
                              section_extent%in%c("SP","SP*")&
                              data_item=="PEAK_LANES",
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Counter_Peak_Lanes is NULL if FACILITY_TYPE is 1

# Counter Peak Lanes is SP
cross_validation_16 = function(data){
  
  facility_type      = data[data_item=="FACILITY_TYPE",
                            .(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric,num_sections)]
  counter_peak_lanes = data[!is.na(expansion_factor)&
                              section_extent%in%c("SP","SP*")&
                              data_item=="COUNTER_PEAK_LANES",
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# SPEED_LIMIT should be divisible by 5, and < 90 OR = 999
# should it include a check of 0?
cross_validation_15 = function(data){
  
  #browser()
  speed_limit = data[data_item=="SPEED_LIMIT",.(route_id,begin_point,end_point,SPEED_LIMIT=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Single_Unit < AADT/2.5

cross_validation_45 = function(data){
  
  #browser()
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",.(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric)]
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
               passes  = AADT_SINGLE_UNIT < AADT/2.5)][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Single_Unit Shoud be > 0

cross_validation_55 = function(data){
  
  #browser()
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",.(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# SU AADT + CU AADT < (0.8*AADT)
# need to confirm when this should apply

cross_validation_17 = function(data){
  
  #browser()
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",.(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric)]
  aadt_combination = data[data_item=="AADT_COMBINATION",.(route_id,begin_point,end_point,AADT_COMBINATION=value_numeric)]
  aadt = data[data_item=="AADT",.(route_id,begin_point,end_point,AADT=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT*PCT_Peak_Single/100 < =AADT_Single_Unit 
# is the division by 100 necessary?

cross_validation_42 = function(data){
  
  #browser()
  pct_peak_single = data[data_item=="PCT_PEAK_SINGLE",.(route_id,begin_point,end_point,PCT_PEAK_SINGLE=value_numeric)]
  aadt_single_unit = data[data_item=="AADT_SINGLE_UNIT",.(route_id,begin_point,end_point,AADT_SINGLE_UNIT=value_numeric)]
  aadt = data[data_item=="AADT",.(route_id,begin_point,end_point,AADT=value_numeric)]
  
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
               passes  = AADT * PCT_PEAK_SINGLE / 100 <= AADT_SINGLE_UNIT)][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# PCT_Peak_Single >0 and < 20%
# should it be greater than 0 or equal to 0?

cross_validation_57 = function(data){
  
  #browser()
  pct_peak_single = data[data_item=="PCT_PEAK_SINGLE",.(route_id,begin_point,end_point,PCT_PEAK_SINGLE=value_numeric)]

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
               passes  = PCT_PEAK_SINGLE > 0 & PCT_PEAK_SINGLE < 20)][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Combination < AADT/2.5
# need to confirm when this should apply

cross_validation_44 = function(data){
  
  #browser()
  aadt_combination = data[data_item=="AADT_COMBINATION",.(route_id,begin_point,end_point,AADT_COMBINATION=value_numeric)]
  aadt = data[data_item=="AADT",.(route_id,begin_point,end_point,AADT=value_numeric)]
  
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
               passes  = AADT_COMBINATION < (AADT/2.5) )][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Combination Should be > 0 
# need to confirm when this should apply

cross_validation_54 = function(data){
  
  #browser()
  aadt_combination = data[data_item=="AADT_COMBINATION",.(route_id,begin_point,end_point,AADT_COMBINATION=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT*PCT_Peak_Single/100 < =AADT_Single_Unit 
# is the division by 100 necessary?

cross_validation_43 = function(data){
  
  #browser()
  pct_combination = data[data_item=="PCT_PEAK_COMBINATION",.(route_id,begin_point,end_point,PCT_PEAK_COMBINATION=value_numeric)]
  aadt_combination = data[data_item=="AADT_COMBINATION",.(route_id,begin_point,end_point,AADT_COMBINATION=value_numeric)]
  aadt = data[data_item=="AADT",.(route_id,begin_point,end_point,AADT=value_numeric)]

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
               passes  = AADT * PCT_PEAK_COMBINATION / 100 <= AADT_COMBINATION)][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# PCT_Peak_Combination >0 and < 20%
# should it be greater than 0 or equal to 0?

cross_validation_56 = function(data){
  
  #browser()
  pct_peak_combination = data[data_item=="PCT_PEAK_COMBINATION",.(route_id,begin_point,end_point,PCT_PEAK_COMBINATION=value_numeric)]
  
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
               passes  = PCT_PEAK_COMBINATION > 0 & PCT_PEAK_COMBINATION < 20)][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# K_Factor must be > 4 and <20 

cross_validation_49 = function(data){
  
  #browser()
  k_factor = data[data_item=="K_FACTOR",.(route_id,begin_point,end_point,K_FACTOR=value_numeric)]

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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# DIR_Factor must be 100 where Facility_Type = 1

cross_validation_39 = function(data){
  
  #browser()
  facility_type = data[data_item=="FACILITY_TYPE",.(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric)]
  dir_factor = data[data_item=="DIR_FACTOR",.(route_id,begin_point,end_point,DIR_FACTOR=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# DIR_Factor must be 50=< and <=70 where Facility_Type = 2

cross_validation_40 = function(data){
  
  #browser()
  facility_type = data[data_item=="FACILITY_TYPE",.(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric)]
  dir_factor = data[data_item=="DIR_FACTOR",.(route_id,begin_point,end_point,DIR_FACTOR=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT < FAADT < 3*AADT
# this is done elsewhere in the scorecard

cross_validation_41 = function(data){
  
  #browser()
  future_aadt = data[data_item=="FUTURE_AADT",.(route_id,begin_point,end_point,FUTURE_AADT=value_numeric)]
  aadt = data[data_item=="AADT",.(route_id,begin_point,end_point,AADT=value_numeric)]

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
               passes  = FUTURE_AADT > AADT  & FUTURE_AADT < 3 * AADT)][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where F_System = 1 and Urban Code <> 99999, Signal_Type should = 5 or Null
# need to confirm when this should apply

cross_validation_y = function(data){
  
  signal_type = data[data_item=="SIGNAL_TYPE",.(route_id,begin_point,end_point,SIGNAL_TYPE=value_numeric)]
  f_system = data[data_item=="F_SYSTEM",.(route_id,begin_point,end_point,F_SYSTEM=value_numeric)]
  urban_code = data[data_item=="URBAN_CODE",.(route_id,begin_point,end_point,URBAN_CODE=value_numeric)]

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
               passes  = SIGNAL_TYPE==5 | is.na(SIGNAL_TYPE) )][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Lande width must be > 5 and <19 

cross_validation_14 = function(data){
  
  #browser()
  lane_width = data[data_item=="LANE_WIDTH",.(route_id,begin_point,end_point,LANE_WIDTH=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Median Type in (2,3,4,5,6,7)	Median Width > 0

cross_validation_22 = function(data){
  
  #browser()
  median_width = data[data_item=="MEDIAN_TYPE",.(route_id,begin_point,end_point,MEDIAN_TYPE=value_numeric)]
  median_type = data[data_item=="MEDIAN_WIDTH",.(route_id,begin_point,end_point,MEDIAN_WIDTH=value_numeric)]

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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Median Width Null if (FACILITY_TYPE is 1 or 4) or Median_Type Code <2
# what does NULL mean? 0 or not reported or reported but NULL?

cross_validation_20 = function(data){
  
  #browser()
  median_width = data[data_item=="MEDIAN_TYPE",  .(route_id,begin_point,end_point,MEDIAN_TYPE=value_numeric)]
  median_type = data[data_item=="MEDIAN_WIDTH", .(route_id,begin_point,end_point,MEDIAN_WIDTH=value_numeric)]
  facility_type = data[data_item=="FACILITY_TYPE",.(route_id,begin_point,end_point,FACILITY_TYPE=value_numeric)]

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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Shoulder_Width_L Should be < Median_Width

cross_validation_60 = function(data){
  
  #browser()
  left_shoulder_width = data[data_item=="SHOULDER_WIDTH_L",.(route_id,begin_point,end_point,SHOULDER_WIDTH_L=value_numeric)]
  median_width = data[data_item=="MEDIAN_WIDTH",.(route_id,begin_point,end_point,MEDIAN_WIDTH=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Widening_Obstacle must contain A-G where Widening_Potential <9

cross_validation_23 = function(data){
  
  #browser()
  widening_obstacle = data[data_item=="WIDENING_OBSTACLE", .(route_id,begin_point,end_point,WIDENING_OBSTACLE=Value_Text)]
  widening_potential = data[data_item=="WIDENING_POTENTIAL",.(route_id,begin_point,end_point,WIDENING_POTENTIAL=value_numeric)]
  
  if(nrow(widening_obstacle)==0|nrow(widening_potential)==0){
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# IRI >= 30 and <= 400

cross_validation_1 = function(data){
  
  #browser()
  iri = data[data_item=="IRI", .(route_id,begin_point,end_point,IRI=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Rutting should be < 1

cross_validation_52 = function(data){
  
  #browser()
  rutting = data[data_item=="RUTTING", .(route_id,begin_point,end_point,RUTTING=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Faulting should be <= 1

cross_validation_47 = function(data){
  
  #browser()
  faulting = data[data_item=="FAULTING", .(route_id,begin_point,end_point,FAULTING=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where Surface Type is in (2,6,7,8) Cracking Percent should not exceed: 
# X based on Lane Width. See table on AC Cracking Validation Tab.

cross_validation_63 = function(data){
  
  #browser()
  cracking_percent = data[data_item=="CRACKING_PERCENT", .(route_id,begin_point,end_point,CRACKING_PERCENT=value_numeric)]
  lane_width = data[data_item=="SURFACE_TYPE",.(route_id,begin_point,end_point,SURFACE_TYPE=value_numeric)]
  surface_type = data[data_item=="LANE_WIDTH",.(route_id,begin_point,end_point,LANE_WIDTH=value_numeric)]
  
  if(nrow(cracking_percent)==0|nrow(lane_width)==0|nrow(surface_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # join the two together
  comparison = lane_width[cracking_percent,on=.(route_id,begin_point,end_point)]
  comparison = surface_type[comparison,on=.(route_id,begin_point,end_point)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(end_point-begin_point)
              ),
             .(applies = SURFACE_TYPE%in%c(2,6,7,8), 
               passes  = SURFACE_TYPE%in%c(2,6,7,8)&CRACKING_PERCENT<=c(rep(NA,9),81.30,72.20,65.00,59.10,54.20,50.00,46.40,43.30,40.60)[1+LANE_WIDTH])][order(applies,passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where Surface Type is in (3,4,5,9,10) Cracking Percent should be < 75

cross_validation_51 = function(data){
  
  #browser()
  cracking_percent = data[data_item=="CRACKING_PERCENT", .(route_id,begin_point,end_point,CRACKING_PERCENT=value_numeric)]
  surface_type = data[data_item=="SURFACE_TYPE",.(route_id,begin_point,end_point,SURFACE_TYPE=value_numeric)]
  
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
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Year_Last_Construction	<= Year_Record or NULL

cross_validation_9 = function(data){
  
  #browser()
  year_last_construction = data[data_item=="YEAR_LAST_CONSTRUCTION", .(route_id,begin_point,end_point,YEAR_LAST_CONSTRUCTION=Value_Date,Year_Record)]

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
               passes  = !is.na(YEAR_LAST_CONSTRUCTION)&year(YEAR_LAST_CONSTRUCTION)<=Year_Record)][order(applies,passes)]
  
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
  
  comparison = data[data_item%in%c("IRI","PSR","RUTTING","FAULTING","CRACKING_PERCENT"),.(route_id,begin_point,end_point,data_item)]
  
  comparison[,section_length:=end_point-begin_point]
  
  results = comparison[,.(num_sections=.N,mileage = sum(section_length)),.(data_item,passes=section_length<=0.11)][order(data_item,passes)]
  
  results[,c("section_total","mileage_total"):=.(sum(num_sections),sum(mileage)),.(data_item)]
  results[,c("sections_pass","mileage_pass"):=.(num_sections/section_total,mileage/mileage_total)]
  results = results[passes==TRUE]
  results[,.id:=data_item]
  results[,c("num_sections","mileage","passes"):=NULL]
  results[,data_item:=paste0("Mileage measurement (<0.11 miles): ",data_item)]
  setnames(results,"data_item","Description")
  
  setcolorder(results,c(".id","Description","section_total","mileage_total","sections_pass","mileage_pass" ))
  return(results)

}


