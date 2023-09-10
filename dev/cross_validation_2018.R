

###################################################################
# exapnds data set to 0.01 mile increments for easier joining
expand = function(data){
  
  data[,BeginPoint:=round(BeginPoint+ 0.00001,2)] # forcing rounding to next highest integer
  data[,  EndPoint:=round(EndPoint  + 0.00001,2)]
  
  # dividing up the num sections so we can recover the 
  # num sections later
  data[,num_sections:=1/((EndPoint-BeginPoint)/0.01)]
  
  # how long do we have to make the faux road network
  range = data[,.(start=min(BeginPoint),end=max(EndPoint))]
  
  route_network = data.table(end = range[,start]:(range[,end]*100)/100)
  route_network[,start:=end-0.01]
  
  route_network[,start:=round(start + 0.00001,2)] # rounding here is necessary to guarantee correct results when looking at inequalities
  route_network[,  end:=round(end   + 0.00001,2)]
  
  route_network = route_network[start>=0]
  
  data.expanded = data[route_network,on=.(BeginPoint<=start,EndPoint>=end),allow.cartesian=TRUE]
  
  data.expanded = data.expanded[!is.na(RouteId),]
  
  return(data.expanded)
}

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
 
 if(nrow(results[["results"]][Applies==TRUE&Passes==TRUE])==0){
   results[["results"]]=rbindlist(list(
     results[["results"]],
     data.table(Applies=TRUE,Passes=TRUE,N=0,num_sections=0,mileage=0))
    )
 }
 results[["results"]][Applies==TRUE,c("section_total","mileage_total"):=.(sum(num_sections),sum(mileage))]  
  
 return(results[["results"]][Applies==TRUE&Passes==TRUE,.(section_total, mileage_total,sections_pass=num_sections/section_total,mileage_pass=mileage/mileage_total)])   
}



# -----------------------------------------------------------------------

###################################################################
# Through_Lanes>1 when Facility_Type = 2
cross_validation_53 = function(data){
  
  through_lanes = data[DataItem=="THROUGH_LANES",.(RouteId,BeginPoint,EndPoint,THROUGH_LANES=ValueNumeric)]
  facility_type = data[DataItem=="FACILITY_TYPE",.(RouteId,BeginPoint,EndPoint,FACILITY_TYPE=ValueNumeric)]

  if(nrow(through_lanes)==0|nrow(facility_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(facility_type)
  dat2.expanded = expand(through_lanes)
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(i.num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = FACILITY_TYPE==2,
               Passes  = FACILITY_TYPE==2&THROUGH_LANES > 1 )][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Counter_Peak_Lanes + Peak_Lanes Must Be >= Through Lanes
# need to confirm when this should apply

cross_validation_x = function(data){
  
  through_lanes = data[DataItem=="THROUGH_LANES",.(RouteId,BeginPoint,EndPoint,THROUGH_LANES=ValueNumeric)]
  counter_peak_lanes = data[DataItem=="COUNTER_PEAK_LANES",.(RouteId,BeginPoint,EndPoint,COUNTER_PEAK_LANES=ValueNumeric)]
  peak_lanes = data[DataItem=="PEAK_LANES",.(RouteId,BeginPoint,EndPoint,PEAK_LANES=ValueNumeric)]

  if(nrow(through_lanes)==0|nrow(counter_peak_lanes)==0|nrow(peak_lanes)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))
  }
  
  #browser()
  # expand to the 0.01
  dat1.expanded = expand(through_lanes)
  dat2.expanded = expand(counter_peak_lanes)
  dat3.expanded = expand(peak_lanes)
  
  dat2.expanded[,num_sections:=NULL]
  dat3.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  comparison = dat3.expanded[comparison,   on=.(RouteId,BeginPoint,EndPoint)]
  
  # setting NAs to 0
  comparison[is.na(COUNTER_PEAK_LANES)&!is.na(PEAK_LANES),COUNTER_PEAK_LANES:=0]
  comparison[is.na(PEAK_LANES)&!is.na(COUNTER_PEAK_LANES),        PEAK_LANES:=0]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(THROUGH_LANES), # always applies if through_lanes exists
               Passes  = COUNTER_PEAK_LANES + PEAK_LANES >= THROUGH_LANES )][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Counter_Peak_Lanes is NULL if FACILITY_TYPE is 1

cross_validation_16 = function(data){
  
  facility_type = data[DataItem=="FACILITY_TYPE",.(RouteId,BeginPoint,EndPoint,FACILITY_TYPE=ValueNumeric)]
  counter_peak_lanes = data[DataItem=="COUNTER_PEAK_LANES",.(RouteId,BeginPoint,EndPoint,COUNTER_PEAK_LANES=ValueNumeric)]
  
  if(nrow(facility_type)==0|nrow(counter_peak_lanes)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  #browser()
  # expand to the 0.01
  dat1.expanded = expand(facility_type)
  dat2.expanded = expand(counter_peak_lanes)
  
  dat2.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = FACILITY_TYPE==1, 
               Passes  = is.na(COUNTER_PEAK_LANES))][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# SPEED_LIMIT should be divisible by 5, and < 90 OR = 999
# should it include a check of 0?
cross_validation_15 = function(data){
  
  #browser()
  speed_limit = data[DataItem=="SPEED_LIMIT",.(RouteId,BeginPoint,EndPoint,SPEED_LIMIT=ValueNumeric)]
  
  if(nrow(speed_limit)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  comparison = expand(speed_limit)

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(SPEED_LIMIT), 
               Passes  = (SPEED_LIMIT==999)|(SPEED_LIMIT%%5==0&SPEED_LIMIT<90))][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Single_Unit < AADT/2.5

cross_validation_45 = function(data){
  
  #browser()
  aadt_single_unit = data[DataItem=="AADT_SINGLE_UNIT",.(RouteId,BeginPoint,EndPoint,AADT_SINGLE_UNIT=ValueNumeric)]
  aadt = data[DataItem=="AADT",.(RouteId,BeginPoint,EndPoint,AADT=ValueNumeric)]
  
  if(nrow(aadt_single_unit)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(aadt_single_unit)
  dat2.expanded = expand(aadt)
  
  dat2.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(AADT_SINGLE_UNIT), 
               Passes  = AADT_SINGLE_UNIT < AADT/2.5)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Single_Unit Shoud be > 0

cross_validation_55 = function(data){
  
  #browser()
  aadt_single_unit = data[DataItem=="AADT_SINGLE_UNIT",.(RouteId,BeginPoint,EndPoint,AADT_SINGLE_UNIT=ValueNumeric)]
  
  if(nrow(aadt_single_unit)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  comparison = expand(aadt_single_unit)
 
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(AADT_SINGLE_UNIT), 
               Passes  = AADT_SINGLE_UNIT >0 )][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# SU AADT + CU AADT < (0.8*AADT)
# need to confirm when this should apply

cross_validation_17 = function(data){
  
  #browser()
  aadt_single_unit = data[DataItem=="AADT_SINGLE_UNIT",.(RouteId,BeginPoint,EndPoint,AADT_SINGLE_UNIT=ValueNumeric)]
  aadt_combination = data[DataItem=="AADT_COMBINATION",.(RouteId,BeginPoint,EndPoint,AADT_COMBINATION=ValueNumeric)]
  aadt = data[DataItem=="AADT",.(RouteId,BeginPoint,EndPoint,AADT=ValueNumeric)]
  
  if(nrow(aadt_single_unit)==0|nrow(aadt_combination)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(aadt_single_unit)
  dat2.expanded = expand(aadt_combination)
  dat3.expanded = expand(aadt)
  
  dat2.expanded[,num_sections:=NULL]
  dat3.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  comparison = dat3.expanded[comparison,   on=.(RouteId,BeginPoint,EndPoint)]
  
  # setting NAs to 0
  comparison[is.na(AADT_SINGLE_UNIT)&!is.na(AADT_COMBINATION),AADT_SINGLE_UNIT:=0]
  comparison[is.na(AADT_COMBINATION)&!is.na(AADT_SINGLE_UNIT),AADT_COMBINATION:=0]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(AADT_COMBINATION), # always applies if through_lanes exists
               Passes  = AADT_SINGLE_UNIT + AADT_COMBINATION < (0.8*AADT) )][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT*PCT_Peak_Single/100 < =AADT_Single_Unit 
# is the division by 100 necessary?

cross_validation_42 = function(data){
  
  #browser()
  pct_peak_single = data[DataItem=="PCT_PEAK_SINGLE",.(RouteId,BeginPoint,EndPoint,PCT_PEAK_SINGLE=ValueNumeric)]
  aadt_single_unit = data[DataItem=="AADT_SINGLE_UNIT",.(RouteId,BeginPoint,EndPoint,AADT_SINGLE_UNIT=ValueNumeric)]
  aadt = data[DataItem=="AADT",.(RouteId,BeginPoint,EndPoint,AADT=ValueNumeric)]
  
  if(nrow(pct_peak_single)==0|nrow(aadt_single_unit)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(pct_peak_single)
  dat2.expanded = expand(aadt_single_unit)
  dat3.expanded = expand(aadt)
  
  dat2.expanded[,num_sections:=NULL]
  dat3.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  comparison = dat3.expanded[comparison,   on=.(RouteId,BeginPoint,EndPoint)]
  
  # setting NAs to 0
  #comparison[is.na(AADT_SINGLE_UNIT)&!is.na(AADT_COMBINATION),AADT_SINGLE_UNIT:=0]
  #comparison[is.na(AADT_COMBINATION)&!is.na(AADT_SINGLE_UNIT),AADT_COMBINATION:=0]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(PCT_PEAK_SINGLE), 
               Passes  = AADT * PCT_PEAK_SINGLE / 100 <= AADT_SINGLE_UNIT)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# PCT_Peak_Single >0 and < 20%
# should it be greater than 0 or equal to 0?

cross_validation_57 = function(data){
  
  #browser()
  pct_peak_single = data[DataItem=="PCT_PEAK_SINGLE",.(RouteId,BeginPoint,EndPoint,PCT_PEAK_SINGLE=ValueNumeric)]

  if(nrow(pct_peak_single)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(pct_peak_single)

  comparison = dat1.expanded
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(PCT_PEAK_SINGLE), 
               Passes  = PCT_PEAK_SINGLE > 0 & PCT_PEAK_SINGLE < 20)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Combination < AADT/2.5
# need to confirm when this should apply

cross_validation_44 = function(data){
  
  #browser()
  aadt_combination = data[DataItem=="AADT_COMBINATION",.(RouteId,BeginPoint,EndPoint,AADT_COMBINATION=ValueNumeric)]
  aadt = data[DataItem=="AADT",.(RouteId,BeginPoint,EndPoint,AADT=ValueNumeric)]
  
  if(nrow(aadt_combination)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(aadt_combination)
  dat2.expanded = expand(aadt)
  
  dat2.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(AADT_COMBINATION), # always applies if through_lanes exists
               Passes  = AADT_COMBINATION < (AADT/2.5) )][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT_Combination Should be > 0 
# need to confirm when this should apply

cross_validation_54 = function(data){
  
  #browser()
  aadt_combination = data[DataItem=="AADT_COMBINATION",.(RouteId,BeginPoint,EndPoint,AADT_COMBINATION=ValueNumeric)]
  
  if(nrow(aadt_combination)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(aadt_combination)
  
  # join the two together
  comparison = dat1.expanded
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(AADT_COMBINATION), # always applies if through_lanes exists
               Passes  = AADT_COMBINATION > 0 )][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT*PCT_Peak_Single/100 < =AADT_Single_Unit 
# is the division by 100 necessary?

cross_validation_43 = function(data){
  
  #browser()
  pct_combination = data[DataItem=="PCT_PEAK_COMBINATION",.(RouteId,BeginPoint,EndPoint,PCT_PEAK_COMBINATION=ValueNumeric)]
  aadt_combination = data[DataItem=="AADT_COMBINATION",.(RouteId,BeginPoint,EndPoint,AADT_COMBINATION=ValueNumeric)]
  aadt = data[DataItem=="AADT",.(RouteId,BeginPoint,EndPoint,AADT=ValueNumeric)]

  if(nrow(pct_combination)==0|nrow(aadt_combination)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(pct_combination)
  dat2.expanded = expand(aadt_combination)
  dat3.expanded = expand(aadt)
  
  dat2.expanded[,num_sections:=NULL]
  dat3.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  comparison = dat3.expanded[comparison,   on=.(RouteId,BeginPoint,EndPoint)]
  
  # setting NAs to 0
  #comparison[is.na(AADT_SINGLE_UNIT)&!is.na(AADT_COMBINATION),AADT_SINGLE_UNIT:=0]
  #comparison[is.na(AADT_COMBINATION)&!is.na(AADT_SINGLE_UNIT),AADT_COMBINATION:=0]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(PCT_PEAK_COMBINATION), 
               Passes  = AADT * PCT_PEAK_COMBINATION / 100 <= AADT_COMBINATION)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# PCT_Peak_Combination >0 and < 20%
# should it be greater than 0 or equal to 0?

cross_validation_56 = function(data){
  
  #browser()
  pct_peak_combination = data[DataItem=="PCT_PEAK_COMBINATION",.(RouteId,BeginPoint,EndPoint,PCT_PEAK_COMBINATION=ValueNumeric)]
  
  if(nrow(pct_peak_combination)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(pct_peak_combination)

  comparison = dat1.expanded
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(PCT_PEAK_COMBINATION), 
               Passes  = PCT_PEAK_COMBINATION > 0 & PCT_PEAK_COMBINATION < 20)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# K_Factor must be > 4 and <20 

cross_validation_49 = function(data){
  
  #browser()
  k_factor = data[DataItem=="K_FACTOR",.(RouteId,BeginPoint,EndPoint,K_FACTOR=ValueNumeric)]

  if(nrow(k_factor)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(k_factor)

  comparison = dat1.expanded
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(K_FACTOR), 
               Passes  = K_FACTOR > 4 & K_FACTOR < 20)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# DIR_Factor must be 100 where Facility_Type = 1

cross_validation_39 = function(data){
  
  #browser()
  facility_type = data[DataItem=="FACILITY_TYPE",.(RouteId,BeginPoint,EndPoint,FACILITY_TYPE=ValueNumeric)]
  dir_factor = data[DataItem=="DIR_FACTOR",.(RouteId,BeginPoint,EndPoint,DIR_FACTOR=ValueNumeric)]
  
  if(nrow(facility_type)==0|nrow(dir_factor)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(facility_type)
  dat2.expanded = expand(dir_factor)
  
  dat2.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = FACILITY_TYPE==1&!is.na(DIR_FACTOR), # DIR_FACTOR is SP 
               Passes  = DIR_FACTOR==100)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# DIR_Factor must be 50=< and <=70 where Facility_Type = 2

cross_validation_40 = function(data){
  
  #browser()
  facility_type = data[DataItem=="FACILITY_TYPE",.(RouteId,BeginPoint,EndPoint,FACILITY_TYPE=ValueNumeric)]
  dir_factor = data[DataItem=="DIR_FACTOR",.(RouteId,BeginPoint,EndPoint,DIR_FACTOR=ValueNumeric)]
  
  if(nrow(facility_type)==0|nrow(dir_factor)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(facility_type)
  dat2.expanded = expand(dir_factor)
  
  dat2.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = FACILITY_TYPE==2&!is.na(DIR_FACTOR), # DIR_FACTOR is SP
               Passes  = DIR_FACTOR>=50&DIR_FACTOR<=70)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# AADT < FAADT < 3*AADT
# this is done elsewhere in the scorecard

cross_validation_41 = function(data){
  
  #browser()
  future_aadt = data[DataItem=="FUTURE_AADT",.(RouteId,BeginPoint,EndPoint,FUTURE_AADT=ValueNumeric)]
  aadt = data[DataItem=="AADT",.(RouteId,BeginPoint,EndPoint,AADT=ValueNumeric)]

  if(nrow(future_aadt)==0|nrow(aadt)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(future_aadt)
  dat2.expanded = expand(aadt)

  dat2.expanded[,num_sections:=NULL]

  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(FUTURE_AADT), 
               Passes  = FUTURE_AADT > AADT  & FUTURE_AADT < 3 * AADT)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where F_System = 1 and Urban Code <> 99999, Signal_Type should = 5 or Null
# need to confirm when this should apply

cross_validation_y = function(data){
  
  signal_type = data[DataItem=="SIGNAL_TYPE",.(RouteId,BeginPoint,EndPoint,SIGNAL_TYPE=ValueNumeric)]
  f_system = data[DataItem=="F_SYSTEM",.(RouteId,BeginPoint,EndPoint,F_SYSTEM=ValueNumeric)]
  urban_id = data[DataItem=="URBAN_ID",.(RouteId,BeginPoint,EndPoint,URBAN_ID=ValueNumeric)]

  if(nrow(signal_type)==0|nrow(f_system)==0|nrow(urban_id)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }

  # expand to the 0.01
  dat1.expanded = expand(signal_type)
  dat2.expanded = expand(f_system)
  dat3.expanded = expand(urban_id)
  
  dat2.expanded[,num_sections:=NULL]
  dat3.expanded[,num_sections:=NULL]
  
  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  comparison = dat3.expanded[comparison,   on=.(RouteId,BeginPoint,EndPoint)]
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = F_SYSTEM == 1 & URBAN_ID != 99999 , 
               Passes  = SIGNAL_TYPE==5 | is.na(SIGNAL_TYPE) )][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Lande width must be > 5 and <19 

cross_validation_14 = function(data){
  
  #browser()
  lane_width = data[DataItem=="LANE_WIDTH",.(RouteId,BeginPoint,EndPoint,LANE_WIDTH=ValueNumeric)]
  
  if(nrow(lane_width)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(lane_width)

  comparison = dat1.expanded
  
  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(LANE_WIDTH), 
               Passes  = LANE_WIDTH > 5 & LANE_WIDTH < 19)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Median Type in (2,3,4,5,6,7)	Median Width > 0

cross_validation_22 = function(data){
  
  #browser()
  median_width = data[DataItem=="MEDIAN_TYPE",.(RouteId,BeginPoint,EndPoint,MEDIAN_TYPE=ValueNumeric)]
  median_type = data[DataItem=="MEDIAN_WIDTH",.(RouteId,BeginPoint,EndPoint,MEDIAN_WIDTH=ValueNumeric)]

  if(nrow(median_width)==0|nrow(median_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(median_width)
  dat2.expanded = expand(median_type)

  dat2.expanded[,num_sections:=NULL]

  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = MEDIAN_TYPE%in%2:7, 
               Passes  = MEDIAN_WIDTH>0)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Median Width Null if (FACILITY_TYPE is 1 or 4) or Median_Type Code <2
# what does NULL mean? 0 or not reported or reported but NULL?

cross_validation_20 = function(data){
  
  #browser()
  median_width = data[DataItem=="MEDIAN_TYPE",  .(RouteId,BeginPoint,EndPoint,MEDIAN_TYPE=ValueNumeric)]
  median_type = data[DataItem=="MEDIAN_WIDTH", .(RouteId,BeginPoint,EndPoint,MEDIAN_WIDTH=ValueNumeric)]
  facility_type = data[DataItem=="FACILITY_TYPE",.(RouteId,BeginPoint,EndPoint,FACILITY_TYPE=ValueNumeric)]

  if(nrow(median_width)==0|nrow(median_type)==0|nrow(facility_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(median_width)
  dat2.expanded = expand(median_type)
  dat3.expanded = expand(facility_type)

  dat2.expanded[,num_sections:=NULL]
  dat3.expanded[,num_sections:=NULL]

  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  comparison = dat3.expanded[comparison,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = MEDIAN_TYPE<1|FACILITY_TYPE%in%c(1,4), 
               Passes  = is.na(MEDIAN_WIDTH))][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Shoulder_Width_L Should be < Median_Width

cross_validation_60 = function(data){
  
  #browser()
  left_shoulder_width = data[DataItem=="SHOULDER_WIDTH_L",.(RouteId,BeginPoint,EndPoint,SHOULDER_WIDTH_L=ValueNumeric)]
  median_width = data[DataItem=="MEDIAN_WIDTH",.(RouteId,BeginPoint,EndPoint,MEDIAN_WIDTH=ValueNumeric)]
  
  if(nrow(left_shoulder_width)==0|nrow(median_width)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(left_shoulder_width)
  dat2.expanded = expand(median_width)

  dat2.expanded[,num_sections:=NULL]

  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(SHOULDER_WIDTH_L), 
               Passes  = SHOULDER_WIDTH_L<MEDIAN_WIDTH)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Widening_Obstacle must contain A-G where Widening_Potential <9

cross_validation_23 = function(data){
  
  #browser()
  widening_obstacle = data[DataItem=="WIDENING_OBSTACLE", .(RouteId,BeginPoint,EndPoint,WIDENING_OBSTACLE=ValueText)]
  widening_potential = data[DataItem=="WIDENING_POTENTIAL",.(RouteId,BeginPoint,EndPoint,WIDENING_POTENTIAL=ValueNumeric)]
  
  if(nrow(widening_obstacle)==0|nrow(widening_potential)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(widening_obstacle)
  dat2.expanded = expand(widening_potential)

  dat2.expanded[,num_sections:=NULL]

  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]


  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = WIDENING_POTENTIAL<9, 
               Passes  = grepl("[a-gA-G]+",WIDENING_OBSTACLE)&WIDENING_POTENTIAL<9)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# IRI >= 30 and <= 400

cross_validation_1 = function(data){
  
  #browser()
  iri = data[DataItem=="IRI", .(RouteId,BeginPoint,EndPoint,IRI=ValueNumeric)]
  
  if(nrow(iri)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(iri)

  # join the two together
  comparison = dat1.expanded

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(IRI), 
               Passes  = !is.na(IRI)&IRI>=30&IRI<=400)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Rutting should be < 1

cross_validation_52 = function(data){
  
  #browser()
  rutting = data[DataItem=="RUTTING", .(RouteId,BeginPoint,EndPoint,RUTTING=ValueNumeric)]
  
  if(nrow(rutting)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(rutting)

  # join the two together
  comparison = dat1.expanded

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(RUTTING), 
               Passes  = !is.na(RUTTING)&RUTTING<1)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Faulting should be <= 1

cross_validation_47 = function(data){
  
  #browser()
  faulting = data[DataItem=="FAULTING", .(RouteId,BeginPoint,EndPoint,FAULTING=ValueNumeric)]
  
  if(nrow(faulting)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(faulting)

  # join the two together
  comparison = dat1.expanded

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(FAULTING), 
               Passes  = !is.na(FAULTING)&FAULTING<=1)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where Surface Type is in (2,6,7,8) Cracking Percent should not exceed: 
# X based on Lane Width. See table on AC Cracking Validation Tab.

cross_validation_63 = function(data){
  
  #browser()
  cracking_percent = data[DataItem=="CRACKING_PERCENT", .(RouteId,BeginPoint,EndPoint,CRACKING_PERCENT=ValueNumeric)]
  lane_width = data[DataItem=="SURFACE_TYPE",.(RouteId,BeginPoint,EndPoint,SURFACE_TYPE=ValueNumeric)]
  surface_type = data[DataItem=="LANE_WIDTH",.(RouteId,BeginPoint,EndPoint,LANE_WIDTH=ValueNumeric)]
  
  if(nrow(cracking_percent)==0|nrow(lane_width)==0|nrow(surface_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(cracking_percent)
  dat2.expanded = expand(lane_width)
  dat3.expanded = expand(surface_type)

  dat2.expanded[,num_sections:=NULL]
  dat3.expanded[,num_sections:=NULL]

  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]
  comparison = dat3.expanded[comparison,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = SURFACE_TYPE%in%c(2,6,7,8), 
               Passes  = SURFACE_TYPE%in%c(2,6,7,8)&CRACKING_PERCENT<=c(rep(NA,9),81.30,72.20,65.00,59.10,54.20,50.00,46.40,43.30,40.60)[1+LANE_WIDTH])][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Where Surface Type is in (3,4,5,9,10) Cracking Percent should be < 75

cross_validation_51 = function(data){
  
  #browser()
  cracking_percent = data[DataItem=="CRACKING_PERCENT", .(RouteId,BeginPoint,EndPoint,CRACKING_PERCENT=ValueNumeric)]
  surface_type = data[DataItem=="SURFACE_TYPE",.(RouteId,BeginPoint,EndPoint,SURFACE_TYPE=ValueNumeric)]
  
  if(nrow(cracking_percent)==0|nrow(surface_type)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(cracking_percent)
  dat2.expanded = expand(surface_type)

  dat2.expanded[,num_sections:=NULL]

  # join the two together
  comparison = dat2.expanded[dat1.expanded,on=.(RouteId,BeginPoint,EndPoint)]

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = SURFACE_TYPE%in%c(3,4,5,9,10), 
               Passes  = SURFACE_TYPE%in%c(3,4,5,9,10)&CRACKING_PERCENT<=75)][order(Applies,Passes)]
  
  return(list(results=results,comparison=comparison))

}

###################################################################
# Year_Last_Construction	<= DataYear or NULL

cross_validation_9 = function(data){
  
  #browser()
  year_last_construction = data[DataItem=="YEAR_LAST_CONSTRUCTION", .(RouteId,BeginPoint,EndPoint,YEAR_LAST_CONSTRUCTION=ValueDate,DataYear)]

  if(nrow(year_last_construction)==0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  # expand to the 0.01
  dat1.expanded = expand(year_last_construction)

  # join the two together
  comparison = dat1.expanded

  # apply the condition
  results = comparison[,
             .(
               .N,
               num_sections = sum(num_sections,na.rm=TRUE),
               mileage      = sum(EndPoint-BeginPoint)
              ),
             .(Applies = !is.na(YEAR_LAST_CONSTRUCTION), 
               Passes  = !is.na(YEAR_LAST_CONSTRUCTION)&year(YEAR_LAST_CONSTRUCTION)<=DataYear)][order(Applies,Passes)]
  
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
  
  comparison = data[DataItem%in%c("IRI","PSR","RUTTING","FAULTING","CRACKING_PERCENT"),.(RouteId,BeginPoint,EndPoint,DataItem)]
  
  comparison[,section_length:=EndPoint-BeginPoint]
  
  results = comparison[,.(num_sections=.N,mileage = sum(section_length)),.(DataItem,Passes=section_length<=0.11)][order(DataItem,Passes)]
  
  results[,c("section_total","mileage_total"):=.(sum(num_sections),sum(mileage)),.(DataItem)]
  results[,c("sections_pass","mileage_pass"):=.(num_sections/section_total,mileage/mileage_total)]
  results = results[Passes==TRUE]
  results[,.id:=DataItem]
  results[,c("num_sections","mileage","Passes"):=NULL]
  results[,DataItem:=paste0("Mileage measurement (<0.11 miles): ",DataItem)]
  setnames(results,"DataItem","Description")
  
  setcolorder(results,c(".id","Description","section_total","mileage_total","sections_pass","mileage_pass" ))
  return(results)

}


