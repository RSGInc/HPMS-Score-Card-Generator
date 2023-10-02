
###################################################################
cross_validation_68 = function(data){
  
  # MAINTENANCE_OPERATIONS ValueNumeric <> OWNERSHIP ValueNumeric 
  
  
  maintenance_operations = data[dataitem == "MAINTENANCE_OPERATIONS",
                                .(routeid, beginpoint, endpoint, datayear, MAINTENANCE_OPERATIONS = valuenumeric,
                                  F_SYTEMorig, valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  ownership     = data[dataitem=="OWNERSHIP",
                       .(routeid,beginpoint,endpoint,OWNERSHIP=valuenumeric,num_sections)]
  
  if(maintenance_operations[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = maintenance_operations[ownership, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(MAINTENANCE_OPERATIONS), 
                         passes  = MAINTENANCE_OPERATIONS != OWNERSHIP
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}


###################################################################
cross_validation_70 = function(data){
  
  # DIR_THROUGH_LANES ValueNumeric Must be < OR = ValueNumeric for THROUGH_LANES 
  
  
  dir_through_lanes = data[dataitem == "DIR_THROUGH_LANES",
                    .(routeid, beginpoint, endpoint, datayear, DIR_THROUGH_LANES = valuenumeric,
                      F_SYTEMorig, valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  through_lanes      = data[dataitem=="THROUGH_LANES",
                            .(routeid,beginpoint,endpoint,THROUGH_LANES=valuenumeric,num_sections)]
  
  if(dir_through_lanes[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = dir_through_lanes[through_lanes, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(DIR_THROUGH_LANES), 
                         passes  = DIR_THROUGH_LANES <= THROUGH_LANES
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_72 = function(data){
  
  # If URBAN_ID ValueNumeric < 99999 then NHFN ValueNumeric must not = 3 
  # If URBAN_ID ValueNumeric = 99999 then NHFN ValueNumeric must not = 2
  
  nhfn = data[dataitem == "NHFN",
                           .(routeid, beginpoint, endpoint, datayear, NHFN = valuenumeric,
                             F_SYTEMorig, valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  urban_id      = data[dataitem=="THROUGH_LANES",
                            .(routeid,beginpoint,endpoint,URBAN_ID=valuenumeric,num_sections)]
  
  if(nhfn[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = nhfn[urban_id, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(NHFN), 
                         passes  = (URBAN_ID < 99999 & NHFN != 3) |
                                   (URBAN_ID = 99999 & NHFN != 2)
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_73 = function(data){
  
  # If F_SYSTEM ValueNumeric = 1 AND FACILITY_TYPE is IN (1,2) then NHS must exist and NHS ValueNumeric must = 1
  
  nhs = data[dataitem == "NHS",
              .(routeid, beginpoint, endpoint, datayear, NHS = valuenumeric,
                F_SYSTEM, valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  facility_type  = data[dataitem=="FACILITY_TYPE",
                       .(routeid,beginpoint,endpoint,FACILITY_TYPE=valuenumeric,num_sections)]
  
  if(nhs[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = nhs[facility_type, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = F_SYSTEM == 1 & FACILITY_TYPE %in% 1:2, 
                         passes  = !is.na(NHS) & NHS == 1
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_74 = function(data){
  
  # If F_SYSTEM ValueNumeric = 1 Then NN must exist and NN ValueNumeric must = 1
  
  nn = data[dataitem == "NN",
             .(routeid, beginpoint, endpoint, datayear, NN = valuenumeric,
               valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  f_system  = data[dataitem=="F_SYSTEM",
                        .(routeid,beginpoint,endpoint,F_SYSTEM=valuenumeric,num_sections)]
  
  if(nn[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = nn[f_system, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = F_SYSTEM == 1, 
                         passes  = !is.na(NN) & NN == 1
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_75_1 = function(data){
  
  # 1 - Where ROUTE_NUMBER ValueNumeric or ValueText is not NULL; ROUTE_SIGNING ValueNumeric must not be NULL
  # 2 - Where ROUTE_NUMBER ValueNumeric or ValueText is not NULL; ROUTE_QUALIFIER ValueNumeric must not be NULL
  
  route_number = data[dataitem == "ROUTE_NUMBER",
            .(routeid, beginpoint, endpoint, datayear, ROUTE_NUMBER_NUM = valuenumeric, ROUTE_NUMBER_TXT = valuetext,
              valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  route_signing  = data[dataitem=="ROUTE_SIGNING",
                   .(routeid,beginpoint,endpoint,ROUTE_SIGNING=valuenumeric,num_sections)]
  
  if(route_number[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = route_number[route_signing, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(ROUTE_NUMBER_NUM) | !is.na(ROUTE_NUMBER_TXT), 
                         passes  = !is.na(ROUTE_SIGNING) 
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_75_2 = function(data){
  
  # 1 - Where ROUTE_NUMBER ValueNumeric or ValueText is not NULL; ROUTE_SIGNING ValueNumeric must not be NULL
  # 2 - Where ROUTE_NUMBER ValueNumeric or ValueText is not NULL; ROUTE_QUALIFIER ValueNumeric must not be NULL
  
  route_number = data[dataitem == "ROUTE_NUMBER",
                      .(routeid, beginpoint, endpoint, datayear, ROUTE_NUMBER_NUM = valuenumeric, ROUTE_NUMBER_TXT = valuetext,
                        valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  route_qualifier  = data[dataitem=="ROUTE_QUALIFIER",
                        .(routeid,beginpoint,endpoint,ROUTE_QUALIFIER=valuenumeric,num_sections)]
  
  if(route_number[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = route_number[route_qualifier, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(ROUTE_NUMBER_NUM) | !is.na(ROUTE_NUMBER_TXT), 
                         passes  = !is.na(ROUTE_QUALIFIER) 
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_76_1 = function(data){
  
  # 1 - If F_SYSTEM ValueNumeric = 1 Then STRAHNET_TYPE must exist and STRAHNET_TYPE ValueNumeric must = 1
  # 2 - If STRAHNET_TYPE ValueNumeric is in the range (1;2) then NHS ValueNumeric must = 1
  
  strahnet_type = data[dataitem == "STRAHNET_TYPE",
                      .(routeid, beginpoint, endpoint, datayear, STRAHNET_TYPE = valuenumeric, 
                        valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  f_system  = data[dataitem=="F_SYSTEM",
                          .(routeid,beginpoint,endpoint,F_SYSTEM=valuenumeric,num_sections)]
  
  if(strahnet_type[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = strahnet_type[f_system, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = F_SYSTEM == 1, 
                         passes  = !is.na(STRAHNET_TYPE) & STRAHNET_TYPE == 1 
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}

###################################################################
cross_validation_76_2 = function(data){
  
  # 1 - If F_SYSTEM ValueNumeric = 1 Then STRAHNET_TYPE must exist and STRAHNET_TYPE ValueNumeric must = 1
  # 2 - If STRAHNET_TYPE ValueNumeric is in the range (1;2) then NHS ValueNumeric must = 1
  
  strahnet_type = data[dataitem == "STRAHNET_TYPE",
                       .(routeid, beginpoint, endpoint, datayear, STRAHNET_TYPE = valuenumeric, 
                         valuetext, valuedate, begindate, num_sections)]  #TODO: clarify difference between F_SYTEMorig and F_SYSTEM
  
  nhs  = data[dataitem=="NHS",
                   .(routeid,beginpoint,endpoint,NHS=valuenumeric,num_sections)]
  
  if(strahnet_type[, .N] == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL, comparison=NULL))  
  }
  
  comparison = strahnet_type[nhs, on = .(routeid, beginpoint, endpoint)]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = STRAHNET_TYPE %in% 1:2, 
                         passes  = NHS == 1 
                       )][order(applies,passes)]
  # passes  = year(valuedate) == datayear)][order(applies,passes)]
  
  if(nrow(results[applies == TRUE]) == 0){
    warning("Not applicable - Sufficient data from the state are not available")
    return(list(results=NULL,comparison=NULL))  
  }
  
  
  return(list(results=results, comparison=comparison))
  
  
}
###################################################################
# cross_validation_40 = function(data, sub_check = 2){
#   # DIR_Factor must be 50=< and <=70 where Facility_Type = 2
#   
#   #browser()
#   facility_type = data[dataitem=="FACILITY_TYPE",
#                        .(routeid,beginpoint,endpoint,FACILITY_TYPE=valuenumeric, num_sections)]
#   dir_factor = data[dataitem=="DIR_FACTOR",
#                     .(routeid,beginpoint,endpoint,DIR_FACTOR=valuenumeric)]
#   
#   if(facility_type[, .N] == 0|dir_factor[, .N] == 0){
#     warning("Not applicable - Sufficient data from the state are not available")
#     return(list(results=NULL,comparison=NULL))  
#   }
#   
#   # join the two together
#   comparison = dir_factor[facility_type,on=.(routeid,beginpoint,endpoint)]
#   
#   # apply the condition
#   
#   if ( sub_check == 1 ) {
#     
#     results = comparison[,
#                          .(
#                            .N,
#                            num_sections = sum(num_sections,na.rm=TRUE),
#                            mileage      = sum(endpoint-beginpoint)
#                          ),
#                          .(applies = FACILITY_TYPE == 1 & !is.na(DIR_FACTOR), # DIR_FACTOR is SP
#                            passes  = DIR_FACTOR == 100 )][order(applies,passes)]
#     
#   } else if ( sub_check == 2 ){
#     
#     results = comparison[,
#                          .(
#                            .N,
#                            num_sections = sum(num_sections,na.rm=TRUE),
#                            mileage      = sum(endpoint-beginpoint)
#                          ),
#                          .(applies = FACILITY_TYPE == 2 & !is.na(DIR_FACTOR), # DIR_FACTOR is SP
#                            passes  = DIR_FACTOR >= 50 & DIR_FACTOR <= 75 )][order(applies,passes)]
#     
#   } else {
#     browser()
#   }
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

# cross_validation_CURVES_TEST = function(data){
#   
#   ## Testing
#   data[dataitem %like% "CURVES", .N]
#   
#   comparison = data[dataitem %like% "CURVES", .(dataitem, F_SYSTEM, URBAN_ID, FACILITY_TYPE, 
#                         routeid, beginpoint, endpoint, num_sections,
#                       sample = !is.na(expansionfactor))]
#   
#   # apply the condition
#   results = comparison[,
#                        .(
#                          .N,
#                          num_sections = sum(num_sections,na.rm=TRUE),
#                          mileage      = sum(endpoint-beginpoint)
#                        ),
#                        .(applies = !is.na(IRI), 
#                          passes  = !is.na(IRI)&IRI>=30&IRI<=400)][order(applies,passes)]
# }
