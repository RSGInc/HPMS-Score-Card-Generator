

cross_validation_CURVES_TEST = function(data){
  
  ## Testing
  data[dataitem %like% "CURVES", .N]
  
  comparison = data[dataitem %like% "CURVES", .(dataitem, F_SYSTEM, URBAN_ID, FACILITY_TYPE, 
                        routeid, beginpoint, endpoint, num_sections,
                      sample = !is.na(expansionfactor))]
  
  # apply the condition
  results = comparison[,
                       .(
                         .N,
                         num_sections = sum(num_sections,na.rm=TRUE),
                         mileage      = sum(endpoint-beginpoint)
                       ),
                       .(applies = !is.na(IRI), 
                         passes  = !is.na(IRI)&IRI>=30&IRI<=400)][order(applies,passes)]
}

