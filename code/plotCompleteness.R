###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont 
#
#
# Description:
#
# Function to plot availability scores on title page. It implements the 
# coverage validation provided by FHWA.
#
###########################################################################

plotCompleteness <- function(data, year, variable, x, y){
  
  ts <- Sys.time()
  
  data <- data[!(F_SYTEMorig==7 & NHS!=1),]
  
  type <- 1
  
  return(type) # for the time being until we can implement the 1 spatial stuff 
  
  if(nrow(data[data_item==variable&year_record==year,])>0){
    type <- 2
  }
  #
  if(nrow(data[data_item==variable&year_record==year,])>0){
    
    # this is where the coverage validation check goes
    # these variables just need to have something to be complete
    if((variable%in%c("STRUCTURE_TYPE","STRAHNET_TYPE","TRUCK","FUTURE_FACILITY","CAPACITY"))){
      type <- 3
    }
    
    # these are strictly sample variables
    # interpreted as reported sample variables need to have an expansion factor.
    # if no expansion factor, than the coverage is invalidated
    
    #####################################
    if(variable%in%c("PEAK_LANES","SPEED_LIMIT","PCT_PEAK_SINGLE","PCT_PEAK_COMBINATION","K_FACTOR","DIR_FACTOR","FUTURE_AADT","STOP_SIGNS","AT_GRADE_OTHER","LANE_WIDTH","MEDIAN_TYPE","SHOULDER_TYPE","WIDENING_OBSTACLE","WIDENING_POTENTIAL","SURFACE_TYPE")){
  
      if(nrow(data[data_item==variable&year_record==year&is.na(expansion_factor),])==0&nrow(data[data_item==variable&year_record==year,])>0){
        type <- 3
      }
    }  
    
    #####################################
    if(variable%in%c("ROUTE_SIGNING","ROUTE_QUALIFIER")){
     
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,FACILITYTYPE %in% c(1,2)&(FSYSTEM %in% c(1,2,3,4)|!is.na(NHS)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
      
    }
    #####################################
    if(variable=="URBAN_CODE"){
     
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,FACILITYTYPE %in% c(1,2,4)&(FSYSTEM %in% c(1,2,3,4,5,6)|!is.na(NHS)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
      
    }
    
    #####################################
    if(variable=="MEDIAN_WIDTH"){
      
      dat.variable    <- data[data_item==variable     &year_record==year,]
      dat.MEDIAN_TYPE <- data[data_item=="MEDIAN_TYPE"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as MEDIANTYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.MEDIAN_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      coverage$required <- with(coverage,MEDIANTYPE %in% 2:7&(!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    #####################################
    if(variable=="SHOULDER_WIDTH_R"){
      
      dat.variable    <- data[data_item==variable     &year_record==year,]
      dat.SHOULDER_TYPE <- data[data_item=="SHOULDER_TYPE"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SHOULDERTYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SHOULDER_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      coverage$required <- with(coverage,SHOULDERTYPE %in% 2:6&(!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    #####################################
    if(variable=="SHOULDER_WIDTH_L"){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SHOULDER_TYPE <- data[data_item=="SHOULDER_TYPE"&year_record==year,]
      dat.MEDIAN_TYPE   <- data[data_item=="MEDIAN_TYPE"  &year_record==year,]
      
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
      
      
      coverage$required <- with(coverage,SHOULDERTYPE %in% 2:6&MEDIANTYPE%in%2:7&(!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
      
    }
    
    #####################################
    if(variable=="SHOULDER_WIDTH_R"){
      
      dat.variable    <- data[data_item==variable     &year_record==year,]
      dat.SHOULDER_TYPE <- data[data_item=="SHOULDER_TYPE"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SHOULDERTYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SHOULDER_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      coverage$required <- with(coverage,SHOULDERTYPE %in% 2:6&(!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
      
    }
    
    #####################################
    if(variable=="PEAK_PARKING"){
      
      dat.variable    <- data[data_item==variable     &year_record==year,]
      dat.URBAN_CODE <- data[data_item=="URBAN_CODE"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.URBAN_CODE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      coverage$required <- with(coverage,URBANCODE<99999&(!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
      
    }
    
    #####################################
    if(variable%in%c("TURN_LANES_R","TURN_LANES_L")){
      
      dat.variable       <- data[data_item==variable    &year_record==year,]
      dat.URBAN_CODE     <- data[data_item=="URBAN_CODE"&year_record==year,]
      dat.ACCESS_CONTROL <- data[data_item=="ACCESS_CONTROL"&year_record==year,]
      
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
      
      
      coverage$required <- with(coverage,URBANCODE<99999&ACCESSCONTROL>1&(!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3
      }
      
    }
    
    #####################################
    if(variable=="F_SYSTEM"){
      
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"&year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
                            B.value_numeric as FSYSTEM 
                            from [dat.FACILITY_TYPE] A 
                            left join [dat.F_SYSTEM] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            ) where A.value_numeric in (1,2,4)")
      
      if(sum(is.na(coverage$FSYSTEM))==0&nrow(coverage)>0){
        type <- 3 
  
      }
      
    }# end Fsystem
    
    #####################################
    if(variable=="HOV_TYPE"){
      
      dat.HOV_TYPE  <- data[data_item=="HOV_TYPE"&year_record==year,]
      dat.HOV_LANES <- data[data_item=="HOV_LANES"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as HOVLANES, 
                            B.value_numeric as HOVTYPE 
                            from [dat.HOV_LANES] A 
                            left join [dat.HOV_TYPE] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      if(sum(is.na(coverage$HOVTYPE))==0&nrow(coverage)>0){
        type <- 3  
      }
      
    } # end HOV_TYPE
    
    #####################################
    if(variable=="HOV_LANES"){
      
      dat.HOV_LANES <- data[data_item=="HOV_LANES"&year_record==year,]
      dat.HOV_TYPE  <- data[data_item=="HOV_TYPE"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as HOVTYPE, 
                            B.value_numeric as HOVLANES  
                            from [dat.HOV_TYPE] A 
                            left join [dat.HOV_LANES] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      if(sum(is.na(coverage$HOVLANES))==0&nrow(coverage)>0){
        type <- 3
      }
    } # end HOV_LANES
    
    #####################################
    if(variable=="TOLL_CHARGED"){
      
      dat.TOLL_CHARGED <- data[data_item=="TOLL_CHARGED"&year_record==year,]
      dat.TOLL_TYPE    <- data[data_item=="TOLL_TYPE"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as TOLLTYPE, 
                            B.value_numeric as TOLLCHARGED  
                            from [dat.TOLL_TYPE] A 
                            left join [dat.TOLL_CHARGED] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      if(sum(is.na(coverage$TOLLCHARGED))==0&nrow(coverage)>0){
        type <- 3
      }
    } # end TOLL_CHARGED
    
    #####################################
    if(variable=="TOLL_TYPE"){
      
      dat.TOLL_TYPE    <- data[data_item=="TOLL_TYPE"&year_record==year,]
      dat.TOLL_CHARGED <- data[data_item=="TOLL_CHARGED"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as TOLLCHARGED, 
                            B.value_numeric as TOLLTYPE   
                            from [dat.TOLL_CHARGED] A 
                            left join [dat.TOLL_TYPE] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      if(sum(is.na(coverage$TOLLTYPE))==0&nrow(coverage)>0){
        type <- 3  
      }
    } # end TOLL_TYPE
    
    #####################################
    if(variable=="COUNTY_CODE"){
      
      dat.COUNTY_CODE   <- data[data_item=="COUNTY_CODE"  &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,FACILITYTYPE %in% c(1,2)&(FSYSTEM %in% c(1,2,3,4,5)|(FSYSTEM==6&URBANCODE==99999)|!is.na(NHS)))
  
      if(sum(is.na(coverage$COUNTYCODE[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } # end COUNTY_CODE
    
    #####################################
    if(variable=="FACILITY_TYPE"){
      
      dat.variable      <- data[data_item==variable  &year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,(FSYSTEM %in% c(1,2,3,4,5)|(FSYSTEM==6&URBANCODE==99999)|!is.na(NHS)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
    } 
    
    #####################################
    if(variable=="ACCESS_CONTROL"){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,FACILITYTYPE %in% c(1,2)&(FSYSTEM %in% c(1,2,3)|!is.na(NHS)|!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
    } 
    
    #####################################
    if(variable=="OWNERSHIP"){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,FACILITYTYPE %in% c(1,2)&(FSYSTEM %in% c(1,2,3,4,5)|((FSYSTEM==6)&(URBANCODE<99999))|!is.na(NHS)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3
      }
    } 
    
    #####################################
    if(variable%in%c("THROUGH_LANES","AADT")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,FACILITYTYPE %in% c(1,2,4)&(FSYSTEM %in% c(1,2,3,4,5)|((FSYSTEM==6)&(URBANCODE<99999))|!is.na(NHS)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
    } 
    
    #####################################
    if(variable=="COUNTER_PEAK_LANES"){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.THROUGH_LANES <- data[data_item=="THROUGH_LANES"     &year_record==year,]
      dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
    
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
  
  
      coverage$required <- with(coverage,FACILITYTYPE %in% c(2)&(URBANCODE<99999|THROUGHLANES>=4)&!is.na(expansion_factor))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    #####################################
    if(variable%in%c("AADT_SINGLE_UNIT","AADT_COMBINATION")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,(FSYSTEM==1|!is.na(NHS))&FACILITYTYPE %in% c(1,2)|!is.na(expansion_factor))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    #####################################
    if(variable%in%c("NHS")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      
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
      
      coverage$required <- with(coverage,(FSYSTEM==1&FACILITYTYPE < 4))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    #####################################
    if(variable%in%c("SIGNAL_TYPE")){
      
      dat.variable       <- data[data_item==variable        &year_record==year,]
      dat.URBAN_CODE     <- data[data_item=="URBAN_CODE"    &year_record==year,]
      dat.ACCESS_CONTROL <- data[data_item=="ACCESS_CONTROL"&year_record==year,]
      
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
      
      coverage$required <- with(coverage,(URBANCODE!=99999&ACCESSCONTROL==1&!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    #####################################
    if(variable%in%c("PCT_GREEN_TIME")){
      
      dat.variable       <- data[data_item==variable        &year_record==year,]
      dat.URBAN_CODE     <- data[data_item=="URBAN_CODE"    &year_record==year,]
      dat.NUMBER_SIGNALS <- data[data_item=="NUMBER_SIGNALS"&year_record==year,]
      
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
      
      coverage$required <- with(coverage,(URBANCODE<99999&NUMBERSIGNALS>=1&!is.na(expansion_factor)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    #####################################
    if(variable%in%c("NUMBER_SIGNALS")){
      
      dat.variable       <- data[data_item==variable        &year_record==year,]
      dat.SIGNAL_TYPE    <- data[data_item=="SIGNAL_TYPE"   &year_record==year,]
      dat.PCT_GREEN_TIME <- data[data_item=="PCT_GREEN_TIME"&year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SIGNALTYPE  , 
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
      
      
      coverage$required <- with(coverage,(!is.na(expansion_factor)&!is.na(PCTGREENTIME))|(!is.na(expansion_factor)&SIGNALTYPE%in%c(1,2,3,4)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
    } 
    
    #####################################
    if(variable%in%c("PCT_PASS_SIGHT")){
      
      dat.variable       <- data[data_item==variable        &year_record==year,]
      dat.URBAN_CODE     <- data[data_item=="URBAN_CODE"    &year_record==year,]
      dat.THROUGH_LANES  <- data[data_item=="THROUGH_LANES" &year_record==year,]
      dat.MEDIAN_TYPE    <- data[data_item=="MEDIAN_TYPE"   &year_record==year,]
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE  , 
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
      
      coverage$required <- with(coverage,(!is.na(expansion_factor)&URBANCODE==99999&THROUGHLANES==2&MEDIANTYPE%in%c(1,2)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3
      }
    } 
   
   #####################################
   # this has been updated
   ##################################### 
   if(variable%in%c("IRI")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      
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
      
      coverage$required <- with(coverage,FACILITYTYPE%in%c(1,2)&(FSYSTEM%in%c(1,2,3)|!is.na(NHS)|(!is.na(expansion_factor)&FSYSTEM==4&URBANCODE==99999))&SURFACETYPE>1)
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
   }   
  
    #####################################
    if(variable %in% c("PSR")){
      
      keep_cols <- c('route_id', 'begin_point', 'end_point', 'section_length', 'expansion_factor', 'data_item', 'value_numeric')
      dat.variable  <- data[data_item == variable & year_record == year, ..keep_cols]
      dat.FACILITY_TYPE <- data[data_item == "FACILITY_TYPE" & year_record == year, ..keep_cols]
      dat.F_SYSTEM      <- data[data_item == "F_SYSTEM"     & year_record == year, ..keep_cols]
      dat.URBAN_CODE    <- data[data_item == "URBAN_CODE"   & year_record == year, ..keep_cols]
      dat.IRI           <- data[data_item == "IRI"          & year_record == year & is.na(value_numeric), ..keep_cols]
      dat.SURFACE_TYPE  <- data[data_item == "SURFACE_TYPE" & year_record == year & value_numeric > 1, ..keep_cols]    

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
    
    #####################################
    if(variable%in%c("RUTTING")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SURFACE_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&SURFACETYPE%in%c(2,6,7,8))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
    }  
    
    #####################################
    if(variable%in%c("FAULTING")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SURFACE_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&SURFACETYPE%in%c(3,4,9,10))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
    }  
    
    #####################################
    if(variable%in%c("CRACKING_PERCENT","YEAR_LAST_CONSTRUCTION")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SURFACE_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&SURFACETYPE%in%2:10)
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3
      }
    }  
    
    #####################################
    if(variable%in%c("THICKNESS_RIGID")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SURFACE_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&SURFACETYPE%in%3:10)
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    #####################################
    if(variable%in%c("THICKNESS_FLEXIBLE")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SURFACE_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&SURFACETYPE%in%c(2,6,7,8))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    } 
    
    #####################################
    if(variable%in%c("BASE_TYPE")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as SURFACETYPE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.SURFACE_TYPE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&SURFACETYPE>1)
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
    }   
    
    #####################################
    if(variable%in%c("BASE_THICKNESS")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]    
      dat.BASE_TYPE     <- data[data_item=="BASE_TYPE"    &year_record==year,]    
      
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
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&SURFACETYPE>1&BASETYPE>1)
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3
      }
    }     
    
    #####################################
    if(variable%in%c("TERRAIN_TYPE")){
      
      dat.variable    <- data[data_item==variable       &year_record==year,]
      dat.URBAN_CODE  <- data[data_item=="URBAN_CODE" &year_record==year,]    
      
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as URBANCODE, 
                            B.value_numeric as variable,B.expansion_factor   
                            from [dat.URBAN_CODE] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
  
      coverage$required <- with(coverage,!is.na(expansion_factor)&URBANCODE<99999)
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3
      }
    } 
    
    #####################################
    if(variable%in%c("MAINTENACE_OPERATIONS")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      dat.TOLL_CHARGED  <- data[data_item=="TOLL_CHARGED" &year_record==year,]    
      
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
  
      coverage$required <- with(coverage,!is.na(TOLLCHARGE)&FACILITYTYPE%in%c(1,2)&(FSYSTEM%in%1:5|!is.na(NHS)|(FSYSTEM==6&URBANCODE<99999)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
   }   
  
   #####################################    
   if(variable%in%c("YEAR_LAST_IMPROV")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.SURFACE_TYPE  <- data[data_item=="SURFACE_TYPE" &year_record==year,]
      dat.YEAR_LAST_CONSTRUCTION <- data[data_item=="YEAR_LAST_CONSTRUCTION"     &year_record==year,]
  
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
      
      coverage$required <- with(coverage,(!is.na(expansion_factor)&SURFACETYPE%in%2:10))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
   }  
  
    #####################################
    if(variable%in%c("YEAR_LAST_CONSTRUCTION")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.YEAR_LAST_CONSTRUCTION <- data[data_item=="YEAR_LAST_IMPROV"     &year_record==year,]
  
      coverage <- sqldf("select 
                            A.route_id,A.begin_point,A.end_point,A.data_item,A.value_date as YEARLASTCONSTRUCTION, 
                            B.value_date as variable,B.expansion_factor   
                            from [dat.YEAR_LAST_CONSTRUCTION] A 
                            left join [dat.variable] B on 
                              A.route_id = B.route_id and (
                              ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                              ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                            )")
      
      coverage$required <- with(coverage,(!is.na(expansion_factor)&!is.na(YEARLASTCONSTRUCTION)))
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3 
      }
   }  
    
    #####################################
    if(variable%in%c("ROUTE_NUMBER")){
      
      dat.variable      <- data[data_item==variable       &year_record==year,]
      dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
      dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
      dat.ROUTE_SIGNING <- data[data_item=="ROUTE_SIGNING"   &year_record==year,]
      dat.NHS           <- data[data_item=="NHS"          &year_record==year,]
      
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
  
      coverage$required <- with(coverage,(FSYSTEM%in%c(1,2,3,4)|!is.na(NHS))&FACILITYTYPE%in%1:2&ROUTESIGNING%in%2:9 )
  
      if(sum(is.na(coverage$variable[coverage$required]))==0&nrow(coverage)>0){
        type <- 3  
      }
   }   
  
  }
  
  # submitted and complete
  if(type==3){
    grid.circle(
      x=x,
      y=y,
      r=unit(0.007,"npc"),
      gp=gpar(fill="slategray",col="slategray")
      )
  }
  # submitted and incomplete
  if(type==2){
    grid.circle(
      x=x,
      y=y,
      r=unit(0.007,"npc"),
      gp=gpar(fill="gray75",col="slategray"))
  }
  # not submitted
  if(type==1){
    grid.circle(
      x=x,
      y=y,
      r=unit(0.007,"npc"),
      gp=gpar(fill="white",col="slategray"))
  }
  cat(paste0("\n\t",variable,": ",round(difftime(Sys.time(),ts,units="secs"),2)," secs"))
  
  return(type)
}


