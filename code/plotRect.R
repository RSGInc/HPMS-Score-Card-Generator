plotRect <- function(data,year,variable,startx,starty,C,R)
{
  
  type <- 3
  
  if(nrow(data[data_item==variable&year_record==year,])>0){
    type <- 2
  }
  
  # this is where the coverage validation check goes
  # these variables just need to have something to be complete
  if((variable%in%c("STRUCTURE_TYPE","STRAHNET_TYPE","TRUCK","FUTURE_FACILITY","CAPACITY"))&nrow(data[data_item==variable&year_record==year,])>0){
    type <- 1
  }
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
      type <- 1  

    }
    
  }# end Fsystem
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
      type <- 1  
    }
    
  } # end HOV_TYPE
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
      type <- 1  
    }
  } # end HOV_LANES
  
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
      type <- 1  
    }
  } # end TOLL_CHARGED
  
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
      type <- 1  
    }
  } # end TOLL_TYPE
  
  if(variable=="COUNTY_CODE"){
    
    dat.COUNTY_CODE   <- data[data_item=="COUNTY_CODE"  &year_record==year,]
    dat.FACILITY_TYPE <- data[data_item=="FACILITY_TYPE"&year_record==year,]
    dat.F_SYSTEM      <- data[data_item=="F_SYSTEM"     &year_record==year,]
    dat.URBAN_CODE    <- data[data_item=="URBAN_CODE"   &year_record==year,]
    dat.NHS           <- data[data_item=="URBAN_CODE"   &year_record==year,]
    
    coverage.1 <- sqldf("select 
                          A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FACILITYTYPE, 
                          B.value_numeric as COUNTYCODE   
                          from [dat.FACILITY_TYPE] A 
                          left join [dat.COUNTY_CODE] B on 
                            A.route_id = B.route_id and (
                            ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                            ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                          ) where A.value_numeric in (1,2)")
    
     coverage.2 <- sqldf("select 
                          A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FSYSTEM, 
                          B.value_numeric as COUNTYCODE   
                          from [dat.F_SYSTEM] A 
                          left join [dat.COUNTY_CODE] B on 
                            A.route_id = B.route_id and (
                            ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                            ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                          ) where A.value_numeric in (1,2,3,4,5)")
    
     #stopped here 
    coverage.3 <- sqldf("select 
                          A.route_id,A.begin_point,A.end_point,A.data_item,A.value_numeric as FSYSTEM, 
                          B.value_numeric as COUNTYCODE   
                          from [dat.F_SYSTEM] A 
                          left join [dat.COUNTY_CODE] B on 
                            A.route_id = B.route_id and (
                            ( A.begin_point between B.begin_point and B.end_point and A.end_point between B.begin_point and B.end_point ) or
                            ( B.begin_point between A.begin_point and A.end_point and B.end_point between A.begin_point and A.end_point )
                          ) where A.value_numeric in (6)")
    
    if(sum(is.na(coverage.1$COUNTYCODE))==0&nrow(coverage.1)>0){
      type <- 1  
    }
  } # end TOLL_TYPE
  
  
  
  
  
  # submitted and complete
  if(type==1){
    grid.rect(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.028,width=unit(0.007,"npc"),height=unit(0.0125,"npc"),gp=gpar(fill="slategray",col="slategray"))
  }
  # submitted and complete
  if(type==2){
    grid.rect(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.028,width=unit(0.007,"npc"),height=unit(0.0125,"npc"),gp=gpar(fill="gray75",col="slategray"))
  }
  # not submitted
  if(type==3){
    grid.rect(x=startx+(C-1)*0.15+0.01,y=starty-(R-1)*0.028,width=unit(0.007,"npc"),height=unit(0.0125,"npc"),gp=gpar(fill="white",col="slategray"))
  }
}


