###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# This function produces a graphical object as an overall report for the
# first page.
#
###########################################################################

create_overall_report <- function(
     data,
     state,
     year
)
{
 
     # Description from Justin on how to count up the number of miles to show.
     # Centerline Miles = (Total length for F_System (sum) where F_System is in (1,2,3,4,5) and Facility_Type is in (1,2)) plus 
     # (total length for F_System where F_System is in (6) and Urban_Code <99999 and Facility_Type is in (1,2))
     # Lane Miles - 
     # Same as above but,  (Through_Lanes x F_System in (1,2,3,4,5)) plus (total length for Rural Minor Collectors x 2)

  
  
     numberOfRecords.1   <- data[state_code==state&year_record==year,.N,]
     numberOfRecords.2   <- data[state_code==state&year_record==year-1,.N,]

     numberOfCtrLine.1   <- data[state_code==state&year_record==year&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2))))
                                      ,sum(end_point-begin_point,na.rm = TRUE),]
     numberOfCtrLine.2   <- data[state_code==state&year_record==(year-1)&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2))))
                                 ,sum(end_point-begin_point,na.rm = TRUE),]

     numberOfLaneMiles.1   <- data[state_code==state&year_record==year&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2))))  
                                   ,sum(THROUGH_LANES*(end_point-begin_point),na.rm = TRUE),]
     
     numberOfLaneMiles.1   <- numberOfLaneMiles.1 + 
                              data[state_code==state&year_record==year&data_item=="F_SYSTEM"&
                                    (F_SYTEMorig==6)&(URBAN_CODE==99999)  
                                   ,sum(2*(end_point-begin_point),na.rm = TRUE),]
     
     numberOfLaneMiles.2   <- data[state_code==state&year_record==(year-1)&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2))))
                                   ,sum(THROUGH_LANES*(end_point-begin_point),na.rm = TRUE),]
     
     numberOfLaneMiles.2   <- numberOfLaneMiles.2 + 
                              data[state_code==state&year_record==(year-1)&data_item=="F_SYSTEM"&
                                    (F_SYTEMorig==6)&(URBAN_CODE==99999)  
                                   ,sum(2*(end_point-begin_point),na.rm = TRUE),]

     numberOfVariables.1 <- length(data[state_code==state&year_record==year,unique(data_item),])
     numberOfVariables.2 <- length(data[state_code==state&year_record==year-1,unique(data_item),])

     numberRoutes.1      <- nrow(unique(data[state_code==state&year_record==year&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2)))),list(route_id),]))
     numberRoutes.2      <- nrow(unique(data[state_code==state&year_record==year-1&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2)))),list(route_id),]))

     numberSegments.1    <- nrow(unique(data[state_code==state&year_record==year&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2)))),list(route_id,begin_point,end_point),]))
     numberSegments.2    <- nrow(unique(data[state_code==state&year_record==year-1&data_item=="F_SYSTEM"&
                                   (((F_SYTEMorig%in%c(1,2,3,4,5))&(FACILITY_TYPE%in%c(1,2)))|
                                    ((F_SYTEMorig==6)&(URBAN_CODE<99999)&(FACILITY_TYPE%in%c(1,2)))),list(route_id,begin_point,end_point),]))

     result <- data.table(
          Label=c("Number of Records","Number of Data Items","Number of Routes","Number of Sections","Total Center Line Miles*","Total Lane Miles*"),
          Val.1=string_format(c(numberOfRecords.1,numberOfVariables.1,numberRoutes.1,numberSegments.1,numberOfCtrLine.1,numberOfLaneMiles.1)),
          Val.2=string_format(c(numberOfRecords.2,numberOfVariables.2,numberRoutes.2,numberSegments.2,numberOfCtrLine.2,numberOfLaneMiles.2))#,
     )
     
     setnames(result,"Val.1",paste0("Year ",year))
     setnames(result,"Val.2",paste0("Year ",year-1))
     setnames(result,"Label","")
     
     return(result)
     
}