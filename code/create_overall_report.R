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
     numberOfRecords.1   <- data[state_code==state&year_record==year,.N,]
     numberOfRecords.2   <- data[state_code==state&year_record==year-1,.N,]
     numberOfRecords.3   <- data[state_code==state&year_record==year-2,.N,]
     numberOfRecords.4   <- data[state_code==state&year_record==year-3,.N,]
     numberOfRecords.5   <- data[state_code==state&year_record==year-4,.N,]
     
     numberOfCtrLine.1   <- data[state_code==state&year_record==year  ,sum(end_point-begin_point,na.rm = TRUE),]
     numberOfCtrLine.2   <- data[state_code==state&year_record==year-1,sum(end_point-begin_point,na.rm = TRUE),]
     numberOfCtrLine.3   <- data[state_code==state&year_record==year-2,sum(end_point-begin_point,na.rm = TRUE),]
     numberOfCtrLine.4   <- data[state_code==state&year_record==year-3,sum(end_point-begin_point,na.rm = TRUE),]
     numberOfCtrLine.5   <- data[state_code==state&year_record==year-4,sum(end_point-begin_point,na.rm = TRUE),]
     
     numberOfLaneMiles.1   <- data[state_code==state&year_record==year  ,sum(THROUGH_LANES*(end_point-begin_point),na.rm = TRUE),]
     numberOfLaneMiles.2   <- data[state_code==state&year_record==year-1,sum(THROUGH_LANES*(end_point-begin_point),na.rm = TRUE),]
     numberOfLaneMiles.3   <- data[state_code==state&year_record==year-2,sum(THROUGH_LANES*(end_point-begin_point),na.rm = TRUE),]
     numberOfLaneMiles.4   <- data[state_code==state&year_record==year-3,sum(THROUGH_LANES*(end_point-begin_point),na.rm = TRUE),]
     numberOfLaneMiles.5   <- data[state_code==state&year_record==year-4,sum(THROUGH_LANES*(end_point-begin_point),na.rm = TRUE),]
     
     numberOfVariables.1 <- length(data[state_code==state&year_record==year,unique(data_item),])
     numberOfVariables.2 <- length(data[state_code==state&year_record==year-1,unique(data_item),])
     numberOfVariables.3 <- length(data[state_code==state&year_record==year-2,unique(data_item),])
     numberOfVariables.4 <- length(data[state_code==state&year_record==year-3,unique(data_item),])
     numberOfVariables.5 <- length(data[state_code==state&year_record==year-4,unique(data_item),])
     
     numberRoutes.1      <- nrow(unique(data[state_code==state&year_record==year,list(route_id),]))
     numberRoutes.2      <- nrow(unique(data[state_code==state&year_record==year-1,list(route_id),]))
     numberRoutes.3      <- nrow(unique(data[state_code==state&year_record==year-2,list(route_id),]))
     numberRoutes.4      <- nrow(unique(data[state_code==state&year_record==year-3,list(route_id),]))
     numberRoutes.5      <- nrow(unique(data[state_code==state&year_record==year-4,list(route_id),]))
     
     numberSegments.1    <- nrow(unique(data[state_code==state&year_record==year,list(route_id,begin_point,end_point),]))
     numberSegments.2    <- nrow(unique(data[state_code==state&year_record==year-1,list(route_id,begin_point,end_point),]))
     numberSegments.3    <- nrow(unique(data[state_code==state&year_record==year-2,list(route_id,begin_point,end_point),]))
     numberSegments.4    <- nrow(unique(data[state_code==state&year_record==year-3,list(route_id,begin_point,end_point),]))
     numberSegments.5    <- nrow(unique(data[state_code==state&year_record==year-4,list(route_id,begin_point,end_point),]))
     
     result <- data.table(
          Label=c("Number of Records","Number of Data Items","Number of Routes","Number of Sections","Total Center Line Miles","Total Lane Miles"),
          Val.1=string_format(c(numberOfRecords.1,numberOfVariables.1,numberRoutes.1,numberSegments.1,numberOfCtrLine.1,numberOfLaneMiles.1)),
          Val.2=string_format(c(numberOfRecords.2,numberOfVariables.2,numberRoutes.2,numberSegments.2,numberOfCtrLine.2,numberOfLaneMiles.2)),
          Val.3=string_format(c(numberOfRecords.3,numberOfVariables.3,numberRoutes.3,numberSegments.3,numberOfCtrLine.3,numberOfLaneMiles.3)),
          Val.4=string_format(c(numberOfRecords.4,numberOfVariables.4,numberRoutes.4,numberSegments.4,numberOfCtrLine.4,numberOfLaneMiles.4)),
          Val.5=string_format(c(numberOfRecords.5,numberOfVariables.5,numberRoutes.5,numberSegments.5,numberOfCtrLine.5,numberOfLaneMiles.5))
     )
     
     setnames(result,"Val.1",paste0("Year ",year))
     setnames(result,"Val.2",paste0("Year ",year-1))
     setnames(result,"Val.3",paste0("Year ",year-2))
     setnames(result,"Val.4",paste0("Year ",year-3))
     setnames(result,"Val.5",paste0("Year ",year-4))
     setnames(result,"Label","")
     
     #ob <- tableGrob(result,
     #                rows=NULL, 
     #                core.just = "right",
     #                col.just="right",
     #                gpar.coretext = gpar(col = "black",fontsize=6.5),
     #                gpar.coltext = gpar(col = "black",fontsize=7, fontface = "bold"),
     #                padding.h=unit(0.1,units="inches"),padding.v=unit(0.1,units="inches")
     #)
     
     #ob <- vertically_align(ob)
     
     return(result)
     
}