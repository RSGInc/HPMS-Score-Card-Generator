###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# This function creates the summary report and passes an object to the
# create_table_grob function.
#
###########################################################################

create_summary_report <- function(
     data,
     state,
     year,
     variable, variable_type, variable_extent,variable_extent_fs,ramps)
{
     
     # functional system aggregation
     if(ramps)
     {
        result1 <- data[state_code==state&year_record==year&data_item==variable&FACILITY_TYPE==4,,]
     } else {
        result1 <- data[state_code==state&year_record==year&data_item==variable&FACILITY_TYPE!=4,,]
     }
     
     result1[,miles:=sum(end_point-begin_point),by=list(F_SYSTEM)]
     result1[,lanemiles:=sum((end_point-begin_point)*THROUGH_LANES,na.rm = TRUE),by=list(F_SYSTEM)]
     if(variable_extent %in% c("SP","FE*"))
     {
          result1[,expandedmiles:=sum((end_point-begin_point)*expansion_factor,na.rm = TRUE),by=list(F_SYSTEM)]
          result1[,expandedlanemiles:=sum((end_point-begin_point)*THROUGH_LANES*expansion_factor,na.rm = TRUE),by=list(F_SYSTEM)]
          if(variable_extent_fs==4) # F_SYSTEM 1 is unexpanded
          {
               result1[F_SYSTEM==1,expandedmiles:=NA]     
               result1[F_SYSTEM==1,expandedlanemiles:=NA]     
          }
          if(variable_extent_fs==2) # F_SYSTEM 1 is unexpanded
          {
               result1[Interstate==1,expandedmiles:=NA]     
               result1[Interstate==1,expandedlanemiles:=NA]     
          }          
     } else
     {
          result1[,expandedmiles:=NA,]
          result1[,expandedlanemiles:=NA,]
     }
     
     result1 <- switch(variable_type,
                      result1[,summaryFunc(value_numeric),by=list(F_SYSTEM,miles,expandedmiles,lanemiles,expandedlanemiles)],
                      result1[,summaryFunc(as.Date(value_date)),by=list(F_SYSTEM,miles,expandedmiles,lanemiles,expandedlanemiles)],
                      result1[,summaryFunc(value_numeric)[1:2],by=list(F_SYSTEM,miles,expandedmiles,lanemiles,expandedlanemiles)]
     )
     
     result1[,groupCat:=F_SYSTEM+2]
     result1[,F_SYSTEM:=NULL]
     result1 <- result1[!is.na(miles),]
     
     # interstate aggregation
     if(ramps)
     {
        result2 <- data[Interstate==1&state_code==state&year_record==year&data_item==variable&FACILITY_TYPE==4,,]
     } else {
        result2 <- data[Interstate==1&state_code==state&year_record==year&data_item==variable&FACILITY_TYPE!=4,,]
     }
     
     result2[,miles:=sum(end_point-begin_point),]
     result2[,lanemiles:=sum((end_point-begin_point)*THROUGH_LANES,na.rm = TRUE)]
     if(variable_extent %in% c("SP"))
     {
          result2[,expandedmiles:=sum((end_point-begin_point)*expansion_factor),]
          result2[,expandedlanemiles:=sum((end_point-begin_point)*THROUGH_LANES*expansion_factor,na.rm = TRUE)]
     } else
     {
          result2[,expandedmiles:=NA,]
          result2[,expandedlanemiles:=NA,]
     }
     
     result2 <- switch(variable_type,
                       result2[,summaryFunc(value_numeric),by=list(miles,expandedmiles,lanemiles,expandedlanemiles)],
                       result2[,summaryFunc(as.Date(value_date)),by=list(miles,expandedmiles,lanemiles,expandedlanemiles)],
                       result2[,summaryFunc(value_numeric)[1:2],by=list(miles,expandedmiles,lanemiles,expandedlanemiles)]
     )
     
     result2[,groupCat:=1]
     
     # NHS aggregation
     if(ramps)
     {
        result3 <- data[NHS==1&state_code==state&year_record==year&data_item==variable&FACILITY_TYPE==4,,]
     } else {
        result3 <- data[NHS==1&state_code==state&year_record==year&data_item==variable&FACILITY_TYPE!=4,,]
     }
     
     result3[,miles:=sum(end_point-begin_point),]
     result3[,lanemiles:=sum((end_point-begin_point)*THROUGH_LANES,na.rm = TRUE)]
     if(variable_extent %in% c("SP"))
     {
          result3[,expandedmiles:=sum((end_point-begin_point)*expansion_factor),]
          result3[,expandedlanemiles:=sum((end_point-begin_point)*THROUGH_LANES*expansion_factor),]
     } else
     {
          result3[,expandedmiles:=NA,]
          result3[,expandedlanemiles:=NA,]
     }
     
     result3 <- switch(variable_type,
                       result3[,summaryFunc(value_numeric),by=list(miles,expandedmiles,lanemiles,expandedlanemiles)],
                       result3[,summaryFunc(as.Date(value_date)),by=list(miles,expandedmiles,lanemiles,expandedlanemiles)],
                       result3[,summaryFunc(value_numeric)[1:2],by=list(miles,expandedmiles,lanemiles,expandedlanemiles)]
     )
     
     result3[,groupCat:=2]
     
     result <- rbind(result2,result3,result1)
     
     result[,count:=string_format(count)]
     result[,count.na:=string_format(count.na)]
     result[,miles:=string_format(miles)]
     result[,expandedmiles:=string_format(expandedmiles)]
     
     # merges in the empty f_systems so full tables are displayed
     if(nrow(result)> 0)
     {
          result <- merge(data.table(groupCat=1:length(gF_SYSTEM_levels)),result,by="groupCat",all.x=T)
     }
     
     if(variable_type==1)
     {
          result[,min:=string_format(min)]    
          result[,mean:=string_format(mean)]
          result[,median:=string_format(median)]
          result[,max:=string_format(max)]
     }
     if(variable_type==2)
     {
          result[,min:=year(min)]    
          result[,mean:=year(mean)]
          result[,median:=year(median)]
          result[,max:=year(max)]
     }
     
     if(nrow(result)>0)
     {
          ob <- create_table_grob(result,variable_type)
          
          return(ob)
     } else
     {
          return(textGrob(NoDataString,gp=gpar(fontsize=12, col="Red")))
     }
}
