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
     variable, variable_type, variable_extent,variable_extent_fs)
{
     
     # functional system aggregation
     result1 <- data[state_code==state&year_record==year&data_item==variable,,]
     
     result1[,miles:=sum(end_point-begin_point),by=list(F_SYSTEM)]
     if(variable_extent %in% c("SP","FE*"))
     {
          result1[,expandedmiles:=sum((end_point-begin_point)*expansion_factor,na.rm = TRUE),by=list(F_SYSTEM)]
          if(variable_extent_fs==4) # F_SYSTEM 1 is unexpanded
          {
               result1[F_SYSTEM==1,expandedmiles:=NA]     
          }
          if(variable_extent_fs==2) # F_SYSTEM 1 is unexpanded
          {
               result1[Interstate==1,expandedmiles:=NA]     
          }          
     } else
     {
          result1[,expandedmiles:=NA,]
     }
     
     result1 <- switch(variable_type,
                      result1[,summaryFunc(value_numeric),by=list(F_SYSTEM,miles,expandedmiles)],
                      result1[,summaryFunc(as.Date(value_date)),by=list(F_SYSTEM,miles,expandedmiles)],
                      result1[,summaryFunc(value_numeric)[1:2],by=list(F_SYSTEM,miles,expandedmiles)]
     )
     
     result1[,groupCat:=F_SYSTEM+2]
     result1[,F_SYSTEM:=NULL]
     result1 <- result1[!is.na(miles),]
     
     # interstate aggregation
     result2 <- data[Interstate==1&state_code==state&year_record==year&data_item==variable,,]
     
     result2[,miles:=sum(end_point-begin_point),]
     if(variable_extent %in% c("SP"))
     {
          result2[,expandedmiles:=sum((end_point-begin_point)*expansion_factor),]
     } else
     {
          result2[,expandedmiles:=NA,]
     }
     
     result2 <- switch(variable_type,
                       result2[,summaryFunc(value_numeric),by=list(miles,expandedmiles)],
                       result2[,summaryFunc(as.Date(value_date)),by=list(miles,expandedmiles)],
                       result2[,summaryFunc(value_numeric)[1:2],by=list(miles,expandedmiles)]
     )
     
     result2[,groupCat:=1]
     
     # NHS aggregation
     result3 <- data[NHS==1&state_code==state&year_record==year&data_item==variable,,]
     
     result3[,miles:=sum(end_point-begin_point),]
     if(variable_extent %in% c("SP"))
     {
          result3[,expandedmiles:=sum((end_point-begin_point)*expansion_factor),]
     } else
     {
          result3[,expandedmiles:=NA,]
     }
     
     result3 <- switch(variable_type,
                       result3[,summaryFunc(value_numeric),by=list(miles,expandedmiles)],
                       result3[,summaryFunc(as.Date(value_date)),by=list(miles,expandedmiles)],
                       result3[,summaryFunc(value_numeric)[1:2],by=list(miles,expandedmiles)]
     )
     
     result3[,groupCat:=2]
     
     result <- rbind(result2,result3,result1)
     
     result[,count:=string_format(count)]
     result[,count.na:=string_format(count.na)]
     result[,miles:=string_format(miles)]
     result[,miles:=string_format(expandedmiles)]
     
    
     
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
          return(textGrob("No Data",gp=gpar(fontsize=15, col="Red")))
     }
}