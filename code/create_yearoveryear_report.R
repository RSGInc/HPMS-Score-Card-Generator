###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This compares the analysis year against the comparison year at the section
# level.
# 
# This function formats the data into the necessary structure to be
# processed by the create_table function.
#
###########################################################################

# TODO:  This code uses virtually the same code as getYOY().  Modify to use
# it instead of repeating

create_yearoveryear_report <- function(
  data,
  state,
  year,
  variable,
  yearcomparison
)
{
  
  highlight_threshold    <- gVariables[Name==variable,YOYH_Thresh]
  
  var.1    <- data[state_code==state&year_record==year&data_item==variable&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,NHS,Interstate)]
  var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE!=4,list(route_id,begin_point,end_point,value_numeric)]
  
  var.yoy <- sqldf("
                      select 
                      A.route_id,
                      A.F_SYSTEM,
                      A.Interstate,
                      A.NHS,
                      A.begin_point as [begin_point.x],
                      A.end_point   as [end_point.x],
                      A.value_numeric as [value_numeric.x],
                      B.value_numeric as [value_numeric.y]
                      from [var.1] A 
                      left join [var.2] B on 
                      A.route_id = B.route_id and 
                      (
                      (
                      ( A.begin_point <= B.end_point   ) and
                      ( A.end_point   >= B.begin_point ) and 
                      ( A.begin_point >= B.begin_point ) and 
                      ( A.end_point   <= B.end_point   )
                      ) or
                      (
                      ( B.begin_point <= A.end_point   ) and
                      ( B.end_point   >= A.begin_point ) and 
                      ( B.begin_point >= A.begin_point ) and 
                      ( B.end_point   <= A.end_point   )
                      )
                      ) 
                      ")
  
  var.yoy <- data.table(var.yoy)
  
  result <- var.yoy[value_numeric.x==value_numeric.y,list(miles=round(sum(end_point.x-begin_point.x),2),.N),by=list(F_SYSTEM)]
  
  result <- merge(data.table(F_SYSTEM=c(1,2)),result,by="F_SYSTEM",all.x=TRUE)
  
  result[is.na(miles),miles:=0]
  result[is.na(N),N:=0]
  
  total <- var.1[ ,list(totalmiles=round(sum(end_point-begin_point),2)),by=list(F_SYSTEM)]
  
  report.1 <- merge(total,result,by="F_SYSTEM",all.x=TRUE,all.y=FALSE)
  report.1[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
  
  report.1[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
  
  report.1[,totalmiles:=NULL]
  
  report.1 <- report.1[!is.na(F_SYSTEM),]
  
  report.1[,groupCat:=F_SYSTEM+2]
  report.1[,F_SYSTEM:=NULL]
  
  # Calculate report.2, with % miles where Interstate == 1
  result <- var.yoy[value_numeric.x==value_numeric.y&Interstate==1,list(miles=round(sum(end_point.x-begin_point.x),2),.N),]
  
  total <- var.1[Interstate==1,list(totalmiles=round(sum(end_point-begin_point),2)),]
  
  # Combine (cbind?) result and total
  if ( nrow(result) == 0 ){
    result <- data.table(miles=NA, N=0)
  }
  
  if (nrow(total) == 0 ){
    total <- data.table(totalmiles = NA)
  }
  
  report.2 <- data.table(result,total)
  report.2[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
  
  report.2[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
  
  report.2[,totalmiles:=NULL]
  
  report.2[,groupCat:=1]
  
  # Calculate report.3 with % miles where NHS == 1
  result <- var.yoy[value_numeric.x==value_numeric.y&NHS==1,list(miles=round(sum(end_point.x-begin_point.x),2),.N),]
  
  total <- var.1[NHS==1,list(totalmiles=round(sum(end_point-begin_point),2)),]
  
  # Combine (cbind?) result and total
  if ( nrow(result) == 0 ){
    result <- data.table(miles=NA, N=0)
  }
  
  if (nrow(total) == 0 ){
    total <- data.table(totalmiles = NA)
  }
  
  report.3 <- data.table(result,total)
  report.3[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
  
  report.3[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
  
  report.3[,totalmiles:=NULL]
  
  report.3[,groupCat:=2]
  
  
  report <- rbind(report.2,report.3, report.1)
  
  report <- data.table(Name=report$groupCat,report)
  
  report[,miles:=string_format(miles)]
  report[,N:=string_format(N)]
  
  if(nrow(report)>0)
  {
    report <- merge(data.table(groupCat=1:4),report,by="groupCat",all.x=T)
    report[, groupCat := as.character(groupCat)]
    report[, Name := as.character(Name)]
    report <- data.table(melt(report,id.vars="groupCat"))
    report[,highlight:=ifelse(
      variable=="perc_miles" & as.numeric(str_replace(value, ',', '')) > highlight_threshold,
      1, 0)]
    
    report[variable=="perc_miles"&!is.na(value),value:=paste0(value,"%")]
    report[variable=="Name",value:=gF_SYSTEM_levels[as.numeric(groupCat)],]
    report[variable=="Name",variable:="Functional\nSystem",]
    report[variable=="N",variable:="Number of\nSections",]
    report[variable=="miles",variable:="Total Centerline\nMiles",]
    report[variable=="perc_miles",variable:="% of All\n Miles\nSubmitted",]
    report[is.na(highlight),highlight:=0]
    return(report)
  } else
  {
    return(NULL)
  }
}
