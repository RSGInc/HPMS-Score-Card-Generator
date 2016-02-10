###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# This function formats the data into the necessary structure to be
# processed by the create_table function.
#
###########################################################################

create_outlier_report <- function(
    data, 
    state,
     year,
     variable
)
{
     
     outlier_threshold_high <- gVariables[Name==variable,Outlier_Max]
     outlier_threshold_low  <- gVariables[Name==variable,Outlier_Min]
     highlight_threshold    <- gVariables[Name==variable,OH_Thresh]
  
     d.l <- data[ data_item==variable&
                      state_code==state&
                      year_record==year&FACILITY_TYPE!=4,,]    
     
     result.1 <- d.l[(value_numeric>outlier_threshold_high|value_numeric<outlier_threshold_low),
                   list(miles=round(sum(end_point-begin_point,na.rm=TRUE),2),.N),
                   by=list(F_SYSTEM)]
     
     total.1 <- d.l[ ,list(totalmiles=round(sum(end_point-begin_point),2)),by=list(F_SYSTEM)]
     
     report.1 <- merge(total.1,result.1,by="F_SYSTEM",all.x=TRUE,all.y=FALSE)
     report.1[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     report.1[is.na(N),N:=0]
     
     report.1[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.1[,totalmiles:=NULL]
     
     report.1 <- report.1[!is.na(F_SYSTEM),]
     
     report.1[,groupCat:=F_SYSTEM+2]
     report.1[,F_SYSTEM:=NULL]
     
     d.l <- data[ data_item==variable&
                      state_code==state&
                      year_record==year&Interstate==1&FACILITY_TYPE!=4,,]    
     
     result.2 <- d.l[(value_numeric>outlier_threshold_high|value_numeric<outlier_threshold_low),
                     list(miles=round(sum(end_point-begin_point,na.rm=TRUE),2),.N),
                     ]
     
     total.2 <- d.l[ ,list(totalmiles=round(sum(end_point-begin_point),2)),]
     
     if(nrow(result.2)==0)
     {
       result.2 <- data.table(miles=0,N=0)
     }
     report.2 <- data.table(result.2,total.2)
     report.2[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     
     report.2[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.2[,totalmiles:=NULL]
     
     report.2[,groupCat:=1]
     
     d.l <- data[ data_item==variable&
                      state_code==state&
                      year_record==year&NHS==1&FACILITY_TYPE!=4,,]    
     
     result.3 <- d.l[(value_numeric>outlier_threshold_high|value_numeric<outlier_threshold_low),
                     list(miles=round(sum(end_point-begin_point,na.rm=TRUE),2),.N),
                     ]
     
     if(nrow(result.3)==0)
     {
       result.3 <- data.table(miles=0,N=0)
     }
     
     total.3 <- d.l[ ,list(totalmiles=round(sum(end_point-begin_point),2)),]
     
     report.3 <- data.table(result.3,total.3)
     report.3[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     
     report.3[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.3[,totalmiles:=NULL]
     
     report.3[,groupCat:=2]
     
     report <- rbind(report.2,report.3,report.1)
     
     report <- data.table(Name=report$groupCat,report)
     
     report[,miles:=string_format(miles)]
     report[,N:=string_format(N)]
     
     if(nrow(report)>0)
     {
          report <- merge(data.table(groupCat=1:4),report,by="groupCat",all.x=T)
          report <- data.table(melt(report,id.vars="groupCat"))
          report[,highlight:=ifelse(variable=="perc_miles"&as.numeric(value)>highlight_threshold,1,0)]
          report[variable=="perc_miles"&!is.na(value),value:=paste0(value,"%")]
          report[variable=="Name",value:=gF_SYSTEM_levels[as.numeric(groupCat)],]
          report[variable=="Name",variable:="Functional\nSystem",]
          report[variable=="N",variable:="Number of\nSections",]
          report[variable=="miles",variable:="Total Centerline\nMiles",]
          report[variable=="perc_miles",variable:="% of All\nMiles\nSubmitted",]
          report[is.na(highlight),highlight:=0]
          return(report)
     } else
     {
          return(NULL)
     }
}
