###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont 
#
#
# Description:
#
# This function does the adjaceny analysis and returns a table of results for
# the outlier analysis on the detailed review pages.
#
###########################################################################

getAdjaceny <- function(data,year,variable)
{

     data <- data[!(F_SYTEMorig==7&NHS!=1),
                  .(year_record,state_code,route_id,begin_point,end_point,
                  data_item,value_numeric,value_text,value_date,F_SYSTEM,NHS,FACILITY_TYPE,THROUGH_LANES,URBAN_CODE,F_SYTEMorig,
                  Interstate)]
  
     data <- unique(data)
     
     d.l <- data[data_item==variable&year_record==year,]
     
     d.l <- unique(d.l) 
     d.r <- d.l
     
     d.l$match_point <- d.l$end_point
     d.r$match_point <- d.r$begin_point
     
     d.adj <- merge(
          d.l,
          d.r,
          by=c("route_id","match_point"),
          all.x=TRUE, all.y=FALSE
     )
     
     result <- d.adj[value_numeric.x==value_numeric.y,list(miles=round(sum(end_point.x-begin_point.x),2),.N),by=list(F_SYSTEM.x)]
     
     result <- merge(data.table(F_SYSTEM.x=c(1,2)),result,by="F_SYSTEM.x",all.x=TRUE)
     
     result[is.na(miles),miles:=0]
     result[is.na(N),N:=0]
     
     setnames(result,"F_SYSTEM.x","F_SYSTEM")
     #setnames(result,"V1","miles")
     
     total <- d.l[ ,list(totalmiles=round(sum(end_point-begin_point),2)),by=list(F_SYSTEM)]
     
     report.1 <- merge(total,result,by="F_SYSTEM",all.x=TRUE,all.y=FALSE)
     report.1[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     
     report.1[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.1[,totalmiles:=NULL]
     
     report.1 <- report.1[!is.na(F_SYSTEM),]
     
     report.1[,groupCat:=F_SYSTEM+2]
     report.1[,F_SYSTEM:=NULL]
     
     
     
     d.l <- data[data_item==variable&year_record==year&Interstate==1,,]
     
     d.r <- d.l
     
     d.l$match_point <- d.l$end_point
     d.r$match_point <- d.r$begin_point
     
     d.adj <- merge(
          d.l,
          d.r,
          by=c("route_id","match_point"),
          all.x=TRUE, all.y=FALSE
     )
     
     result <- d.adj[value_numeric.x==value_numeric.y,list(miles=round(sum(end_point.x-begin_point.x),2),.N),]
     
     total <- d.l[ ,list(totalmiles=round(sum(end_point-begin_point),2)),]
     
     if(nrow(result)==0)
     {
       result <- data.table(miles=0,N=0)
     }
     
     report.2 <- data.table(result,total)
     report.2[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     
     report.2[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.2[,totalmiles:=NULL]
     
     report.2[,groupCat:=1]
     
     
     d.l <- data[data_item==variable&year_record==year&NHS==1,,]
     
     d.r <- d.l
     
     d.l$match_point <- d.l$end_point
     d.r$match_point <- d.r$begin_point
     
     d.adj <- merge(
          d.l,
          d.r,
          by=c("route_id","match_point"),
          all.x=TRUE, all.y=FALSE
     )
     
     result <- d.adj[value_numeric.x==value_numeric.y,list(miles=round(sum(end_point.x-begin_point.x),2),.N),]

     total <- d.l[ ,list(totalmiles=round(sum(end_point-begin_point),2)),]
     
     if(nrow(result)==0)
     {
       result <- data.table(miles=0,N=0)
     }
     
     report.3 <- data.table(result,total)
     report.3[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     
     report.3[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.3[,totalmiles:=NULL]
     
     report.3[,groupCat:=2]
     
     report <- rbind(report.2,report.3,report.1)
     
     report <- data.table(Name=report$groupCat,report)
     
     report <- merge(data.table(groupCat=1:4),report,by="groupCat",all.x=T)
     
     report[is.na(perc_miles),perc_miles:=as.character(0)]
     
     return(report)
  
}