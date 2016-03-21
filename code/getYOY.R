getYOY <- function(data,year,yearcomparison,variable)
{

     var.1    <- data[year_record==year&data_item==variable,list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,NHS,Interstate)]
     var.2    <- data[year_record==yearcomparison&data_item==variable,list(route_id,begin_point,end_point,value_numeric)]
     
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
     
     result <- var.yoy[value_numeric.x==value_numeric.y&Interstate==1,list(miles=round(sum(end_point.x-begin_point.x),2),.N),]
     
     total <- var.1[Interstate==1,list(totalmiles=round(sum(end_point-begin_point),2)),]
     
     report.2 <- data.table(result,total)
     report.2[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     
     report.2[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.2[,totalmiles:=NULL]
     
     report.2[,groupCat:=1]
     
     result <- var.yoy[value_numeric.x==value_numeric.y&NHS==1,list(miles=round(sum(end_point.x-begin_point.x),2),.N),]
     
     total <- var.1[NHS==1,list(totalmiles=round(sum(end_point-begin_point),2)),]
     
     report.3 <- data.table(result,total)
     report.3[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
     
     report.3[,perc_miles:=ifelse(is.na(miles),0,as.character(round(miles/totalmiles,2)*100))]
     
     report.3[,totalmiles:=NULL]
     
     report.3[,groupCat:=2]
     
     report <- rbind(report.2,report.3, report.1)
     
     report <- data.table(Name=report$groupCat,report)
     
     report <- merge(data.table(groupCat=1:4),report,by="groupCat",all.x=T)
     
     report[is.na(perc_miles),perc_miles:=0]
     
     return(report)
  
  
}