###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont 
# Modified: Matt Landis
#
# Description:
#
# This function does the year-over-year analysis and returns a table of results for
# the outlier analysis on the detailed review pages.
#
###########################################################################

getYOY <- function(data, year, yearcomparison, variable, yoy_change){
 

  #data <- data[!(F_SYTEMorig == 7 & NHS != 1), ]
  
  var.1    <- data[year_record == year & data_item==variable,
                   list(route_id, begin_point, end_point, value_numeric,
                        F_SYSTEM, NHS, Interstate)]
  var.1 <- expand(var.1)
  
  
  var.2    <- data[year_record == yearcomparison & data_item==variable,
                   list(route_id, begin_point, end_point, value_numeric)]
    
  var.yoy = var.1[var.2,on=.(route_id,begin_point,end_point)]

  

  
  # # Check result of join.  
  # # How many miles are in the joined data vs. each dataset separately?
  # m1 <- var.1[, .(miles = sum(end_point - begin_point)), by=list(F_SYSTEM)]
  # m2 <- data[year_record == yearcomparison & data_item==variable,
  #            list(route_id, begin_point, end_point, value_numeric, F_SYSTEM)][
  #              , .(miles = sum(end_point - begin_point)), by=list(F_SYSTEM)]
  # m12 <- var.yoy[, .(miles1 = sum(end_point.x - begin_point.x),
  #                    miles2 = sum(end_point.y - begin_point.y, na.rm=TRUE)),
  #                by=list(F_SYSTEM)]
  # 
  # # Miles by routeid
  # r1 <- var.1[F_SYSTEM == 1, .(miles = sum(end_point - begin_point)), by=list(route_id)]
  # r2 <- data[year_record == yearcomparison & data_item==variable & F_SYSTEM == 1,
  #            list(route_id, begin_point, end_point, value_numeric, F_SYSTEM)][
  #              , .(miles = sum(end_point - begin_point)), by=list(route_id)]
  # r12 <- var.yoy[F_SYSTEM == 1, .(miles1 = sum(end_point.x - begin_point.x),
  #                    miles2 = sum(end_point.y - begin_point.y, na.rm=TRUE)),
  #                by=list(route_id)]
  # 
  # r1r2 <- merge(r1, r2, by='route_id', all=TRUE)
  # r_all <- merge(r1r2, r12, by='route_id', all=TRUE)
  # r_all[miles1 > miles.x & miles1 > miles.y]
  # 
  # # Now we're getting somewhere.
  # 
  # var.yoy[route_id == '02SR016002']
   
  
  # Calculate report.1, with percent miles where values matched across years
  # by F_SYSTEM

  if ( yoy_change == 'N' ){
    result <- var.yoy[value_numeric == i.value_numeric,
                      .(miles=round(sum(end_point-begin_point), 2), 
                           N=round(sum(num_sections))),
                      by=list(F_SYSTEM)]
  }
  
  if ( yoy_change == 'Y' ){
    result <- var.yoy[value_numeric != i.value_numeric,
                      .(miles=round(sum(end_point-begin_point), 2), 
                           N=round(sum(num_sections))),
                      by=list(F_SYSTEM)]
  }
  
  result <- merge(data.table(F_SYSTEM=c(1,2)), result,
                  by="F_SYSTEM", all.x=TRUE)
  
  result[is.na(miles), miles := 0]
  result[is.na(N), N := 0]
  
  total <- var.1[, list(totalmiles=round(sum(end_point-begin_point), 2)),
                 by=list(F_SYSTEM)]
  
  report.1 <- merge(total, result, by="F_SYSTEM", all.x=TRUE, all.y=FALSE)
  report.1[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
  
  report.1[, perc_miles := ifelse(is.na(miles),
                                  0,
                                  as.character(round(miles/totalmiles,2)*100))]
  
  report.1[, totalmiles:=NULL]
  
  report.1 <- report.1[!is.na(F_SYSTEM),]
  
  report.1[, groupCat:=F_SYSTEM+2]
  report.1[, F_SYSTEM:=NULL]
  
  
  # Calcualte report.2, with percent miles where values matched across years
  # where Interstate == 1
  
  result <- var.yoy[value_numeric == i.value_numeric & Interstate==1,
                    .(miles = round(sum(end_point - begin_point), 2), 
                         N=round(sum(num_sections))),]

  total <- var.1[Interstate == 1,
                 list(totalmiles = round(sum(end_point - begin_point), 2)),]

  # Combine (cbind?) result and total
  if ( nrow(result) == 0 ){
    result <- data.table(miles=NA, N=NA)
  }
  
  if (nrow(total) == 0 ){
    total <- data.table(totalmiles = NA)
  }
  
  report.2 <- data.table(result, total)
  
  report.2[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
  
  report.2[, perc_miles := ifelse(is.na(miles),
                                  0,
                                  as.character(round(miles/totalmiles, 2) * 100))]
  
  report.2[, totalmiles := NULL]
  
  report.2[, groupCat := 1]
  
  
  # Calculate report.3, with percent miles where values matched across years 
  # where NHS == 1
  
  result <- var.yoy[value_numeric == i.value_numeric & NHS == 1,
                    .(miles=round(sum(end_point-begin_point),2), 
                         N=round(sum(num_sections))),]

  total <- var.1[NHS == 1, 
                 list(totalmiles=round(sum(end_point-begin_point), 2)), ]
  
  if ( nrow(result) == 0 ){
    result <- data.table(miles=NA, N=NA)
  }
  
  if (nrow(total) == 0 ){
    total <- data.table(totalmiles = NA)
  }
  
  report.3 <- data.table(result, total)
  report.3[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
  
  report.3[, perc_miles := ifelse(is.na(miles),
                                  0,
                                  as.character(round(miles/totalmiles, 2) * 100))]
  
  report.3[, totalmiles := NULL]
  
  report.3[, groupCat := 2]
  
  # Combine report.1, report.2, report.3 into a single data.table.
  #browser()
  
  report <- rbind(report.2, report.3, report.1)
  
  report <- data.table(Name=report$groupCat, report)
  
  report <- merge(data.table(groupCat=1:4), report, by="groupCat", all.x=T)
  
  report[is.na(perc_miles), perc_miles := as.character(0)]
  
  return(report)
  
  
}
