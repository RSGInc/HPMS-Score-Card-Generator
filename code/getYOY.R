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
                   list(route_id, begin_point, end_point, value_numeric, value_text, value_date,
                        F_SYSTEM, NHS, Interstate, num_sections)]
  
  var.2    <- data[year_record == yearcomparison & data_item==variable,
                   list(route_id, begin_point, end_point, value_numeric, value_text, value_date)]
  
  if ( variable %in% c('YEAR_LAST_IMPROV', 'YEAR_LAST_CONSTRUCTION') ){
    var.1[is.na(value_numeric) | value_numeric == 0, value_numeric := year(value_date)]
    var.2[is.na(value_numeric) | value_numeric == 0, value_numeric := year(value_date)]
  }
  
  if ( variable == 'WIDENING_OBSTACLE'){
    setnames(var.1, 'value_text', 'value.1')
    setnames(var.2, 'value_text', 'value.2')
  } else {
    setnames(var.1, 'value_numeric', 'value.1')
    setnames(var.2, 'value_numeric', 'value.2')
  }
  
  var.yoy = merge(var.1, var.2, by = c('route_id', 'begin_point', 'end_point'),
                  all=FALSE)
  
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
  
  if ( nrow(var.yoy) == 0 ){
    report <- data.table(groupCat = 1:4, Name = 1:4, miles = NA, N = NA, perc_miles = NA)
  } else {
    
    if ( yoy_change == 'N' ){
      result <- var.yoy[value.1 == value.2,
                        .(miles=round(sum(end_point-begin_point), 2), 
                          N=round(sum(num_sections))),
                        by=list(F_SYSTEM)]
    }
    
    if ( yoy_change == 'Y' ){
      result <- var.yoy[value.1 != value.2,
                        .(miles=round(sum(end_point-begin_point), 2), 
                          N=round(sum(num_sections))),
                        by=list(F_SYSTEM)]
    }
    
    result <- merge(data.table(F_SYSTEM=c(1,2)), result,
                    by="F_SYSTEM", all.x=TRUE)
    
    result[is.na(miles), miles := 0]
    result[is.na(N), N := 0]
    
    total <- var.yoy[, list(totalmiles=round(sum(end_point-begin_point), 2)),
                   by=list(F_SYSTEM)]
    
    report.1 <- merge(total, result, by="F_SYSTEM", all.x=TRUE, all.y=FALSE)
    report.1[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    
    report.1[, perc_miles := ifelse(is.na(miles),
                                    0,
                                    as.character(round(miles/totalmiles,2)*100))]
    
    # report.1[, totalmiles:=NULL]
    
    report.1 <- report.1[!is.na(F_SYSTEM),]
    
    report.1[, groupCat:=F_SYSTEM+2]
    report.1[, F_SYSTEM:=NULL]
    
    
    # Calcualte report.2, with percent miles where values matched across years
    # where Interstate == 1
    
    if ( yoy_change == 'N' ){
      result <- var.yoy[value.1 == value.2 & Interstate==1,
                        .(miles=round(sum(end_point - begin_point), 2), 
                          N=round(sum(num_sections))),]
    }
    
    if ( yoy_change == 'Y' ){
      result <- var.yoy[value.1 != value.2 & Interstate==1,
                        .(miles=round(sum(end_point - begin_point), 2), 
                          N=round(sum(num_sections))),]
    }
    
    total <- var.yoy[Interstate == 1,
                   list(totalmiles = round(sum(end_point - begin_point), 2)),]
    
    # Combine (cbind?) result and total
    if ( nrow(result) == 0 ){
      result <- data.table(miles=0, N=0)
    }
    
    if (nrow(total) == 0 ){
      total <- data.table(totalmiles = NA)
    }
    
    report.2 <- data.table(result, total)
    
    # setting values to 0 where there are no merges. this mean that the state
    # had no lane miles outside the thresholds set
    report.2[is.na(miles), miles := 0] 
    
    report.2[, perc_miles := ifelse(is.na(miles), 0,
                      as.character(round(miles/totalmiles, 2) * 100))]
    
    # report.2[, totalmiles := NULL]
    
    report.2[, groupCat := 1]
    
    
    # Calculate report.3, with percent miles where values matched across years 
    # where NHS == 1
    
    if ( yoy_change == 'N' ){
      result <- var.yoy[value.1 == value.2 & NHS == 1,
                        .(miles=round(sum(end_point-begin_point),2), 
                          N=round(sum(num_sections))),]
    }
    
    if ( yoy_change == 'Y' ){
      result <- var.yoy[value.1 != value.2 & NHS == 1,
                        .(miles=round(sum(end_point-begin_point),2), 
                          N=round(sum(num_sections))),]
    }
    
    
    total <- var.yoy[NHS == 1, 
                   list(totalmiles=round(sum(end_point-begin_point), 2)), ]
    
    if ( nrow(result) == 0 ){
      result <- data.table(miles=0, N=0)
    }
    
    if (nrow(total) == 0 ){
      total <- data.table(totalmiles = NA)
    }
    
    report.3 <- data.table(result, total)
    report.3[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    
    report.3[, perc_miles := ifelse(is.na(miles),
                                    0,
                                    as.character(round(miles/totalmiles, 2) * 100))]
    
    # report.3[, totalmiles := NULL]
    
    report.3[, groupCat := 2]
    
    # Combine report.1, report.2, report.3 into a single data.table.
    
    report <- rbind(report.2, report.3, report.1)
    
    report <- data.table(Name=report$groupCat, report)
    
    report <- merge(data.table(groupCat=1:4), report, by="groupCat", all.x=T)
    
    # if ( any( report[!is.na(perc_miles), as.numeric(perc_miles)] > 100 |
    #           report[!is.na(perc_miles), as.numeric(perc_miles)] == 0 )) browser()
    # 
    #report[is.na(perc_miles), perc_miles := as.character(0)]
  }
  return(report)
}
