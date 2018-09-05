###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont 
#
#
# Description:
#
# This function does the adjacency analysis and returns a table of results for
# the outlier analysis on the detailed review pages.
#
###########################################################################

getAdjacency <- function(data, year, variable, adjacency_change){
  
  #if ( variable == 'RUTTING' ) browser()
  
  # Retain only one row per section_id
  data <- unique(data[data_item == variable & year_record == year,
                      .(route_id, section_id, begin_point_og, end_point_og,
                        value_numeric, F_SYSTEM, Interstate, NHS)])
  
  d.l <- data[order(route_id, begin_point_og)]
  
  if ( nrow(d.l) == 0 ){
    report.1 <- data.table(groupCat = c(3, 4), N = NA, miles = NA, perc_miles = NA)
  } else {
    
    # Instead of a join, use a lagged value_numeric
    # # https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group
    d.adj <- d.l[, value_numeric_lag := data.table::shift(value_numeric),
                 by = route_id]
    d.adj <- d.adj[!is.na(value_numeric_lag)]
    
    if ( adjacency_change == 'N' ){
      result <- d.adj[value_numeric == value_numeric_lag,
                      list(miles=round(sum(end_point_og - begin_point_og), 2), .N),
                      by = list(F_SYSTEM)]
    }
    
    if (adjacency_change == 'Y' ){
      result <- d.adj[value_numeric != value_numeric_lag,
                      list(miles = round(sum(end_point_og - begin_point_og), 2), .N),
                      by = list(F_SYSTEM)]
    }  
    
    result <- merge(data.table(F_SYSTEM=c(1,2)),result,by="F_SYSTEM",all.x=TRUE)
    
    result[is.na(miles),miles:=0]
    result[is.na(N),N:=0]
    
    total <- d.adj[ ,list(totalmiles=round(sum(end_point_og-begin_point_og),2)),
                    by=list(F_SYSTEM)]
    
    report.1 <- merge(total,
                      result,
                      by="F_SYSTEM",all.x=TRUE,all.y=FALSE)
    
    report.1[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    
    report.1[,perc_miles:=ifelse(is.na(miles),0,
                                 as.character(round(miles/totalmiles,2)*100))]
    
    report.1[,totalmiles:=NULL]
    
    report.1 <- report.1[!is.na(F_SYSTEM),]
    
    report.1[,groupCat:=F_SYSTEM + 2]
    report.1[,F_SYSTEM := NULL]
  }
  
  d.l = data[Interstate == 1][order(route_id, begin_point_og)]
  
  if ( nrow(d.l) == 0 ){
    report.2 <- data.table(groupCat = 1, N = NA, miles = NA, perc_miles = NA)
    
  } else {
    
    d.adj <- d.l[, value_numeric_lag := data.table::shift(value_numeric),
                 by = route_id]
    d.adj <- d.adj[!is.na(value_numeric_lag)]
    
    if ( adjacency_change == 'N'){
      result <- d.adj[value_numeric == value_numeric_lag,
                      list(miles=round(sum(end_point_og - begin_point_og), 2), .N),]
      
    }
    
    if ( adjacency_change == 'Y'){
      result <- d.adj[value_numeric != value_numeric_lag,
                      list(miles=round(sum(end_point_og - begin_point_og), 2), .N),]
      
    }
    total <- d.adj[, list(totalmiles=round(sum(end_point_og - begin_point_og),2)),]
    
    if(nrow(result)==0){
      result <- data.table(miles=0,N=0)
    }
    
    report.2 <- data.table(result, total)
    report.2[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    
    report.2[, perc_miles := ifelse(is.na(miles), 0, as.character(round(miles/totalmiles, 2)*100))]
    
    report.2[, totalmiles := NULL]
    
    report.2[, groupCat := 1]
  }
  
  
  d.l = data[NHS == 1][order(route_id, begin_point_og)]
  
  if ( nrow(d.l) == 0 ){
    report.3 <- data.table(groupCat = 2, N = NA, miles = NA, perc_miles = NA)
  } else {
    
    d.adj <- d.l[, value_numeric_lag := data.table::shift(value_numeric),
                 by = route_id]
    d.adj <- d.adj[!is.na(value_numeric_lag)]
    
    if ( adjacency_change == 'N'){
      result <- d.adj[value_numeric == value_numeric_lag,
                      list(miles=round(sum(end_point_og-begin_point_og),2), .N),]
    }
    
    if ( adjacency_change == 'Y'){
      result <- d.adj[value_numeric != value_numeric_lag,
                      list(miles=round(sum(end_point_og-begin_point_og),2), .N),]
    }
    
    
    total <- d.adj[ , list(totalmiles = round(sum(end_point_og - begin_point_og), 2)),]
    
    if(nrow(result) == 0){
      result <- data.table(miles=0, N=0)
    }
    
    report.3 <- data.table(result, total)
    report.3[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    
    report.3[, perc_miles := ifelse(is.na(miles), 0, as.character(round(miles / totalmiles, 2) * 100))]
    
    report.3[, totalmiles := NULL]
    
    report.3[, groupCat := 2]
  }
  
  report <- rbind(report.2, report.3, report.1)
  
  report <- data.table(Name = report$groupCat, report)
  
  report <- merge(data.table(groupCat = 1:4), report, by="groupCat", all.x=T)
  
  report[is.na(perc_miles), perc_miles := as.character(0)]
  
  return(report)
  
}
