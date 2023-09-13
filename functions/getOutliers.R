###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont 
#
#
# Description:
#
# This function does the outlier analysis and returns a table of results for
# the outlier analysis on the detailed review pages.
#
###########################################################################

getOutliers <- function(data, year, variable){
  
  outlier_threshold_high <- gVariables[Name == variable, Outlier_Max]
  outlier_threshold_low  <- gVariables[Name == variable, Outlier_Min]
  
  if ( is.na(outlier_threshold_high) & is.na(outlier_threshold_low) ) return(NULL)
  
  data <- data[dataitem==variable & datayear==year,]
  
  d.l <- data    
  
  if (nrow(d.l) == 0){
    
    report.1 <- data.table(groupCat = c(3, 4), N = NA, totalmiles = NA,
                           miles = NA, perc_miles = NA)
    
  } else {
    
    total.1 <- d.l[ ,
                    list(totalmiles = round(sum(endpoint-beginpoint), 2)),
                    by=list(F_SYSTEM)]
    
    result.1 <- d.l[(valuenumeric > outlier_threshold_high | 
                       valuenumeric < outlier_threshold_low),
                    list(miles = round(sum(endpoint - beginpoint, na.rm=TRUE), 2), N=sum(num_sections)),
                    by=list(F_SYSTEM)
                    ]
    
    report.1 <- merge(total.1, result.1, by="F_SYSTEM", all.x=TRUE, all.y=FALSE)
    report.1[is.na(miles), miles := 0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    report.1[is.na(N), N := 0]
    
    report.1[, perc_miles := as.character(round(miles / totalmiles, 2) * 100)]
    
    # report.1[, totalmiles:=NULL]
    
    report.1 <- report.1[!is.na(F_SYSTEM),]
    
    report.1[, groupCat := F_SYSTEM + 2]
    report.1[, F_SYSTEM := NULL]
  }
  
  # Interstate --------------------------
  d.l <- data[Interstate == 1,,]    
  
  if ( nrow(d.l) == 0 ){
    report.2 <- data.table(totalmiles = NA, miles = NA, N = NA, perc_miles = NA)  
  } else {
    
    result.2 <- d.l[(valuenumeric > outlier_threshold_high |
                       valuenumeric < outlier_threshold_low),
                    list(miles=round(sum(endpoint-beginpoint,na.rm=TRUE),2),N=sum(num_sections)),
                    ]
    
    total.2 <- d.l[, list(totalmiles = round(sum(endpoint - beginpoint), 2)), ]
    
    if(nrow(result.2) == 0){
      result.2 <- data.table(miles=0, N=0)
    }
    
    report.2 <- data.table(result.2, total.2)
    #report.2[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    
    report.2[, perc_miles:=as.character(round(miles/totalmiles,2)*100)]
    
    # report.2[, totalmiles:=NULL]
  }
  
  report.2[, groupCat:=1]
  
  # NHS ------------------------------------
  
  d.l <- data[NHS == 1,,]    
  
  if ( nrow(d.l) == 0 ){
    report.3 <- data.table(totalmiles = NA, miles = NA, N = NA, perc_miles = NA)    
  } else {
    
    result.3 <- d.l[(valuenumeric > outlier_threshold_high |
                       valuenumeric < outlier_threshold_low),
                    list(miles=round(sum(endpoint-beginpoint,na.rm=TRUE),2),N=sum(num_sections)),]
    
    if(nrow(result.3)==0){
      result.3 <- data.table(miles=c(0), N=c(0))
    }
    
    total.3 <- d.l[ ,list(totalmiles=round(sum(endpoint-beginpoint),2)),]
    
    report.3 <- data.table(result.3,total.3)
    #report.3[is.na(miles),miles:=0] # setting values to 0 where there are no merges. this mean that the state had no lane miles outside the thresholds set
    
    report.3[, perc_miles:=as.character(round(miles/totalmiles,2)*100)]
    
    # report.3[, totalmiles:=NULL]
  }
  
  report.3[, groupCat:=2]
  
  report <- rbind(report.2, report.3, report.1)
  
  report <- data.table(Name=report$groupCat,report)
  
  report <- merge(data.table(groupCat=1:4),report,by="groupCat", all.x=T)
  
  #report[is.na(perc_miles), perc_miles := 0]
  report[is.nan(perc_miles), perc_miles := NA]
  
  return(report)
}
