###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Create travel data outlier graphical object. Future AADT values greater
# than or less than three times current values are considered outliers.
#
# Create travel data outlier
# future aadt values > 3 * current or less than current
# this provides a single graphical object
#
###########################################################################

create_travel_data_outlier <- function(
                                       data,   
                                       state,
                                       year,
                                       yearcomparison
                                       )
{
  # AADT = Annual Average Daily Traffic
  # AADT_COMBINATION = Combination Truck
  # AADT_SINGLE_UNIT = Single Unit Trucks/Buses
  # FUTURE_AADT
  
  aadt    <- data[state_code==state&year_record==year&data_item=="AADT",
                  list(route_id, begin_point, end_point, value_numeric, F_SYSTEM, NHS, Interstate)]
  faadt   <- data[state_code==state&year_record==year&data_item=="FUTURE_AADT",
                  list(route_id, begin_point, end_point, value_numeric, F_SYSTEM, NHS, Interstate)]
  
  comparison <- merge(aadt, faadt,
                      by=c("route_id", "begin_point", "end_point", "F_SYSTEM", "NHS", "Interstate"), all.y=TRUE, all.x=FALSE)
  setnames(comparison, "value_numeric.x", "AADT")
  setnames(comparison, "value_numeric.y", "FUTURE_AADT")
  
  result.1 <- comparison[Interstate == 1 & ((FUTURE_AADT > 3 * AADT) | (FUTURE_AADT < AADT)),
                         list(miles=sum(end_point - begin_point))]
  result.2 <- comparison[NHS == 1 & ((FUTURE_AADT > 3 * AADT) | (FUTURE_AADT < AADT)),
                         list(miles=sum(end_point - begin_point))]
  result.3 <- comparison[F_SYSTEM == 1 & ((FUTURE_AADT > 3 * AADT) | (FUTURE_AADT < AADT)),
                         list(miles=sum(end_point - begin_point))]
  result.4 <- comparison[F_SYSTEM == 2 & ((FUTURE_AADT > 3 * AADT) | (FUTURE_AADT < AADT)),
                         list(miles=sum(end_point - begin_point))]
  
  total.1 <- aadt[Interstate == 1,
                  list(totalmiles=round(sum(end_point-begin_point), 2)), ]
  total.2 <- aadt[NHS == 1,
                  list(totalmiles=round(sum(end_point-begin_point), 2)), ]
  total.3 <- aadt[F_SYSTEM == 1,
                  list(totalmiles=round(sum(end_point-begin_point), 2)), ]
  total.4 <- aadt[F_SYSTEM == 2,
                  list(totalmiles=round(sum(end_point-begin_point), 2)), ]
  
  report <- data.table(groupCat=1:4,
                       miles=as.numeric(c(result.1, result.2, result.3, result.4)),
                       totalmiles=as.numeric(c(total.1, total.2, total.3, total.4)))

  report[, perc_miles:=ifelse(is.na(miles), 0, as.character(round(miles/totalmiles, 2)*100))]
  
  report <- report[, totalmiles:=NULL]
  browser()
  if(nrow(report) > 0)
  {
    #report <- merge(data.table(groupCat=1:4), report, by="_SYSTEM", all.x=T)
    report[, groupCat:=gF_SYSTEM_levels[as.numeric(report[, groupCat])]]
    report[, miles:=string_format(miles)]
    report[, perc_miles:=paste0(perc_miles, "%")]
    setnames(report, "groupCat", "Functional\nSystem")
    setnames(report, "miles", "Total Centerline\nMiles")
    setnames(report, "perc_miles", "% of \nMiles")

    thm <- ttheme_default(
      core = list(fg_params = list(col='black', fontsize=6.5, hjust=1, x=0.95),
                  bg_params=list(fill='grey95'),
                  padding=unit(c(0.1, 0.1), 'inches')),
      colhead = list(fg_params = list(col='black', fontsize=7, fontface='bold', hjust=1, x=0.95),
                     bg_params = list(fill='grey90'),
                     padding=unit(c(0.1, 0.1), 'inches'))
    )
    
    ob <- tableGrob(report, rows=NULL, theme=thm)
    
    ob <- vertically_align(ob)
    
    return(ob)
  } else
  {
    return(textGrob(NoDataString, gp=gpar(fontsize=8, col="Red")))
  }
  
}
