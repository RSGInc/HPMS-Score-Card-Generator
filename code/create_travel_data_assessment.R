###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This function provides the comparison of AADT, FAAT, AADT_CU, and AADT_SU
# and produces an overall row of graphical objects for the travel data
# assessment analysis.
#
###########################################################################

create_travel_data_assessment <- function(
     data,
     state,
     year,
     yearcomparison
){
  
  # A function to get weighted quantiles
  sf <- function(x, diff=0, weight=rep(1,length(x))){
    x <- (x - diff)
    
    if ( all(is.na(x))){
      return(list(min = NA, lq = NA, mq = NA, uq = NA, max = NA))
    }
    
    return(list(
      min=min(x, na.rm=TRUE),
      lq=weighted.quantile(x, w=weight, c(0.25), na.rm=TRUE),
      mq=weighted.quantile(x, w=weight, c(0.5), na.rm=TRUE),
      uq=weighted.quantile(x, w=weight, c(0.75), na.rm=TRUE),
      max=max(x, na.rm=TRUE)
    )
    )
  }
  
     # AADT = Annual Average Daily Traffic
     # AADT_COMBINATION = Combination Truck
     # AADT_SINGLE_UNIT = Single Unit Trucks/Buses
     # FUTURE_AADT
  
     aadt    <- data[state_code == state & year_record == year &
                       data_item == "AADT" & FACILITY_TYPE!=4,
                     list(route_id, begin_point, end_point, value_numeric, 
                          F_SYSTEM, NHS, Interstate, num_sections)]
     
     aadt_cu <- data[state_code == state & year_record == year &
                       data_item == "AADT_COMBINATION" & FACILITY_TYPE != 4,
                     list(route_id, begin_point, end_point, value_numeric,
                          F_SYSTEM, NHS, Interstate, num_sections)]
     
     aadt_su <- data[state_code == state & year_record == year &
                       data_item == "AADT_SINGLE_UNIT" & FACILITY_TYPE != 4,
                     list(route_id, begin_point, end_point, value_numeric,
                          F_SYSTEM, NHS, Interstate, num_sections)]
     
     faadt   <- data[state_code == state & year_record == year &
                       data_item == "FUTURE_AADT" & FACILITY_TYPE != 4,
                     list(route_id, begin_point, end_point, value_numeric,
                          F_SYSTEM, NHS, Interstate, num_sections)]
     
     aadt    <- unique(aadt)
     aadt_cu <- unique(aadt_cu)
     aadt_su <- unique(aadt_su)
     faadt   <- unique(faadt)
     
    
     comparison <- merge(aadt, faadt,
                         by=c("route_id", "begin_point", "end_point", "F_SYSTEM", "Interstate", "NHS"),
                         all.y=TRUE, all.x=FALSE)
     setnames(comparison,"value_numeric.x","AADT")
     setnames(comparison,"value_numeric.y","FUTURE_AADT")
     
     comparison[, miles := sum(end_point-begin_point), by=.(F_SYSTEM)]
     
     
     faadt.compare.1      <- comparison[Interstate == 1, 
                                        sf(FUTURE_AADT/AADT, weight=num_sections.x),]
     faadt.compare.2      <- comparison[NHS==1, 
                                        sf(FUTURE_AADT/AADT, weight=num_sections.x),]
     faadt.compare.3      <- comparison[F_SYSTEM==1,
                                        sf(FUTURE_AADT/AADT, weight=num_sections.x),]
     faadt.compare.4      <- comparison[F_SYSTEM==2,
                                        sf(FUTURE_AADT/AADT, weight=num_sections.x),]
     
     faadt.compare.1[,groupCat:=1]
     faadt.compare.2[,groupCat:=2]
     faadt.compare.3[,groupCat:=3]
     faadt.compare.4[,groupCat:=4]
     
     faadt.compare <- data.table(rbind(faadt.compare.1,faadt.compare.2,faadt.compare.3,faadt.compare.4))
     
     faadt.compare[,groupCat:=factor(groupCat,levels=4:1,labels=gF_SYSTEM_levels[4:1])]
     
     
     comparison <- merge(aadt, aadt_cu,
                         by=c("route_id", "begin_point", "end_point", "F_SYSTEM", "Interstate", "NHS"),
                         all.y=TRUE, all.x=FALSE)     
     setnames(comparison,"value_numeric.x","AADT")
     setnames(comparison,"value_numeric.y","AADT_COMBINATION")
     
     aadt_combo.compare.1      <- comparison[Interstate==1,sf(AADT_COMBINATION/AADT,weight=num_sections.x),]
     aadt_combo.compare.2      <- comparison[NHS==1,sf(AADT_COMBINATION/AADT,weight=num_sections.x),]
     aadt_combo.compare.3      <- comparison[F_SYSTEM==1,sf(AADT_COMBINATION/AADT,weight=num_sections.x),]
     aadt_combo.compare.4      <- comparison[F_SYSTEM==2,sf(AADT_COMBINATION/AADT,weight=num_sections.x),]
     
     aadt_combo.compare.1[,groupCat:=1]
     aadt_combo.compare.2[,groupCat:=2]
     aadt_combo.compare.3[,groupCat:=3]
     aadt_combo.compare.4[,groupCat:=4]
     
     aadt_combo.compare <- data.table(rbind(aadt_combo.compare.1,aadt_combo.compare.2,aadt_combo.compare.3,aadt_combo.compare.4))
     
     aadt_combo.compare[,groupCat:=factor(groupCat,levels=4:1,labels=gF_SYSTEM_levels[4:1])]
     
     
     comparison <- merge(aadt, aadt_su, 
                         by=c("route_id", "begin_point", "end_point", "F_SYSTEM", "Interstate", "NHS"),
                         all.y=TRUE, all.x=FALSE)
     setnames(comparison,"value_numeric.x","AADT")
     setnames(comparison,"value_numeric.y","AADT_SINGLE_UNIT")
     
     aadt_su.compare.1      <- comparison[Interstate==1,sf(AADT_SINGLE_UNIT/AADT,weight=num_sections.x),]
     aadt_su.compare.2      <- comparison[NHS==1,sf(AADT_SINGLE_UNIT/AADT,weight=num_sections.x),]
     aadt_su.compare.3      <- comparison[F_SYSTEM==1,sf(AADT_SINGLE_UNIT/AADT,weight=num_sections.x),]
     aadt_su.compare.4      <- comparison[F_SYSTEM==2,sf(AADT_SINGLE_UNIT/AADT,weight=num_sections.x),]
     
     aadt_su.compare.1[,groupCat:=1]
     aadt_su.compare.2[,groupCat:=2]
     aadt_su.compare.3[,groupCat:=3]
     aadt_su.compare.4[,groupCat:=4]
     
     aadt_su.compare <- data.table(rbind(aadt_su.compare.1,aadt_su.compare.2,aadt_su.compare.3,aadt_su.compare.4))
     
     aadt_su.compare[,groupCat:=factor(groupCat,levels=4:1,labels=gF_SYSTEM_levels[4:1])]
     
     if(nrow(faadt.compare[!is.na(mq),])> 0){
          p1 <- crossbarPlot(faadt.compare)
     } else {
          p1 <- textGrob(NoDataString, gp = gpar(fontsize = 8, col = "red"))
     }
     
     if(nrow(aadt_combo.compare[!is.na(mq),])> 0){
          p2 <- crossbarPlot(aadt_combo.compare)
     } else {
          p2 <- textGrob(NoDataString, gp = gpar(fontsize = 8, col = "red"))
     }

     if(nrow(aadt_su.compare[!is.na(mq),])> 0) {
          p3 <- crossbarPlot(aadt_su.compare)
     } else {
          p3 <- textGrob(NoDataString, gp = gpar(fontsize = 8, col = "red"))
     }

     ob <- arrangeGrob(
          p1,
          p2,
          p3,
          nrow=1,widths=unit(rep(1/3,3),units="npc")
     )
     
     return(ob)
}