###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
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
)
{
     # AADT = Annual Average Dailty Traffic
     # AADT_COMBINATION = Combination Truck
     # AADT_SINGLE_UNIT = Single Unit Trucks/Buses
     # FUTURE_AADT
     
     aadt    <- data[state_code==state&year_record==year&data_item=="AADT",list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,NHS,Interstate)]
     aadt_cu <- data[state_code==state&year_record==year&data_item=="AADT_COMBINATION",list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,NHS,Interstate)]
     aadt_su <- data[state_code==state&year_record==year&data_item=="AADT_SINGLE_UNIT",list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,NHS,Interstate)]
     faadt   <- data[state_code==state&year_record==year&data_item=="FUTURE_AADT",list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,NHS,Interstate)]
     
     comparison <- merge(aadt,aadt_cu,by=c("route_id","begin_point","end_point","F_SYSTEM","Interstate","NHS"),all.y=TRUE,all.x=FALSE)     
     setnames(comparison,"value_numeric.x","AADT")
     setnames(comparison,"value_numeric.y","AADT_COMBINATION")
     comparison <- merge(comparison,aadt_su,by=c("route_id","begin_point","end_point","F_SYSTEM","Interstate","NHS"),all.y=TRUE,all.x=FALSE)
     setnames(comparison,"value_numeric","AADT_SINGLE_UNIT")
     comparison <- merge(comparison,faadt,by=c("route_id","begin_point","end_point","F_SYSTEM","Interstate","NHS"),all.y=TRUE,all.x=FALSE)
     setnames(comparison,"value_numeric","FUTURE_AADT")
     
     comparison[,miles:=sum(end_point-begin_point),by=.(F_SYSTEM)]
     
     sf <- function(x,diff=0)
     {
          x <- (x - diff)
          return(list(
               min=min(x),
               lq=quantile(x,c(0.25)),
               mq=quantile(x,c(0.5)),
               uq=quantile(x,c(0.75)),
               max=max(x)
               )
          )
     }
     
     faadt.compare.1      <- comparison[Interstate==1,sf(FUTURE_AADT/AADT),]
     faadt.compare.2      <- comparison[NHS==1,sf(FUTURE_AADT/AADT),]
     faadt.compare.3      <- comparison[F_SYSTEM==1,sf(FUTURE_AADT/AADT),]
     faadt.compare.4      <- comparison[F_SYSTEM==2,sf(FUTURE_AADT/AADT),]
     
     faadt.compare.1[,groupCat:=1]
     faadt.compare.2[,groupCat:=2]
     faadt.compare.3[,groupCat:=3]
     faadt.compare.4[,groupCat:=4]
     
     faadt.compare <- data.table(rbind(faadt.compare.1,faadt.compare.2,faadt.compare.3,faadt.compare.4))
     
     faadt.compare[,groupCat:=factor(groupCat,levels=4:1,labels=gF_SYSTEM_levels[4:1])]
     
     
     
     aadt_combo.compare.1      <- comparison[Interstate==1,sf(AADT_COMBINATION/AADT),]
     aadt_combo.compare.2      <- comparison[NHS==1,sf(AADT_COMBINATION/AADT),]
     aadt_combo.compare.3      <- comparison[F_SYSTEM==1,sf(AADT_COMBINATION/AADT),]
     aadt_combo.compare.4      <- comparison[F_SYSTEM==2,sf(AADT_COMBINATION/AADT),]
     
     aadt_combo.compare.1[,groupCat:=1]
     aadt_combo.compare.2[,groupCat:=2]
     aadt_combo.compare.3[,groupCat:=3]
     aadt_combo.compare.4[,groupCat:=4]
     
     aadt_combo.compare <- data.table(rbind(aadt_combo.compare.1,aadt_combo.compare.2,aadt_combo.compare.3,aadt_combo.compare.4))
     
     aadt_combo.compare[,groupCat:=factor(groupCat,levels=4:1,labels=gF_SYSTEM_levels[4:1])]
     
     
     
     aadt_su.compare.1      <- comparison[Interstate==1,sf(AADT_SINGLE_UNIT/AADT),]
     aadt_su.compare.2      <- comparison[NHS==1,sf(AADT_SINGLE_UNIT/AADT),]
     aadt_su.compare.3      <- comparison[F_SYSTEM==1,sf(AADT_SINGLE_UNIT/AADT),]
     aadt_su.compare.4      <- comparison[F_SYSTEM==2,sf(AADT_SINGLE_UNIT/AADT),]
     
     aadt_su.compare.1[,groupCat:=1]
     aadt_su.compare.2[,groupCat:=2]
     aadt_su.compare.3[,groupCat:=3]
     aadt_su.compare.4[,groupCat:=4]
     
     aadt_su.compare <- data.table(rbind(aadt_su.compare.1,aadt_su.compare.2,aadt_su.compare.3,aadt_su.compare.4))
     
     aadt_su.compare[,groupCat:=factor(groupCat,levels=4:1,labels=gF_SYSTEM_levels[4:1])]
     
     if(nrow(faadt.compare)> 0)
     {
          p1 <- crossbarPlot(faadt.compare)
     } else
     {
          p1 <- textGrob("No Data", gp = gpar(fontsize = 16, col = "red"))
     }
     
     if(nrow(aadt_combo.compare)> 0)
     {
          p2 <- crossbarPlot(aadt_combo.compare)
     } else
     {
          p2 <- textGrob("No Data", gp = gpar(fontsize = 16, col = "red"))
     }

     if(nrow(aadt_su.compare)> 0)
     {
          p3 <- crossbarPlot(aadt_su.compare)
     } else
     {
          p3 <- textGrob("No Data", gp = gpar(fontsize = 16, col = "red"))
     }

     ob <- arrangeGrob(
          p1,
          p2,
          p3,
          nrow=1,widths=unit(rep(1/3,3),units="npc")
     )
     
     return(ob)
}