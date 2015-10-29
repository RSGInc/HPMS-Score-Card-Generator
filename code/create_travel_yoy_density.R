###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# This function creates a faceted year over year density chart for the
# summary tables.
#
###########################################################################

create_travel_yoy_density <- function(
     data,
     state,
     year,
     yearcomparison,
     variable,
     includeNational,
     ramps
)
{
     
     if(ramps)
     {   
        var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE==4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS)]
        var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE==4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS)]
     } else {   
        var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE!=4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS)]
        var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE!=4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS)]
     }
  
     if(nrow(var.1)>2|nrow(var.2)>2) # we have something to report (density plots require at least 3 points to draw)
     {
          minvalue1 <- var.1[,min(value_numeric)]
          minvalue2 <- var.2[,min(value_numeric)]
          
          maxvalue1 <- var.1[,max(value_numeric)]
          maxvalue2 <- var.2[,max(value_numeric)]
          
          minvalue <- min(minvalue1,minvalue2)
          maxvalue <- max(maxvalue1,maxvalue2)
          
          if(nrow(var.2)>0&includeNational=="Y")
          {
               national1 <- var.2[sample(1:nrow(var.2),size=(nrow(var.2)*0.25)),]
               national2 <- var.2[sample(1:nrow(var.2),size=(nrow(var.2)*0.25)),]
               national3 <- var.2[sample(1:nrow(var.2),size=(nrow(var.2)*0.25)),]
               national4 <- var.2[sample(1:nrow(var.2),size=(nrow(var.2)*0.25)),]
          } else {
               national1 <- var.1[sample(1:nrow(var.1),size=(nrow(var.1)*0.25)),]
               national2 <- var.1[sample(1:nrow(var.1),size=(nrow(var.1)*0.25)),]
               national3 <- var.1[sample(1:nrow(var.1),size=(nrow(var.1)*0.25)),]
               national4 <- var.1[sample(1:nrow(var.1),size=(nrow(var.1)*0.25)),]
          }

          if(includeNational!="Y")
          {
               national1 <- NULL  
               national2 <- NULL  
               national3 <- NULL  
               national4 <- NULL  
          }
          
          if(gVariablesLabels[Name==variable,NumLevels]==0)
          {
            p1 <- densityPlot(var.1[Interstate==1],var.2[Interstate==1],d3=national1,title=gF_SYSTEM_levels[1],minvalue,maxvalue)
            p2 <- densityPlot(var.1[NHS==1]       ,var.2[NHS==1]       ,d3=national2,title=gF_SYSTEM_levels[2],minvalue,maxvalue)
            p3 <- densityPlot(var.1[F_SYSTEM==1]  ,var.2[F_SYSTEM==1]  ,d3=national3,title=gF_SYSTEM_levels[3],minvalue,maxvalue)
            p4 <- densityPlot(var.1[F_SYSTEM==2]  ,var.2[F_SYSTEM==2]  ,d3=national4,title=gF_SYSTEM_levels[4],minvalue,maxvalue)
            
            obj <- arrangeGrob(p1,p2,p3,p4,ncol=2,nrow=2)
          }
          else {
            labels <- c()
            l <- unlist(gVariablesLabels[Name==variable,list(Code1,Code2,Code3,Code4,Code5,Code6,Code7,Code8,Code9,Code10,Code11)])
            for(i in 1:11)
            {
              if(l[i]!="")
              {
                labels <- c(labels,l[i])
              }
            }
            
            p1 <- barPlot(var.1[Interstate==1],var.2[Interstate==1],d3=national1,labels,title=gF_SYSTEM_levels[1])
            p2 <- barPlot(var.1[NHS==1]       ,var.2[NHS==1]       ,d3=national2,labels,title=gF_SYSTEM_levels[2])
            p3 <- barPlot(var.1[F_SYSTEM==1]  ,var.2[F_SYSTEM==1]  ,d3=national3,labels,title=gF_SYSTEM_levels[3])
            p4 <- barPlot(var.1[F_SYSTEM==2]  ,var.2[F_SYSTEM==2]  ,d3=national4,labels,title=gF_SYSTEM_levels[4])
            
            obj <- arrangeGrob(p1,p2,p3,p4,ncol=2,nrow=2)
              
            obj <- textGrob("New chart type",gp=gpar(fontsize=12, col="Red"))  
          }
          return(obj)
     } else
     {
          # nothing to report because data are missing
          return(textGrob(NoDataString,gp=gpar(fontsize=12, col="Red")))
     }
     
}

