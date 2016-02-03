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
          national  <- readRDS(paste0("data\\+National\\",yearcomparison,"\\",variable,".rds"))
          
          if(nrow(var.1[!is.na(value_numeric),])>2)
          {
            d1 <- density(var.1[,value_numeric],weights=var.1[,(end_point-begin_point)/sum(end_point-begin_point)],na.rm = TRUE)
            minvalue1 <- min(d1$x)
            maxvalue1 <- max(d1$x)
            ymax1     <- max(d1$y)
          } else {
            minvalue1 <- NULL
            maxvalue1 <- NULL
            ymax1     <- NULL
          }
          if(nrow(var.2[!is.na(value_numeric),])>2)
          {
            d2 <- density(var.2[,value_numeric],weights=var.2[,(end_point-begin_point)/sum(end_point-begin_point)],na.rm = TRUE)
            minvalue2 <- min(d2$x)
            maxvalue2 <- max(d2$x)
            ymax2     <- max(d2$y)
          } else {
            minvalue2 <- NULL
            maxvalue2 <- NULL
            ymax2     <- NULL
          }
          if(nrow(national[!is.na(value_numeric),])>2)
          {
              d3 <- density(national[,value_numeric],weights=national[!is.na(value_numeric),(end_point-begin_point)/sum(end_point-begin_point)],na.rm = TRUE)
              minvalue3 <- min(d3$x)
              maxvalue3 <- max(d3$x)
              ymax3     <- max(d3$y)
          } else {
            minvalue3 <- NULL
            maxvalue3 <- NULL
            ymax3     <- NULL
          }          
          
          minvalue <- min(minvalue1,minvalue2,minvalue3)
          maxvalue <- max(maxvalue1,maxvalue2,maxvalue3)
          ymax     <- max(ymax1,ymax2,ymax3)*1.10
          
          #ymax <- max(var.1[,V1:=(end_point-begin_point)/sum(end_point-begin_point)][,sum(V1),by=.(value_numeric)][,V1],
          #    var.2[,V1:=(end_point-begin_point)/sum(end_point-begin_point)][,sum(V1),by=.(value_numeric)][,V1],
          #    national[,V1:=(end_point-begin_point)/sum(end_point-begin_point)][,sum(V1),by=.(value_numeric)][,V1]
          #)
          #if(includeNational=="Y")
          #{
               
          #} else 
          #{
          #     national <- NULL
          #}
          
          if(gVariablesLabels[Name==variable,NumLevels]==0)
          {
            if(is.null(national))
            {
              p1 <- densityPlot(var.1[Interstate==1],var.2[Interstate==1],d3=national,title=gF_SYSTEM_levels[1],minvalue,maxvalue,year,yearcomparison,ymax=ymax)
              p2 <- densityPlot(var.1[NHS==1]       ,var.2[NHS==1]       ,d3=national,title=gF_SYSTEM_levels[2],minvalue,maxvalue,year,yearcomparison,ymax=ymax)
              p3 <- densityPlot(var.1[F_SYSTEM==1]  ,var.2[F_SYSTEM==1]  ,d3=national,title=gF_SYSTEM_levels[3],minvalue,maxvalue,year,yearcomparison,ymax=ymax)
              p4 <- densityPlot(var.1[F_SYSTEM==2]  ,var.2[F_SYSTEM==2]  ,d3=national,title=gF_SYSTEM_levels[4],minvalue,maxvalue,year,yearcomparison,ymax=ymax)
            } else
            {
              p1 <- densityPlot(d1=var.1[Interstate==1],d2=var.2[Interstate==1],d3=national[Interstate==1],title=gF_SYSTEM_levels[1],minvalue=minvalue,maxvalue=maxvalue,year1=year,year2=yearcomparison,showLabel=TRUE,ymax=ymax)
              p2 <- densityPlot(var.1[NHS==1]       ,var.2[NHS==1]       ,d3=national[NHS==1]       ,title=gF_SYSTEM_levels[2],minvalue=minvalue,maxvalue=maxvalue,year1=year,year2=yearcomparison,ymax=ymax)
              p3 <- densityPlot(var.1[F_SYSTEM==1]  ,var.2[F_SYSTEM==1]  ,d3=national[F_SYSTEM==1]  ,title=gF_SYSTEM_levels[3],minvalue=minvalue,maxvalue=maxvalue,year1=year,year2=yearcomparison,showLabel=TRUE,ymax=ymax)
              p4 <- densityPlot(d1=var.1[F_SYSTEM==2,]  ,d2=var.2[F_SYSTEM==2,]  ,d3=national[F_SYSTEM==2,]  ,title=gF_SYSTEM_levels[4],minvalue=minvalue,maxvalue=maxvalue,year1=year,year2=yearcomparison,ymax=ymax)
            }
            obj <- arrangeGrob(p1,p2,p3,p4,ncol=2,nrow=2)
          }
          else {
            
            labels <- 1:7
            
            scale <- max(c(var.1[,sum(end_point-begin_point),by=list(F_SYSTEM,Interstate,NHS,value_numeric)][,max(V1)],
                       var.2[,sum(end_point-begin_point),by=list(F_SYSTEM,Interstate,NHS,value_numeric)][,max(V1)]))
            
            
            p11 <- barPlot(var.1[Interstate==1],labels,title=gF_SYSTEM_levels[1],barcolor="slategray",bottomMargin=-0.5,scale=scale,showLabel=TRUE)
            p12 <- barPlot(var.1[NHS==1]       ,labels,title=gF_SYSTEM_levels[2],barcolor="slategray",bottomMargin=-0.5,scale=scale)
            p13 <- barPlot(var.1[F_SYSTEM==1]  ,labels,title=gF_SYSTEM_levels[3],barcolor="slategray",bottomMargin=-0.5,scale=scale)
            p14 <- barPlot(var.1[F_SYSTEM==2]  ,labels,title=gF_SYSTEM_levels[4],barcolor="slategray",bottomMargin=-0.5,scale=scale)

            p21 <- barPlot(var.2[Interstate==1],labels,title="",barcolor="gray",scale=scale,showLabel=TRUE)
            p22 <- barPlot(var.2[NHS==1]       ,labels,title="",barcolor="gray",scale=scale)
            p23 <- barPlot(var.2[F_SYSTEM==1]  ,labels,title="",barcolor="gray",scale=scale)
            p24 <- barPlot(var.2[F_SYSTEM==2]  ,labels,title="",barcolor="gray",scale=scale)

            if(is.null(national))
            {
              p31 <- barPlot(national,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,showLabel=TRUE,showAxis=TRUE)
              p32 <- barPlot(national,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=FALSE)
              p33 <- barPlot(national,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=FALSE)
              p34 <- barPlot(national,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=FALSE)
            } else           
            {
              p31 <- barPlot(national[Interstate==1],labels,title="",barcolor="black",topMargin=-0.5,scale=scale,showLabel=TRUE,showAxis=TRUE)
              p32 <- barPlot(national[NHS==1]       ,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=FALSE)
              p33 <- barPlot(national[F_SYSTEM==1]  ,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=FALSE)
              p34 <- barPlot(national[F_SYSTEM==2]  ,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=FALSE)
            }               
            obj <- arrangeGrob(p11,p12,p13,p14,p21,p22,p23,p24,p31,p32,p33,p34,ncol=4,nrow=3)
              
            #obj <- textGrob("New chart type",gp=gpar(fontsize=12, col="Red"))  
          }
          return(obj)
     } else
     {
          # nothing to report because data are missing
          return(textGrob(NoDataString,gp=gpar(fontsize=12, col="Red")))
     }
     
}

