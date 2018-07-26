###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
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
){
  
  #if ( variable == 'AADT_COMBINATION' ) browser()
  
  type <- gVariables[Name==variable,Type]
  
  if(type==1){

    if(ramps){   
      var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE==4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS,num_sections)]
      var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE==4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS,num_sections)]
    } else {   
      var.1    <- data[state_code==state&year_record==year          &data_item==variable&FACILITY_TYPE!=4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS,num_sections)]
      var.2    <- data[state_code==state&year_record==yearcomparison&data_item==variable&FACILITY_TYPE!=4&!is.na(value_numeric),list(route_id,begin_point,end_point,value_numeric,F_SYSTEM,Interstate,NHS,num_sections)]
    }
  }
  
  if(type==2){
    
    if(ramps){   
      var.1    <- data[state_code==state & year_record==year           & data_item==variable & FACILITY_TYPE==4 & !is.na(value_date), list(route_id, begin_point, end_point, value_numeric=year(value_date), F_SYSTEM, Interstate, NHS,num_sections)]
      var.2    <- data[state_code==state & year_record==yearcomparison & data_item==variable & FACILITY_TYPE==4 & !is.na(value_date), list(route_id, begin_point, end_point, value_numeric=year(value_date), F_SYSTEM, Interstate, NHS,num_sections)]
    } else {   
      var.1    <- data[state_code==state & year_record==year           & data_item==variable & FACILITY_TYPE !=4 & !is.na(value_date), list(route_id, begin_point, end_point, value_numeric=year(value_date), F_SYSTEM, Interstate, NHS,num_sections)]
      var.2    <- data[state_code==state & year_record==yearcomparison & data_item==variable & FACILITY_TYPE !=4 & !is.na(value_date), list(route_id, begin_point, end_point, value_numeric=year(value_date), F_SYSTEM, Interstate, NHS,num_sections)]
    }
  }
  
  # we have something to report (density plots require at least 3 points to draw)
  if(nrow(var.1)>2|nrow(var.2)>2) {
    
    national  <- readRDS(paste0("data\\+National\\",yearcomparison,"\\",variable,".rds"))
    
    if(ramps){
      national <- national[FACILITY_TYPE==4,]
    } else {
      national <- national[FACILITY_TYPE!=4,]
    }
    
    if(gVariablesLabels[Name==variable, NumLevels]==0){ 
      # make density plots
      
      if(is.null(national)){
        # Interstate
        p1 <- densityPlot(d1=var.1[Interstate==1],
                          d2=var.2[Interstate==1],
                          d3=national[Interstate==1],
                          title=gF_SYSTEM_levels[1],
                          year1=year,
                          year2=yearcomparison)

        # National Highway System
        p2 <- densityPlot(d1=var.1[NHS==1],
                          d2=var.2[NHS==1],
                          d3=national[NHS==1],
                          title=gF_SYSTEM_levels[2],
                          year1=year,
                          year2=yearcomparison)
        
        # Other / Minor Arterials
        p3 <- densityPlot(d1=var.1[F_SYSTEM==1],
                          d2=var.2[F_SYSTEM==1],
                          d3=national[F_SYSTEM==1],
                          title=gF_SYSTEM_levels[3],
                          year1=year,
                          year2=yearcomparison)
        
        # Collectors and Locals
        p4 <- densityPlot(d1=var.1[F_SYSTEM==2],
                          d2=var.2[F_SYSTEM==2],
                          d3=national[F_SYSTEM==2],
                          title=gF_SYSTEM_levels[4],
                          year1=year,
                          year2=yearcomparison)
      } else {
        p1 <- densityPlot(d1=var.1[Interstate==1],
                          d2=var.2[Interstate==1],
                          d3=national[Interstate==1],
                          title=gF_SYSTEM_levels[1],
                          year1=year,
                          year2=yearcomparison,
                          showLabel=TRUE)
        
        p2 <- densityPlot(d1=var.1[NHS==1],
                          d2=var.2[NHS==1],
                          d3=national[NHS==1],
                          title=gF_SYSTEM_levels[2],
                          year1=year,year2=yearcomparison)
        
        p3 <- densityPlot(d1=var.1[F_SYSTEM==1],
                          d2=var.2[F_SYSTEM==1],
                          d3=national[F_SYSTEM==1],
                          title=gF_SYSTEM_levels[3],
                          year1=year,
                          year2=yearcomparison,
                          showLabel=TRUE)
        
        p4 <- densityPlot(d1=var.1[F_SYSTEM==2,],
                          d2=var.2[F_SYSTEM==2,],
                          d3=national[F_SYSTEM==2,],
                          title=gF_SYSTEM_levels[4],
                          year1=year,
                          year2=yearcomparison)
      }
      
      spacer_height <- 0.07
      fill_rect <- rectGrob(gp = gpar(fill='white', col='white'))
      obj <- arrangeGrob(
        p1, p2, 
        fill_rect, fill_rect,
        p3, p4,
        ncol=2, nrow=3, 
        widths=unit(rep(3.77776666666667/2,2), units="inches"),
        heights=unit(c((1 - spacer_height)/2, spacer_height, (1-spacer_height)/2), units='npc'))
    } else {
      
      labels <- 1:7
      
      scale <- max(c(var.1[,sum(end_point-begin_point),by=list(F_SYSTEM,Interstate,NHS,value_numeric)][,max(V1)],
                     var.2[,sum(end_point-begin_point),by=list(F_SYSTEM,Interstate,NHS,value_numeric)][,max(V1)]
      ))
      
      natWeight <- scale/national[,sum(end_point-begin_point),by=list(F_SYSTEM,Interstate,NHS,value_numeric)][,max(V1)]
      
      national[,end_point  :=natWeight*end_point]
      national[,begin_point:=natWeight*begin_point]
      
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
        p32 <- barPlot(national[NHS==1]       ,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=TRUE)
        p33 <- barPlot(national[F_SYSTEM==1]  ,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=TRUE)
        p34 <- barPlot(national[F_SYSTEM==2]  ,labels,title="",barcolor="black",topMargin=-0.5,scale=scale,               showAxis=TRUE)
      }               
      obj <- arrangeGrob(p11, p12, p13, p14, textGrob(""),
                         p21, p22, p23, p24, textGrob(""),
                         p31, p32, p33, p34, textGrob(""),
                         ncol=5, nrow=3, 
                         widths=unit(c(rep(3.5/4,4),0.2777667), units="inches") )
      
    }
    return(obj)
  } else
  {
    # nothing to report because data are missing
    return(textGrob(NoDataString,gp=gpar(fontsize=8, col="Red")))
  }
  
}

