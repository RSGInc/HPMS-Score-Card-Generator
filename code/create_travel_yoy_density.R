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
  
  if ( variable %in% c('SURFACE_TYPE', 'WIDENING_OBSTACLE', 'YEAR_LAST_IMPROV') ) browser()
  
  type <- gVariables[Name==variable,Type]
  
  # Which columns to keep from data?
  keep_cols = c('route_id', 'begin_point', 'end_point', 'value_numeric',
                'F_SYSTEM', 'Interstate', 'NHS', 'num_sections')
  
  # get common indices to increase readability
  idx_var1 <- data[, state_code == state &
                     year_record == year &
                     data_item == variable ] 
  
  idx_var2 <- data[, state_code == state &
                     year_record == yearcomparison &
                     data_item == variable ]
  
  if ( ramps ){
    
    idx_var1 = idx_var1 & data[, FACILITY_TYPE == 4]
    idx_var2 = idx_var2 & data[, FACILITY_TYPE == 4]
    
  } else {
    
    idx_var1 = idx_var1 & data[, FACILITY_TYPE != 4]
    idx_var2 = idx_var2 & data[, FACILITY_TYPE != 4]
    
  }
  
  if(type == 1){ # Numeric
    
    idx_var1 = idx_var1 & data[, !is.na(value_numeric)]
    idx_var2 = idx_var2 & data[, !is.na(value_numeric)]
    
    var1 <- data[idx_var1, keep_cols, with=FALSE]
    var2 <- data[idx_var2, keep_cols, with=FALSE]
    
  }
  
  if ( type == 2 ){ # DATE
    
    idx_var1 = idx_var1 & data[, !is.na(value_date)]
    idx_var2 = idx_var2 & data[, !is.na(value_date)]
    
    var1 <- data[idx_var1]
    var2 <- data[idx_var2]
    
    var1[, value_numeric := year(value_date)]
    var2[, value_numeric := year(value_date)]
    
    var1 <- var1[, keep_cols, with=FALSE]
    var2 <- var2[, keep_cols, with=FALSE]
  }
  
  
  # we have something to report (density plots require at least 3 points to draw)
  if( nrow(var1) > 2 | nrow(var2) > 2 ) {
    
    national  <- readRDS(paste0("data\\+National\\", yearcomparison, "\\",
                                variable, ".rds"))
    if ( type == 2 ){
      national[, value_numeric := year(value_date)]
    }
    
    if(ramps){
      national <- national[FACILITY_TYPE == 4,]
    } else {
      national <- national[FACILITY_TYPE != 4,]
    }
    
    national <- national[!is.na(value_numeric)]
    national <- national[, keep_cols, with=FALSE]
    
    if(gVariablesLabels[Name==variable, NumLevels]==0){ 
      # make density plots
      
      # Interstate
      p1 <- densityPlot(d1=var1[Interstate==1],
                        d2=var2[Interstate==1],
                        d3=national[Interstate==1],
                        title=gF_SYSTEM_levels[1],
                        year1=year,
                        year2=yearcomparison,
                        showLabel = !is.null(national))
      
      # National Highway System
      p2 <- densityPlot(d1=var1[NHS==1],
                        d2=var2[NHS==1],
                        d3=national[NHS==1],
                        title=gF_SYSTEM_levels[2],
                        year1=year,
                        year2=yearcomparison)
      
      # Other / Minor Arterials
      p3 <- densityPlot(d1=var1[F_SYSTEM==1],
                        d2=var2[F_SYSTEM==1],
                        d3=national[F_SYSTEM==1],
                        title=gF_SYSTEM_levels[3],
                        year1=year,
                        year2=yearcomparison,
                        showLabel = !is.null(national))
      
      # Collectors and Locals
      p4 <- densityPlot(d1=var1[F_SYSTEM==2],
                        d2=var2[F_SYSTEM==2],
                        d3=national[F_SYSTEM==2],
                        title=gF_SYSTEM_levels[4],
                        year1=year,
                        year2=yearcomparison)
      
      spacer_height <- 0.07
      fill_rect <- rectGrob(gp = gpar(fill='white', col='white'))
      
      
      obj <- arrangeGrob(
        p1, p2, 
        fill_rect, fill_rect,
        p3, p4,
        ncol=2, nrow=3, 
        widths=unit(rep(3.77776666666667/2,2), units="inches"),
        heights=unit(c((1 - spacer_height)/2, spacer_height, (1-spacer_height)/2), units='npc'))
      
    } else {  # Make bar plots
      
      col_year1 = 'slategray'
      col_year2 = 'gray75'
      col_national = 'black'
      col_noplot = 'white'
      
      # get labels for bars
      
      # gVariablesLabels[Name==variable, NumLevels]
      # labels <- 1:7
      
      labels <- sort(
        unique(
          c(var1[, value_numeric],
            var2[, value_numeric],
            national[, value_numeric])
        )
      )
      
      scale <- max(c(var1[, sum(end_point - begin_point),
                          by=list(F_SYSTEM, NHS, value_numeric)][, max(V1)],
                     var1[NHS == 1, sum(end_point - begin_point),
                          by = list(value_numeric)][, max(V1)],
                     var2[, sum(end_point - begin_point),
                          by=list(F_SYSTEM, NHS, value_numeric)][, max(V1)],
                     var2[NHS == 1, sum(end_point - begin_point),
                          by = list(value_numeric)][, max(V1)]
      ))
      
      natWeight <- scale / national[
        , sum(end_point-begin_point),
        by=list(F_SYSTEM,Interstate,NHS,value_numeric)][,max(V1)]
      
      national[, end_point := natWeight * end_point]
      national[, begin_point := natWeight * begin_point]
      
      # Pad scale slightly so we don't have problems with the bar plots
      scale <- scale * 1.001
      
      p11 <- barPlot(var1[Interstate==1],
                     labels,
                     title=gF_SYSTEM_levels[1],
                     barcolor=col_year1,
                     bottomMargin=-0.5,
                     scale=scale,
                     showLabel=TRUE)
      
      p12 <- barPlot(var1[NHS==1]       ,
                     labels,
                     title=gF_SYSTEM_levels[2],
                     barcolor=col_year1,
                     bottomMargin=-0.5,
                     scale=scale)
      
      p13 <- barPlot(var1[F_SYSTEM==1]  ,
                     labels,
                     title=gF_SYSTEM_levels[3],
                     barcolor=col_year1,
                     bottomMargin=-0.5,
                     scale=scale)
      
      p14 <- barPlot(var1[F_SYSTEM==2]  ,
                     labels,
                     title=gF_SYSTEM_levels[4],
                     barcolor=col_year1,
                     bottomMargin=-0.5,
                     scale=scale)
      
      p21 <- barPlot(var2[Interstate==1],
                     labels,
                     title="",
                     barcolor=col_year2,
                     scale=scale,
                     showLabel=TRUE)
      
      p22 <- barPlot(var2[NHS==1]       ,
                     labels,
                     title="",
                     barcolor=col_year2,
                     scale=scale)
      
      p23 <- barPlot(var2[F_SYSTEM==1]  ,
                     labels,
                     title="",
                     barcolor=col_year2,
                     scale=scale)
      
      p24 <- barPlot(var2[F_SYSTEM==2]  ,
                     labels,
                     title="",
                     barcolor=col_year2,
                     scale=scale)
      
      p31 <- barPlot(ifelse(is.null(national), national, national[Interstate==1]),
                     labels,
                     title="",
                     barcolor=col_national,
                     topMargin=-0.5,
                     scale=scale,
                     showLabel=TRUE,
                     showAxis=TRUE)
      
      p32 <- barPlot(ifelse(is.null(national), national, national[NHS == 1]),
                     labels,
                     title="",
                     barcolor=col_national,
                     topMargin=-0.5,
                     scale=scale,
                     showAxis=!is.null(national))
      
      p33 <- barPlot(ifelse(is.null(national), national, national[F_SYSTEM == 1]),
                     labels,
                     title="",
                     barcolor=col_national,
                     topMargin=-0.5,
                     scale=scale,
                     showAxis=!is.null(national))
      
      p34 <- barPlot(ifelse(is.null(national), national, national[F_SYSTEM == 2]),
                     labels,
                     title="",
                     barcolor=col_national,
                     topMargin=-0.5,
                     scale=scale,
                     showAxis=!is.null(national))
      
      obj <- arrangeGrob(p11, p12, p13, p14, textGrob(""),
                         p21, p22, p23, p24, textGrob(""),
                         p31, p32, p33, p34, textGrob(""),
                         ncol=5, nrow=3, 
                         widths=unit(c(rep(3.5/4,4),0.2777667), units="inches") )
      
    }
    return(obj)
  } else {
    # nothing to report because data are missing
    return(textGrob(NoDataString,gp=gpar(fontsize=8, col="Red")))
  }
  
}

