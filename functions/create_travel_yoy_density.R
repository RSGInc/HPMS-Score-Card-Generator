###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: RSG, Inc
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
  ramps,
  nvalues_bar = 11
){
  
  col_year1 = gColors$dark
  col_year2 = gColors$light
  col_national = gColors$text
  col_noplot = gColors$blank
  
  #if ( variable %in% c('YEAR_LAST_CONSTRUCTION', 'YEAR_LAST_IMPROVEMENTEMENT') ) browser()
  
  # What data type?
  type <- gVariables[Name == variable, Data_Type]
  
  # Is this a categorical (labeled) variable?
  cont_variable = gVariables[Name == variable, Continuous_variable] == 0
  
  # For non-categorical, should we use a density plot or bar plot?
  density_type = gVariables[Name == variable, Density_Type]
  
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

  if ( type == 'date' ){ # DATE
    
    data[(idx_var1 | idx_var2) & (is.na(value_numeric) | value_numeric == 0),
         value_numeric := year(value_date)]
    
  }
  
  idx_var1 = idx_var1 & data[, !is.na(value_numeric)]
  idx_var2 = idx_var2 & data[, !is.na(value_numeric)]
  
  var1 <- data[idx_var1, keep_cols, with=FALSE]
  var2 <- data[idx_var2, keep_cols, with=FALSE]
  
  # we have something to report (density plots require at least 3 points to draw)
  if( nrow(var1) > 2 | nrow(var2) > 2 ) {
    
    # message('Loading national data for ', variable)
    
    national  <- readRDS(paste0("data\\+National\\", yearcomparison, "\\",
                                variable, ".rds"))
    if ( type == 'date'){
      national[is.na(value_numeric) | value_numeric == 0, 
               value_numeric := year(value_date)]
    }
    
    if(ramps){
      national <- national[FACILITY_TYPE == 4,]
    } else {
      national <- national[FACILITY_TYPE != 4,]
    }
    
    national <- national[!is.na(value_numeric)]
    national <- national[, keep_cols, with=FALSE]
    
    if(cont_variable){ 
      # make density plots
      
      # What is the maximum number of unique values in a plot?
      if ( density_type == '' | is.na(density_type) ){
        
        
        vals_p1 = sort(
          unique(c(var1[Interstate == 1, value_numeric],
                   var2[Interstate == 1, value_numeric],
                   national[Interstate == 1, value_numeric]))
        )
        
        vals_p2 = sort(
          unique(
            c(var1[NHS == 1, value_numeric],
              var2[NHS == 1, value_numeric],
              national[NHS == 1, value_numeric])
          )
        )
        
        vals_p3 = sort(
          unique(
            c(var1[F_SYSTEM == 1, value_numeric],
              var2[F_SYSTEM == 1, value_numeric],
              national[F_SYSTEM == 1, value_numeric])
          )
        )
        
        vals_p4 = sort(
          unique(
            c(var1[F_SYSTEM == 2, value_numeric],
              var2[F_SYSTEM == 2, value_numeric],
              national[F_SYSTEM == 1, value_numeric])
          )
        )
        
        unique_vals = sort(unique(c(var1$value_numeric, var2$value_numeric)))
        nvalues <- max(c(length(vals_p1), length(vals_p2), length(vals_p3), length(vals_p4)))
        
        density_type = ifelse(nvalues <= nvalues_bar, 'bar', 'density')
      }
      
      # Interstate
      p1 <- densityPlot(d1=var1[Interstate==1],
                        d2=var2[Interstate==1],
                        d3=national[Interstate==1],
                        title=gF_SYSTEM_levels[1],
                        year1=year,
                        year2=yearcomparison,
                        showLabel = !is.null(national),
                        density_type = density_type)
      
      # National Highway System
      p2 <- densityPlot(d1=var1[NHS==1],
                        d2=var2[NHS==1],
                        d3=national[NHS==1],
                        title=gF_SYSTEM_levels[2],
                        year1=year,
                        year2=yearcomparison,
                        density_type = density_type)
      
      # Other / Minor Arterials
      p3 <- densityPlot(d1=var1[F_SYSTEM==1],
                        d2=var2[F_SYSTEM==1],
                        d3=national[F_SYSTEM==1],
                        title=gF_SYSTEM_levels[3],
                        year1=year,
                        year2=yearcomparison,
                        showLabel = !is.null(national),
                        density_type = density_type)
      
      # Collectors and Locals
      p4 <- densityPlot(d1=var1[F_SYSTEM==2],
                        d2=var2[F_SYSTEM==2],
                        d3=national[F_SYSTEM==2],
                        title=gF_SYSTEM_levels[4],
                        year1=year,
                        year2=yearcomparison,
                        density_type = density_type)
      
      spacer_height <- 0.07
      fill_rect <- rectGrob(gp = gpar(fill=gColors$blank, col=gColors$blank))
      
      
      obj <- arrangeGrob(
        p1, p2, 
        fill_rect, fill_rect,
        p3, p4,
        ncol=2, nrow=3, 
        widths=unit(rep(3.77776666666667/2,2), units="inches"),
        heights=unit(c((1 - spacer_height)/2, spacer_height, (1-spacer_height)/2), units='npc'))
      
    } else {  # Make bar plots
      
      # get labels for bars
            
      labels <- sort(
        unique(
          c(var1[, value_numeric],
            var2[, value_numeric],
            national[, value_numeric])
        )
      )
      
      
      # Scale the national data to the state data
      scale <- max(
        c(
          var1[F_SYSTEM == 1, sum(end_point - begin_point),
               by=list(value_numeric)][, max(V1)],
          var1[F_SYSTEM == 2, sum(end_point - begin_point),
               by = list(value_numeric)][, max(V1)],
          var1[Interstate == 1, sum(end_point - begin_point),
               by=list(value_numeric)][, max(V1)],
          var1[NHS == 2, sum(end_point - begin_point),
               by = list(value_numeric)][, max(V1)],
          var2[F_SYSTEM == 1, sum(end_point - begin_point),
               by=list(value_numeric)][, max(V1)],
          var2[F_SYSTEM == 2, sum(end_point - begin_point),
               by = list(value_numeric)][, max(V1)],
          var2[Interstate == 1, sum(end_point - begin_point),
               by=list(value_numeric)][, max(V1)],
          var2[NHS == 2, sum(end_point - begin_point),
               by = list(value_numeric)][, max(V1)]
        ))
      
      natscale <- max(
        c(
          national[F_SYSTEM == 1, sum(end_point - begin_point),
               by=list(value_numeric)][, max(V1)],
          national[F_SYSTEM == 2, sum(end_point - begin_point),
               by = list(value_numeric)][, max(V1)],
          national[Interstate == 1, sum(end_point - begin_point),
               by=list(value_numeric)][, max(V1)],
          national[NHS == 2, sum(end_point - begin_point),
               by = list(value_numeric)][, max(V1)]
        )
      )
      
      natWeight <- scale / natscale

      national[, end_point := natWeight * end_point]
      national[, begin_point := natWeight * begin_point]
      
      # Pad scale slightly so we don't have problems with the bar plots
      scale <- scale * 1.01
      
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
      
      if ( is.null(national) ){
        dnat <- national
      } else {
        dnat <- national[Interstate == 1]
      }
      
      p31 <- barPlot(dnat,
                     labels,
                     title="",
                     barcolor=col_national,
                     topMargin=-0.5,
                     scale=scale,
                     showLabel=TRUE,
                     showAxis=TRUE)

      if ( is.null(national) ){
        dnat <- national
      } else {
        dnat <- national[NHS == 1]
      }
      
      p32 <- barPlot(dnat,
                     labels,
                     title="",
                     barcolor=col_national,
                     topMargin=-0.5,
                     scale=scale,
                     showAxis=!is.null(national))
      
      if ( is.null(national) ){
        dnat <- national
      } else {
        dnat <- national[F_SYSTEM == 1]
      }
      
      p33 <- barPlot(dnat,
                     labels,
                     title="",
                     barcolor=col_national,
                     topMargin=-0.5,
                     scale=scale,
                     showAxis=!is.null(national))

      if ( is.null(national) ){
        dnat <- national
      } else {
        dnat <- national[F_SYSTEM == 2]
      }
      
      p34 <- barPlot(dnat, 
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
    return(textGrob(NoDataString,gp=gpar(fontsize=8, col=gColors$highlight)))
  }
  
}

