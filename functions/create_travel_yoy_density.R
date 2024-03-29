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
  ramps,
  nvalues_bar = 11
){
  # if(variable %in% c('YEAR_LAST_CONSTRUCTION', 'YEAR_LAST_IMPROVEMENT')) {browser()}
  if ( yearcomparison <= 2020 ){
    expectedChange <- gVariables[Name == variable, Expect_YOY_change_2020]

    # If expectedChange is missing, then don't load yearcomparsion or national data
    include_previous_year = !is.na(expectedChange)
    
    if (!include_previous_year & debugmode) browser()

  } else {
    include_previous_year = TRUE
  }
  
  col_year1 = gColors$dark
  col_year2 = gColors$light
  col_national = gColors$text
  col_noplot = gColors$blank
  
  #if ( variable %in% c('YEAR_LAST_CONSTRUCTION', 'YEAR_LAST_IMPROVEMENT') ) browser()
  
  # What data type?
  type <- gVariables[Name == variable, Data_Type]
  
  
  # Is this a categorical (labeled) variable?
  cont_variable = gVariables[Name == variable, Continuous_variable] == 1
  
  # For non-categorical, should we use a density plot or bar plot?
  density_type = gVariables[Name == variable, Density_Type]
  
  # Which columns to keep from data?
  keep_cols = c('routeid', 'beginpoint', 'endpoint', 'valuenumeric',
                'F_SYSTEM', 'Interstate', 'NHS', 'num_sections')
  
  # get common indices to increase readability
  idx_var1 <- data[, stateid == state &
                     datayear == year &
                     dataitem == variable ] 
  
  idx_var2 <- data[, stateid == state &
                     datayear == yearcomparison &
                     dataitem == variable ]
  
  if ( ramps ){
    
    idx_var1 = idx_var1 & data[, FACILITY_TYPE == 4]
    idx_var2 = idx_var2 & data[, FACILITY_TYPE == 4]
    
  } else {
    
    idx_var1 = idx_var1 & data[, FACILITY_TYPE != 4]
    idx_var2 = idx_var2 & data[, FACILITY_TYPE != 4]
    
  }

  if ( type == 'date' ){ # DATE
    
    # data[(idx_var1 | idx_var2) & (is.na(valuenumeric) | valuenumeric == 0),
    #      valuenumeric := year(valuedate)]
    data[(idx_var1 | idx_var2) & (is.na(valuenumeric) | valuenumeric == 0),
         valuenumeric := ifelse( !is.na(valuedate), year(valuedate), year(begindate) )]
    
  }
  
  idx_var1 = idx_var1 & data[, !is.na(valuenumeric)]
  idx_var2 = idx_var2 & data[, !is.na(valuenumeric)]
  
  var1 <- data[idx_var1, keep_cols, with=FALSE]
  var2 <- data[idx_var2, keep_cols, with=FALSE]
  
  # we have something to report (density plots require at least 3 points to draw)
  if( nrow(var1) > 2 | nrow(var2) > 2 ) {
    
    message('Loading national data for ', variable)
    
    national_file = file.path(
      'data', '+National', yearcomparison, paste0(variable, '.rds'))
    
    if ( file.exists(national_file) ){
      national  <- readRDS(national_file)
    } else {
      national <- NULL
      if ( debugmode ) browser()
    }
    
    if ( type == 'date'){
      national[is.na(valuenumeric) | valuenumeric == 0, 
               valuenumeric := ifelse( !is.na(valuedate), year(valuedate), year(begindate) )]#year(valuedate)]
    }
    
    if(ramps){
      national <- national[FACILITY_TYPE == 4,]
    } else {
      national <- national[FACILITY_TYPE != 4,]
    }
    
    national <- national[!is.na(valuenumeric)]
    national <- national[, keep_cols, with=FALSE]
    
    if(cont_variable){ 
      # make density plots
      
      # What is the maximum number of unique values in a plot?
      if ( density_type == '' | is.na(density_type) ){
        
        
        vals_p1 = sort(
          unique(c(var1[Interstate == 1, valuenumeric],
                   var2[Interstate == 1, valuenumeric],
                   national[Interstate == 1, valuenumeric]))
        )
        
        vals_p2 = sort(
          unique(
            c(var1[NHS == 1, valuenumeric],
              var2[NHS == 1, valuenumeric],
              national[NHS == 1, valuenumeric])
          )
        )
        
        vals_p3 = sort(
          unique(
            c(var1[F_SYSTEM == 1, valuenumeric],
              var2[F_SYSTEM == 1, valuenumeric],
              national[F_SYSTEM == 1, valuenumeric])
          )
        )
        
        vals_p4 = sort(
          unique(
            c(var1[F_SYSTEM == 2, valuenumeric],
              var2[F_SYSTEM == 2, valuenumeric],
              national[F_SYSTEM == 1, valuenumeric])
          )
        )
        
        unique_vals = sort(unique(c(var1$valuenumeric, var2$valuenumeric)))
        nvalues <- max(c(length(vals_p1), length(vals_p2), length(vals_p3), length(vals_p4)))
        
        density_type = ifelse(nvalues <= nvalues_bar, 'bar', 'density')
      }
      
      # Interstate
      if (debugmode) browser()
      
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
          c(var1[, valuenumeric],
            var2[, valuenumeric],
            national[, valuenumeric])
        )
      )
      
      
      # Scale the national data to the state data
      scale <- max(
        c(
          var1[F_SYSTEM == 1, sum(endpoint - beginpoint),
               by=list(valuenumeric)][, max(V1)],
          var1[F_SYSTEM == 2, sum(endpoint - beginpoint),
               by = list(valuenumeric)][, max(V1)],
          var1[Interstate == 1, sum(endpoint - beginpoint),
               by=list(valuenumeric)][, max(V1)],
          var1[NHS == 2, sum(endpoint - beginpoint),
               by = list(valuenumeric)][, max(V1)],
          var2[F_SYSTEM == 1, sum(endpoint - beginpoint),
               by=list(valuenumeric)][, max(V1)],
          var2[F_SYSTEM == 2, sum(endpoint - beginpoint),
               by = list(valuenumeric)][, max(V1)],
          var2[Interstate == 1, sum(endpoint - beginpoint),
               by=list(valuenumeric)][, max(V1)],
          var2[NHS == 2, sum(endpoint - beginpoint),
               by = list(valuenumeric)][, max(V1)]
        ))
      
      natscale <- max(
        c(
          national[F_SYSTEM == 1, sum(endpoint - beginpoint),
               by=list(valuenumeric)][, max(V1)],
          national[F_SYSTEM == 2, sum(endpoint - beginpoint),
               by = list(valuenumeric)][, max(V1)],
          national[Interstate == 1, sum(endpoint - beginpoint),
               by=list(valuenumeric)][, max(V1)],
          national[NHS == 2, sum(endpoint - beginpoint),
               by = list(valuenumeric)][, max(V1)]
        )
      )
      
      natWeight <- scale / natscale

      national[, endpoint := natWeight * endpoint]
      national[, beginpoint := natWeight * beginpoint]
      
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

