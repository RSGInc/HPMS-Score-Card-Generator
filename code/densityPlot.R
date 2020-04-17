###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# The basic density plot used on the summary pages
#
###########################################################################

# Get max and min x values, max y across all three datasets

getLimits <- function(dt){
  
  
  # Get max and min x values, max y
  
  if(nrow(dt[!is.na(value_numeric), ]) > 2){
    minvalue <- quantile(dt[, value_numeric], probs=0.05, na.rm=TRUE)
    maxvalue <- quantile(dt[, value_numeric], probs=0.95, na.rm=TRUE)
    if(minvalue == maxvalue){
      maxvalue <- 1 + minvalue
    }
    
    # Get max y by fitting a weighted density
    x <- dt[, value_numeric]
    w <- dt[, (end_point - begin_point) / sum(end_point - begin_point)]
    w <- w[!is.na(x)]
    ymax <- max(density(x, weights=w, na.rm=TRUE)$y)
    
  } else {
    minvalue <- NULL
    maxvalue <- NULL
    ymax     <- NULL
  }
  
  list(minvalue=minvalue, maxvalue=maxvalue, ymax=ymax)
} # getLimits

theme_adjust <- theme(
  axis.text.y=element_blank(),
  strip.text.x = element_text(size = 8, angle = 0),
  strip.text.y = element_text(size = 8, angle = 0),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "white"),
  plot.margin = unit(c(topMargin=0,leftMargin=0,bottomMargin=0,rightMargin=0), "cm")
)

densityPlot <- function(
  d1,         # Current year data.table
  d2,         # Comparison year data.table
  d3=NULL,    # National data.table
  title="",
  year1,
  year2,
  topMargin=0, leftMargin=0, bottomMargin=0, rightMargin=0,
  showLabel=FALSE,
  showXaxis=FALSE,
  plotType = 'density'){
  
  plotfun = get(paste0('geom_', plotType))
  
  col_year1 = 'slategray'
  col_year2 = 'gray75'
  col_national = 'black'
  col_noplot = 'white'
  
  # Get x axis limits and y-axis maximum
  lims1 <- getLimits(d1)
  lims2 <- getLimits(d2)
  lims3 <- getLimits(d3)
  
  minvalue <- min(lims1$minvalue, lims2$minvalue, lims3$minvalue)
  maxvalue <- max(lims1$maxvalue, lims2$maxvalue, lims3$maxvalue)
  # ymax     <- max(lims1$ymax, lims2$ymax, lims3$ymax3) * 1.20
  # 
  # ymax <- max(2.5 * ymax, ymax + 0.05)
  
  if(plotType == 'bar'){
    minvalue <- minvalue - 1
    maxvalue <- maxvalue + 1
    adjustment <- NA  # adjust is not used for geom_bar
    
    if ( all(unique_vals %% 1 == 0) ){
      breaks = minvalue:maxvalue
      width=0.8
      label_cfg = label_number(accuracy=1)
    } else {
      label_cfg = label_number()
      breaks=waiver()
      width=min(diff(unique_vals))
    }
    
  } 
  
  if ( plotType == 'density' ) {
    breaks = waiver()
    width = NA  # width is not used for geom_density
    adjustment <- 1 #c(1,1)[densitytype]
    label_cfg = label_number()
    
  }
  
  if((nrow(d1)>2 | nrow(d2)>2) & !is.null(minvalue)){
    # we have something to report (density plots require at least 3 points to draw)
    
    p1 <- ggplot(data = d1, aes(x = value_numeric,
                                weight=(end_point - begin_point) / sum(end_point - begin_point)))
    p2 <- ggplot(data = d1, aes(x = value_numeric,
                                weight=(end_point - begin_point) / sum(end_point - begin_point)))
    p3 <- ggplot(data = d1, aes(x = value_numeric,
                                weight=(end_point - begin_point) / sum(end_point - begin_point)))
    
    if(nrow(d1)>2){
      p1 <- p1 +
        plotfun(data = d1,
                color=col_year1,
                linetype="solid",
                size=0.25,
                fill=col_year1,
                width=width,
                adjust=adjustment)
      
    } else {
      
      p1 <- p1 + plotfun(data = d3,
                color =col_noplot,
                linetype="solid",
                size=0.25,
                fill=col_noplot,
                width=width,
                adjust=adjustment)    
      
    }
    
    if(nrow(d2)>2) {
      p2 <- p2 + plotfun(data = d2,
                         color =col_year2,
                         linetype="solid",
                         size=0.25,
                         fill=col_year2,
                         width=width,
                         adjust=adjustment)
      
    } else {
      p2 <- p2 + plotfun(data = d3,
                         color =col_noplot,
                         linetype="solid",
                         size=0.25,
                         fill=col_noplot,
                         width=width,
                         adjust=adjustment)  
    }
    
    if(!is.null(d3)) {
      p3 <- p3 + plotfun(data = d3,
                         color =col_national,
                         linetype="solid",
                         size=0.25,
                         fill=col_national,
                         width=width,
                         adjust=adjustment)
    }
    
    p1 <- p1 +   
      ggtitle(title) +
      theme_minimal() + 
      scale_x_continuous(labels = label_cfg,
                         limits=c(minvalue, maxvalue),
                         breaks=breaks) +
      ylab(label = ifelse(showLabel, year1, '')) +
      theme_adjust +
      theme(axis.text.x=element_text(size=4.5, angle=30, hjust = 1,colour=col_noplot),
            plot.title = element_text(size=6.1, face="bold",colour = col_year1, hjust=0.5),
            axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5, colour=col_year1))
    
    p2 <- p2 +     
      ggtitle(title) +
      theme_minimal() + 
      scale_y_continuous()+
      scale_x_continuous(labels = label_cfg,
                         limits=c(minvalue, maxvalue),
                         breaks=breaks) +
      ylab(label = ifelse(showLabel, year2, '')) +
      theme_adjust +
      theme(plot.title = element_text(size=6.1, face="bold",colour = col_noplot, hjust=0.5),
            axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5, colour=col_year2),
            axis.text.x=element_text(size=4.5, angle=30, hjust = 1, colour=col_noplot))
    
    
    p3 <- p3 + 
      ggtitle(title) +
      theme_minimal() + 
      scale_y_continuous()+
      scale_x_continuous(labels = label_cfg,
                         limits=c(minvalue, maxvalue),
                         breaks=breaks)  +
      ylab(label = ifelse(showLabel, "National", '')) +
      theme_adjust +
      theme(axis.text.x=element_text(size=4.5, angle=30, hjust = 1,colour=col_year1),
            plot.title = element_text(size=6.1, face="bold",colour = "white", hjust=0.5),
            axis.title.y=element_text(size=5, face="bold", angle = 90, hjust = 0.5, colour=col_national),
      )
    
    # browser()
    
    # if (nvalues <= 10){
    #   browser()
    # }
    
    p <- arrangeGrob(
      p1, p2, p3, 
      nrow=3, heights=unit(rep(1/3,3), units="npc")
    )
    
    return(p)
  } else {
    return(
      arrangeGrob(
        textGrob(paste0(title,"\ndata are not available or not applicable"),
                 just="top", gp=gpar(fontsize=5, col = "red")),
        heights=unit(1, units="npc"))
    )
  }
  
} # function
