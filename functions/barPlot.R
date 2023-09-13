###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# General bar plot used in the summary pages
#
###########################################################################


barPlot <- function(
  d1,
  labels,
  levels = 1:7,
  title="",
  barcolor,
  topMargin=0,
  leftMargin=0,
  bottomMargin=0,
  rightMargin=0,
  showLabel=FALSE,
  showAxis=FALSE,
  scale
){
  
  # Set a theme adjustment
  theme_adjustment <- theme(
    strip.text.x = element_text(size = 8, angle = 0),
    strip.text.y = element_text(size = 8, angle = 0),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title = element_text(size=6.1,face="bold",colour=gColors$dark,hjust=0),
    legend.position="none",
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = gColors$blank))
  
  # If data is NULL return a blank grob
  
  if(is.null(d1)){
    return(textGrob(""))
  }
  
  # Define the dataset

  dt <- copy(d1)
  dt[, value_numeric := factor(value_numeric, levels=levels)]
  dt[, value_numeric := factor(value_numeric, levels=rev(levels(value_numeric)))]
  
  dt <- dt[, sum(endpoint - beginpoint), by=list(value_numeric)]
  
  dt <- merge(data.table(value_numeric=factor(rev(levels))), 
             dt,
             by="value_numeric", all.x=TRUE)
  
  dt <- cbind(type=barcolor, dt)
  
  dt <- dt[, type := factor(type)]
  
  dt[is.na(V1), type := gColors$blank]
  dt[is.na(V1), V1 := 0]
  
  dt <- dt[, value_numeric := factor(value_numeric, levels=rev(levels(value_numeric)))]
  
  dt[is.na(V1), V1 := 0]
  
  p <- ggplot(dt, aes(x=value_numeric, y=V1, fill=type))
  p <- p + geom_bar(width=0.8, stat="identity",position=position_dodge(0.0))
  p <- p + scale_y_continuous("", limits=c(0, scale))
  p <- p + coord_flip()
  p <- p + ggtitle(title)
  p <- p + scale_fill_manual("", values=c("slategray"=gColors$dark,
                                          "gray75"=gColors$light,
                                          "black"=gColors$text,
                                          "white"=gColors$blank))
  
  p <- p + theme_minimal() + 
    theme_adjustment +
    theme(plot.margin = unit(
            c(topMargin + 0.15, rightMargin + 0.15, bottomMargin + 0.15, leftMargin+0.15), "cm")
    )
  
  if(showAxis) {
    p <- p + theme(axis.text.x=
                     element_text(size=4.5, angle=90, hjust=0.5, colour=gColors$dark))  
  } else {
    p <- p + theme(axis.text.x=element_blank())
  }
  
  if(showLabel) {
    p <- p + theme(axis.text.y=element_text(size=5, hjust = 1,colour=gColors$dark))
  } else {
    p <- p + theme(axis.text.y=element_blank())
  }
  
  return(p)
  
}


