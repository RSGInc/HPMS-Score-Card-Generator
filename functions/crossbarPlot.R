###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# This generates the cross bar plot on the traffic: detailed review page
#
###########################################################################

crossbarPlot <- function(result){

  col_point = 'black'
  col_label = 'slategray'
  col_blank = 'white'
  
     p <- ggplot(result, aes(groupCat, mq, ymin = lq, ymax=uq))
     p <- p + 
          geom_point(data=result,aes(y=min,x=groupCat),colour=col_point)+
          geom_point(data=result,aes(y=max,x=groupCat),colour=col_point)+
          geom_crossbar(width = 0.5,fill=col_point,col=col_blank,fatten = 1) +
          theme_minimal() + coord_flip() +
          scale_y_continuous(labels = percent)+
          theme(
               axis.text.x=element_text(size = 8, hjust = 0,colour=col_label),
               axis.text.y=element_text(size = 8, angle = 0, hjust = 1,colour=col_label), 
               strip.text.x = element_text(size = 8, angle = 0,colour=col_label),
               strip.text.y = element_text(size = 8, angle = 0,colour=col_label),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               plot.title = element_text(size=15, face="bold",colour = col_label, hjust=0.5),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = col_blank)
          ) + guides(fill=FALSE)
     return(p)
}