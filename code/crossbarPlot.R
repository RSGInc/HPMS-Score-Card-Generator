###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# Author needs to add a description!
#
###########################################################################

crossbarPlot <- function(result)
{

     p <- ggplot(result, aes(groupCat, mq, ymin = lq, ymax=uq))
     p <- p + 
          geom_point(data=result,aes(y=min,x=groupCat),colour="steelblue4")+
          geom_point(data=result,aes(y=max,x=groupCat),colour="steelblue4")+
          geom_crossbar(width = 0.5,fill="steelblue4",col="white",fatten = 1) +
          theme_minimal() + coord_flip() +
          scale_y_continuous(labels = percent)+
          #scale_y_continuous("")+ 
          #scale_x_continuous(labels = comma) +
          theme(
               axis.text.x=element_text(size = 8, hjust = 0,colour="slategray"),
               axis.text.y=element_text(size = 8, angle = 0, hjust = 1,colour="slategray"), 
               strip.text.x = element_text(size = 8, angle = 0,colour="slategray"),
               strip.text.y = element_text(size = 8, angle = 0,colour="slategray"),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               plot.title = element_text(size=15, face="bold",colour = "slategray"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "white")
          ) + guides(fill=FALSE)
     return(p)
}