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

     p <- ggplot(result, aes(groupCat, mq, ymin = lq, ymax=uq))
     p <- p + 
          geom_point(data=result,aes(y=min,x=groupCat),colour="black")+
          geom_point(data=result,aes(y=max,x=groupCat),colour="black")+
          geom_crossbar(width = 0.5,fill="black",col="white",fatten = 1) +
          theme_minimal() + coord_flip() +
          scale_y_continuous(labels = percent)+
          theme(
               axis.text.x=element_text(size = 8, hjust = 0,colour="slategray"),
               axis.text.y=element_text(size = 8, angle = 0, hjust = 1,colour="slategray"), 
               strip.text.x = element_text(size = 8, angle = 0,colour="slategray"),
               strip.text.y = element_text(size = 8, angle = 0,colour="slategray"),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               plot.title = element_text(size=15, face="bold",colour = "slategray", hjust=0.5),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "white")
          ) + guides(fill=FALSE)
     return(p)
}