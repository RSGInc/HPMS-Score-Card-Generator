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

densityPlot <- function(
     d1,
     d2,
     d3=NULL,
     title="",
     minvalue,
     maxvalue,
     densitytype)
{
     
     adjustment <- c(1,1)[densitytype]
     
     if(nrow(d1)>2|nrow(d2)>2) # we have something to report (density plots require at least 3 points to draw)
     {
          
          p <- ggplot(data = d1, aes(x = value_numeric))
          
          if(nrow(d1)>0)
          {
               p <- p + geom_density(data = d1, color="red", linetype="solid", size=0.2,fill="white",adjust=adjustment)

          }
          
          if(nrow(d2)>0)
          {
               p <- p + geom_density(data = d2, color ="blue", linetype="dashed", size=0.2,fill="white",adjust=adjustment)
          }
          
          if(nrow(d3)>0)
          {
               p <- p + geom_density(data = d3, color ="black", linetype="dotted", size=0.2,fill="white",adjust=adjustment)
          }
          
          p <- p + ggtitle(title)
          
          p <- p + xlim(minvalue,maxvalue)
          
          p <- p +     #geom_density(data = var.national,color="black", linetype="twodash", size=0.25) +
               theme_minimal() + 
               #scale_y_continuous(labels = comma)+ 
               #scale_x_continuous(labels = comma) +
               theme(
                    axis.text.x=element_text(size=6, hjust = 0,colour="slategray"),
                    axis.text.y=element_text(size = 0, angle = 0, hjust = 0), 
                    strip.text.x = element_text(size = 8, angle = 0),
                    strip.text.y = element_text(size = 8, angle = 0),
                    axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    plot.title = element_text(size=7, face="bold",colour = "slategray"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "white")
               )
          
          return(p)
     } else
     {
          return(textGrob(""))
     }
     
}