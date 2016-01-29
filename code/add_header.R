###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# Function to the add the text elements to pages as necessary
#
###########################################################################

add_header <- function(state,year,title="title",icontext)
{
     #grid.text(paste0(gState_Labels[index==state,label]," ",year), x = 0.98, y = 0.96, just = "right", gp = gpar(col = "slategray", fontfamily="Garamond",fontface = "bold", fontsize = 24))
     grid.text(paste0(gState_Labels[index==state,label]," ",year), x = 0.98, y = 0.96, just = "right", gp = gpar(col = "slategray",fontface = "bold", fontsize = 24))
     #grid.text("HPMS Scorecard", x = 0.98, y = 0.94, just = "right", gp = gpar(col = "white", fontface = "plain", fontsize = 10))
     #grid.text(title, x = 0.05, y = 0.96, just = "left", gp = gpar(col="slategray", fontfamily="Garamond",fontface = "bold", fontsize = 18))
     grid.text(title, x = 0.05, y = 0.96, just = "left", gp = gpar(col="slategray",fontface = "bold", fontsize = 18))
     #grid.raster(glogo, x = 0.02, y = 0.96, just = "left", width = 0.18)
     
     grid.draw(ellipseGrob(x=0.03,y=0.96,size=15,ar=1,angle=0,def="npc", gp=gpar(fill="slategray",col="slategray")))
     #grid.text(icontext ,0.03,0.96,gp = gpar(col = "white", fontfamily="Garamond",fontface = "bold", fontsize = 18))
     grid.text(icontext ,0.03,0.96,gp = gpar(col = "white", fontface = "bold", fontsize = 18))
}