###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
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
     #grid.text(paste0(gState_Labels[index==state,label]," ",year), x = 0.98, y = 0.96, just = "right", gp = gpar(col = gColors$dark, fontfamily="Garamond",fontface = "bold", fontsize = 24))
     grid.text(paste0(gState_Labels[index==state,label]," ",year), x = 0.98, y = 0.96, just = "right", gp = gpar(col = gColors$dark,fontface = "bold", fontsize = 19))
     #grid.text("HPMS Scorecard", x = 0.98, y = 0.94, just = "right", gp = gpar(col = gColors$blank, fontface = "plain", fontsize = 10))
     #grid.text(title, x = 0.05, y = 0.96, just = "left", gp = gpar(col=gColors$dark, fontfamily="Garamond",fontface = "bold", fontsize = 18))
     grid.text(title, x = 0.05, y = 0.96, just = "left", gp = gpar(col=gColors$dark,fontface = "bold", fontsize = 15))
     #grid.raster(glogo, x = 0.02, y = 0.96, just = "left", width = 0.18)
     
     grid.draw(ellipseGrob(x=0.03,y=0.96,size=5,ar=1,angle=0,def="npc", gp=gpar(fill=gColors$dark,col=gColors$dark)))
     #grid.text(icontext ,0.03,0.96,gp = gpar(col = gColors$blank, fontfamily="Garamond",fontface = "bold", fontsize = 18))
     grid.text(icontext ,0.03,0.96,gp = gpar(col = gColors$blank, fontface = "bold", fontsize = 15))
}