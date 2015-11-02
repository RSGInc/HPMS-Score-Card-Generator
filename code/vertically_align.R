###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2015
# Author: Jeff Dumont
#
#
# Description:
#
# This function vertically aligns table objects within the grid.
#
###########################################################################

vertically_align <- function(ob)
{
     w  <- grobWidth(ob)
     h  <- grobHeight(ob)
     
     ob$vp <- viewport(x=0.5,y=unit(1,"npc") - 0.5*h)
     
     return(ob)
}
