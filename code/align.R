###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2016
# Author: Jeff Dumont
#
#
# Description:
#
# Centers vertically and horizontally graphical objects
#
###########################################################################

align <- function(ob)
{
     w  <- grobWidth(ob)
     h  <- grobHeight(ob)
     
     ob$vp <- viewport(x=0.5*w,y=unit(1,"npc") - 0.5*h)
     
     return(ob)
}