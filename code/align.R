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

align <- function(ob)
{
     w  <- grobWidth(ob)
     h  <- grobHeight(ob)
     
     ob$vp <- viewport(x=0.5*w,y=unit(1,"npc") - 0.5*h)
     
     return(ob)
}