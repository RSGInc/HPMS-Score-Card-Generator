###########################################################################
#  Title: FHWA HPMS Score Card Generator
#   Date: July 2018
# Author: Matt Landis
#
#
# Description:
#
# Function to plot quality on title page. Quality is calculated in 
# calcQuality function.
###########################################################################

plotQuality <- function(score, startx, starty, C, R){

  full_length <- 0.015
  
  grid.rect(x=startx + (C - 1) * 0.15 + 0.01,
            y=starty - (R - 1) * 0.020,
            width=unit(full_length, "npc"),
            height=unit(0.0125, "npc"),
            gp=gpar(fill="white", col="slategray"))
  
  grid.rect(x=startx + (C - 1) * 0.15 + 0.01,
            y=starty - (R - 1) * 0.020,
            width=unit(full_length * (score / 100), "npc"),
            height=unit(0.0125, "npc"),
            gp=gpar(fill="slategray", col="slategray"))
}
