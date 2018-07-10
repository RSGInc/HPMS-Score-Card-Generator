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

plotQuality <- function(score, x, y, text=TRUE){

  full_length <- 0.015
  
  grid.rect(x=x,
            y=y,
            width=unit(full_length, "npc"),
            height=unit(0.0125, "npc"),
            just='left',
            gp=gpar(fill="white", col="slategray"))
  
  grid.rect(x=x,
            y=y,
            width=unit(full_length * (score / 100), "npc"),
            height=unit(0.0125, "npc"),
            just='left',
            gp=gpar(fill="slategray", col="slategray"))
  
  if ( text ){
    grid.text(label = score,
              x= x + full_length + 0.004,
              y=y,
              just = c(0, 0.5),
              gp = gpar(col='slategray', fontsize=6.5))
  }
}
